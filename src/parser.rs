use crate::lexer::{self, TokenKind as TK};

use std::{collections::HashMap, path, process};

#[derive(Debug)]
pub struct AstTopLevel {
    pub decls: HashMap<String, AstDecl>,
}

#[derive(Debug)]
pub enum AstDecl {
    Fn(AstFnDecl),
    Var(AstVarDecl),
}

#[derive(Debug)]
pub struct AstFnDecl {
    pub name: String,
    pub params: Vec<AstParam>,
    pub return_type: AstTypeExpr,
    pub body: Option<AstBlock>, // `None` means it does not have a definition.
}

#[derive(Debug)]
pub struct AstVarDecl {
    pub name: String,
    pub ty: AstTypeExpr,
    pub value: Option<AstExpr>,
}

// @Note: Params may differ in the future.
pub type AstParam = AstVarDecl;

#[derive(Debug)]
pub enum AstExpr {
    Str(String),
    Char(u8),
    Int(u128),
    Float(f64),
    Bool(bool),
    Type(AstTypeExpr),
    Var {
        name: String,
        ty: AstTypeExpr,
    },
    FnCall {
        id: String,
        args: Vec<AstExpr>,
    },
    UnaryOp {
        op: UnaryOpKind,
        operand: Box<AstExpr>,
    },
    BinaryOp {
        op: BinaryOpKind,
        lhs: Box<AstExpr>,
        rhs: Box<AstExpr>,
    },
}

#[derive(Debug)]
pub enum AstTypeExpr {
    U0, // alias: void
    U8, // alias: char
    U16,
    U32,
    U64,
    U128,
    I0, // alias: bool
    I8,
    I16,
    I32, // alias: int
    I64,
    I128,
    F32, // alias: float
    F64,
    Array(Box<AstTypeExpr>),
    Ref(Box<AstTypeExpr>),
    Unknown, // This should get resolved at the typing phase.
}

#[derive(Debug)]
pub enum AstStmt {
    Block(AstBlock),
    Decl(AstDecl),
    Assign {
        op: AssignOpKind,
        lvalue: AstExpr,
        rvalue: AstExpr,
    },
    If {
        cond: AstExpr,
        then_block: AstBlock,
        else_block: Option<AstBlock>,
    },
    For {
        init: Box<Option<AstStmt>>,
        cond: AstExpr,
        incr: Box<Option<AstStmt>>,

        body: AstBlock,
    },
    Return(AstExpr),
    Expr(AstExpr),
}

pub type AstBlock = Vec<AstStmt>;

#[derive(Debug)]
pub enum AssignOpKind {
    Default, // =

    Add, // +=
    Sub, // -=
    Mul, // *=
    Div, // /=
    Mod, // %=

    BitwiseAnd, // &=
    BitwiseOr,  // |=
    BitwiseNot, // ~=
    BitwiseXor, // ^=
    BitwiseShl, // <<=
    BitwiseShr, // >>=
}

#[derive(Debug)]
pub enum BinaryOpKind {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %

    BitwiseAnd, // &
    BitwiseOr,  // |
    BitwiseXor, // ^
    BitwiseShl, // <<
    BitwiseShr, // >>

    LessThan,      // <
    GreaterThan,   // >
    LessEquals,    // <=
    GreaterEquals, // >=
    IsEqual,       // ==
    IsNotEqual,    // !=
    LogicalAnd,    // &&
    LogicalOr,     // ||

    Pipe, // |>
}

#[derive(Debug)]
pub enum UnaryOpKind {
    Plus,       // +
    Minus,      // -
    BitwiseNot, // ~
    LogicalNot, // !
    Ref,        // *.
    Deref,      // .*
}

#[derive(Debug)]
pub struct Parser<'a> {
    pub lexer: lexer::Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, input_path: &'a path::Path) -> Self {
        Self {
            lexer: lexer::Lexer::new(input, input_path),
        }
    }

    pub fn parse_top_level(&mut self) -> AstTopLevel {
        let mut top_level = AstTopLevel {
            decls: HashMap::new(),
        };

        loop {
            let tok = self.lexer.peek_next_token();
            match tok.kind {
                TK::Ident(id) => {
                    if top_level
                        .decls
                        .insert(id.clone(), self.parse_decl())
                        .is_some()
                    {
                        self.lexer.report_error_at(
                            tok.loc,
                            &format!("Redefintion of symbol `{}` at top level.", &id),
                        );
                    };
                }
                TK::Eof => break,
                _ => {
                    self.lexer.report_error_at(
                        tok.loc,
                        &format!("Expected an identifier, found {}", tok),
                    );
                    break;
                }
            }
        }

        top_level
    }

    fn parse_decl(&mut self) -> AstDecl {
        let TK::Ident(name) = self.lexer.next_token().kind else {
            unreachable!()
        };

        self.expect_token(TK::Colon);

        let ty = match self.lexer.peek_next_token().kind {
            TK::Colon | TK::Equals => {
                self.eat_token();
                AstTypeExpr::Unknown
            }
            _ => self.parse_type_expr(),
        };

        if self.lexer.peek_next_token().kind == TK::Semicolon {
            return AstDecl::Var(AstVarDecl {
                name,
                ty,
                value: None,
            });
        }

        let tok = self.lexer.peek_next_token();
        match tok.kind {
            TK::Fn => {
                self.eat_token();

                let mut params: Vec<AstParam> = vec![];
                if self.lexer.peek_next_token().kind == TK::OpenParen {
                    self.eat_token();
                    loop {
                        match self.lexer.peek_next_token().kind {
                            TK::Ident(_) => {
                                params.push(self.parse_fn_param());
                            }
                            TK::Comma => {
                                self.eat_token();
                                continue;
                            }
                            TK::CloseParen => {
                                self.eat_token();
                                break;
                            }
                            _ => {
                                self.lexer.report_error_at(
                                    tok.loc,
                                    &format!(
                                        "Expected one of `,`, `)` or an identifier, found {}",
                                        tok
                                    ),
                                );
                                process::exit(1);
                            }
                        }
                    }
                }

                let return_type = if self.lexer.peek_next_token().kind == TK::RightArrow {
                    self.eat_token();
                    self.parse_type_expr()
                } else {
                    AstTypeExpr::U0
                };

                let tok = self.lexer.peek_next_token();
                let body = if tok.kind == TK::OpenCurly {
                    Some(self.parse_block())
                } else if tok.kind == TK::Semicolon {
                    None
                } else {
                    self.lexer
                        .report_error_at(tok.loc, &format!("Unexpected token {}", tok));
                    process::exit(1);
                };

                AstDecl::Fn(AstFnDecl {
                    name,
                    params,
                    return_type,
                    body,
                })
            }
            _ => AstDecl::Var(AstVarDecl {
                name,
                ty,
                value: Some(self.parse_expr()),
            }),
        }
    }

    fn parse_fn_param(&mut self) -> AstParam {
        let TK::Ident(name) = self.lexer.next_token().kind else {
            unreachable!()
        };

        self.expect_token(TK::Colon);

        let ty = match self.lexer.peek_next_token().kind {
            TK::Colon | TK::Equals => {
                self.lexer.next_token();
                AstTypeExpr::Unknown
            }
            _ => self.parse_type_expr(),
        };

        let value = match self.lexer.peek_next_token().kind {
            TK::Comma | TK::CloseParen => None,
            _ => Some(self.parse_expr()),
        };

        AstParam { name, ty, value }
    }

    fn parse_block(&mut self) -> AstBlock {
        self.expect_token(lexer::TokenKind::OpenCurly);
        let mut block: AstBlock = vec![];

        while self.lexer.peek_next_token().kind != lexer::TokenKind::CloseCurly {
            block.push(self.parse_statement());
            if self.lexer.last_token().kind != lexer::TokenKind::CloseCurly {
                self.expect_token(lexer::TokenKind::Semicolon);
            }
        }
        self.expect_token(lexer::TokenKind::CloseCurly);
        block
    }

    fn parse_statement(&mut self) -> AstStmt {
        let tok = self.lexer.peek_next_token();

        match tok.kind {
            TK::Ident(_) if self.lexer.peek_token(1).kind == TK::Colon => {
                AstStmt::Decl(self.parse_decl())
            }
            TK::OpenCurly => AstStmt::Block(self.parse_block()),
            TK::For => {
                self.eat_token();

                let init_statement = if self.lexer.peek_next_token().kind == TK::Semicolon {
                    None
                } else {
                    Some(self.parse_statement())
                };
                self.expect_token(TK::Semicolon);

                let cond_expr = self.parse_expr();
                self.expect_token(TK::Semicolon);

                let incr_statement = if self.lexer.peek_next_token().kind == TK::OpenCurly {
                    None
                } else {
                    Some(self.parse_statement())
                };

                let if_block = self.parse_block();

                AstStmt::For {
                    init: Box::new(init_statement),
                    cond: cond_expr,
                    incr: Box::new(incr_statement),
                    body: if_block,
                }
            }
            TK::If => {
                self.eat_token();

                let cond = self.parse_expr();

                let mut then_block: AstBlock = vec![];
                if self.lexer.peek_next_token().kind == TK::OpenCurly {
                    then_block = self.parse_block();
                } else {
                    then_block.push(self.parse_statement());
                }

                let else_block = if self.lexer.peek_next_token().kind == TK::Else {
                    self.eat_token();
                    Some(self.parse_block())
                } else {
                    None
                };

                AstStmt::If {
                    cond,
                    then_block,
                    else_block,
                }
            }
            TK::Return => {
                self.eat_token();
                AstStmt::Return(self.parse_expr())
            }
            _ => {
                let expr1 = self.parse_expr();
                let op = match self.lexer.peek_next_token().kind {
                    TK::Equals => AssignOpKind::Default,

                    TK::AddEquals => AssignOpKind::Add,
                    TK::SubEquals => AssignOpKind::Sub,
                    TK::TimesEquals => AssignOpKind::Mul,
                    TK::DivEquals => AssignOpKind::Div,
                    TK::ModEquals => AssignOpKind::Mod,

                    TK::BitwiseAndEquals => AssignOpKind::BitwiseAnd,
                    TK::BitwiseOrEquals => AssignOpKind::BitwiseOr,
                    TK::BitwiseXorEquals => AssignOpKind::BitwiseNot,
                    TK::BitwiseNotEquals => AssignOpKind::BitwiseXor,
                    TK::BitwiseShlEquals => AssignOpKind::BitwiseShl,
                    TK::BitwiseShrEquals => AssignOpKind::BitwiseShr,
                    _ => return AstStmt::Expr(expr1),
                };
                self.eat_token();
                let expr2 = self.parse_expr();
                AstStmt::Assign {
                    op,
                    lvalue: expr1,
                    rvalue: expr2,
                }
            }
        }
    }

    fn parse_var_decl(&mut self) -> AstVarDecl {
        let TK::Ident(name) = self.lexer.next_token().kind else {
            unreachable!()
        };

        self.expect_token(TK::Colon);

        let ty = match self.lexer.peek_next_token().kind {
            TK::Colon | TK::Equals => {
                self.lexer.next_token();
                AstTypeExpr::Unknown
            }
            _ => self.parse_type_expr(),
        };

        let value = match self.lexer.peek_next_token().kind {
            TK::Semicolon | TK::OpenCurly => None,
            _ => Some(self.parse_expr()),
        };

        AstVarDecl { name, ty, value }
    }

    fn parse_expr(&mut self) -> AstExpr {
        self.parse_expr_bp(0)
    }

    fn parse_expr_bp(&mut self, bp: usize) -> AstExpr {
        let tok = self.lexer.peek_next_token();

        let mut lhs = match tok.kind {
            TK::StrLit(s) => {
                self.eat_token();
                AstExpr::Str(s)
            }
            TK::CharLit(c) => {
                self.eat_token();
                AstExpr::Char(c)
            }
            TK::IntLit(i) => {
                self.eat_token();
                AstExpr::Int(i)
            }
            TK::FloatLit(f) => {
                self.eat_token();
                AstExpr::Float(f)
            }
            TK::BoolLit(b) => {
                self.eat_token();
                AstExpr::Bool(b)
            }
            TK::Ident(_) if self.lexer.peek_token(1).kind == TK::OpenParen => self.parse_fn_call(),
            TK::Ident(id) => {
                self.eat_token();
                AstExpr::Var {
                    name: id,
                    ty: AstTypeExpr::Unknown,
                }
            }
            TK::OpenParen => {
                self.eat_token();
                let expr = self.parse_expr();
                self.expect_token(TK::CloseParen);
                expr
            }
            TK::Plus => {
                self.eat_token();
                AstExpr::UnaryOp {
                    op: UnaryOpKind::Plus,
                    operand: Box::new(self.parse_expr_bp(30)),
                }
            }
            TK::Minus => {
                self.eat_token();
                AstExpr::UnaryOp {
                    op: UnaryOpKind::Minus,
                    operand: Box::new(self.parse_expr_bp(30)),
                }
            }
            TK::BitwiseNot => {
                self.eat_token();
                AstExpr::UnaryOp {
                    op: UnaryOpKind::BitwiseNot,
                    operand: Box::new(self.parse_expr_bp(30)),
                }
            }
            TK::LogicalNot => {
                self.eat_token();
                AstExpr::UnaryOp {
                    op: UnaryOpKind::LogicalNot,
                    operand: Box::new(self.parse_expr_bp(30)),
                }
            }
            TK::Ref => {
                self.eat_token();
                AstExpr::UnaryOp {
                    op: UnaryOpKind::Ref,
                    operand: Box::new(self.parse_expr_bp(30)),
                }
            }
            _ => {
                self.lexer
                    .report_error_at(tok.loc, &format!("Unexpected token {}", tok));
                process::exit(1);
            }
        };

        loop {
            if self.lexer.peek_next_token().kind == TK::Deref {
                self.eat_token();
                lhs = AstExpr::UnaryOp {
                    op: UnaryOpKind::Deref,
                    operand: Box::new(lhs),
                };
                continue;
            }

            let (lbp, rbp) = match self.lexer.peek_next_token().kind {
                TK::LogicalOr => (7, 8),
                TK::LogicalAnd => (9, 10),
                TK::IsEqual | TK::IsNotEqual => (11, 12),
                TK::LessThan | TK::LessEquals | TK::GreaterThan | TK::GreaterEquals => (13, 14),
                TK::BitwiseOr => (15, 16),
                TK::BitwiseXor => (17, 18),
                TK::BitwiseAnd => (19, 20),
                TK::BitwiseShl | TK::BitwiseShr => (21, 22),
                TK::Plus | TK::Minus => (23, 24),
                TK::Star | TK::Div | TK::Mod => (25, 26),
                _ => break,
            };
            if lbp < bp {
                break;
            }

            let tok = self.lexer.next_token();
            let op = match tok.kind {
                TK::Plus => BinaryOpKind::Add,
                TK::Minus => BinaryOpKind::Sub,
                TK::Star => BinaryOpKind::Mul,
                TK::Div => BinaryOpKind::Div,
                TK::Mod => BinaryOpKind::Mod,
                TK::BitwiseAnd => BinaryOpKind::BitwiseAnd,
                TK::BitwiseOr => BinaryOpKind::BitwiseOr,
                TK::BitwiseXor => BinaryOpKind::BitwiseXor,
                TK::BitwiseShl => BinaryOpKind::BitwiseShl,
                TK::BitwiseShr => BinaryOpKind::BitwiseShr,
                TK::LessThan => BinaryOpKind::LessThan,
                TK::GreaterThan => BinaryOpKind::GreaterThan,
                TK::LessEquals => BinaryOpKind::LessEquals,
                TK::GreaterEquals => BinaryOpKind::GreaterEquals,
                TK::IsEqual => BinaryOpKind::IsEqual,
                TK::IsNotEqual => BinaryOpKind::IsNotEqual,
                TK::LogicalAnd => BinaryOpKind::LogicalAnd,
                TK::LogicalOr => BinaryOpKind::LogicalOr,
                _ => unreachable!(),
            };
            let rhs = self.parse_expr_bp(rbp);

            lhs = AstExpr::BinaryOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }

        lhs
    }

    fn parse_fn_call(&mut self) -> AstExpr {
        let TK::Ident(id) = self.lexer.next_token().kind else {
            unreachable!()
        };

        let mut args: Vec<AstExpr> = vec![];
        self.expect_token(TK::OpenParen);
        loop {
            match self.lexer.peek_next_token().kind {
                TK::Comma => {
                    self.eat_token();
                    continue;
                }
                TK::CloseParen => {
                    self.eat_token();
                    break;
                }
                _ => args.push(self.parse_expr()),
            }
        }

        AstExpr::FnCall { id, args }
    }

    fn parse_type_expr(&mut self) -> AstTypeExpr {
        let tok = self.lexer.next_token();

        match tok.kind {
            TK::U0 => AstTypeExpr::U0,
            TK::U8 => AstTypeExpr::U8,
            TK::U16 => AstTypeExpr::U16,
            TK::U32 => AstTypeExpr::U32,
            TK::U64 => AstTypeExpr::U64,
            TK::U128 => AstTypeExpr::U128,
            TK::I0 => AstTypeExpr::I0,
            TK::I8 => AstTypeExpr::I8,
            TK::I16 => AstTypeExpr::I16,
            TK::I32 => AstTypeExpr::I32,
            TK::I64 => AstTypeExpr::I64,
            TK::I128 => AstTypeExpr::I128,
            TK::F32 => AstTypeExpr::F32,
            TK::F64 => AstTypeExpr::F64,
            TK::BitwiseAnd => AstTypeExpr::Ref(Box::new(self.parse_type_expr())),
            TK::OpenBracket => {
                self.expect_token(TK::CloseBracket);
                AstTypeExpr::Array(Box::new(self.parse_type_expr()))
            }
            _ => {
                self.lexer
                    .report_error_at(tok.loc, &format!("Unexpected token {}", tok));
                process::exit(1);
            }
        }
    }

    fn eat_token(&mut self) {
        self.lexer.next_token();
    }

    fn expect_token(&mut self, expected_token_kind: lexer::TokenKind) {
        let next_token = self.lexer.next_token();

        if next_token.kind != expected_token_kind {
            self.lexer.report_error_at(
                next_token.loc,
                &format!("Expected `{}`, found {}", expected_token_kind, next_token),
            );
            process::exit(1);
        }
    }
}
