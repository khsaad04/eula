use crate::lexer;

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
    UnaryOp {
        op: UnaryOpKind,
        operand: Box<AstExpr>,
    },
    BinaryOp {
        op: BinaryOpKind,
        lhs: Box<AstExpr>,
        rhs: Box<AstExpr>,
    },
    FnCall {
        id: String,
        args: Vec<AstExpr>,
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
        init: Option<Box<AstStmt>>,
        cond: Option<Box<AstStmt>>,
        incr: Option<Box<AstStmt>>,

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

        use lexer::TokenKind as TK;
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
                            &format!("Redefintion of `{}` at top level.", &id),
                        );
                    };
                }
                TK::Eof => break,
                _ => {
                    self.lexer.report_error_at(
                        tok.loc,
                        &format!("Expected Identifier but got {}", tok.kind),
                    );
                    break;
                }
            }
        }

        top_level
    }

    fn parse_decl(&mut self) -> AstDecl {
        use lexer::TokenKind as TK;

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
                                    &format!("Expected Identifier, `,` or ) but got {}", tok.kind),
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

                let body = if self.lexer.peek_next_token().kind == TK::OpenCurly {
                    Some(self.parse_block())
                } else {
                    None
                };

                AstDecl::Fn(AstFnDecl {
                    name,
                    params,
                    return_type,
                    body,
                })
            }
            TK::Semicolon => {
                self.eat_token();
                AstDecl::Var(AstVarDecl {
                    name,
                    ty,
                    value: None,
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
        use lexer::TokenKind as TK;

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
        let mut block: AstBlock = Vec::new();

        while self.lexer.peek_next_token().kind != lexer::TokenKind::CloseCurly {
            block.push(self.parse_statement());
        }
        self.expect_token(lexer::TokenKind::CloseCurly);
        block
    }

    fn parse_statement(&mut self) -> AstStmt {
        let tok = self.lexer.peek_next_token();

        use lexer::TokenKind as TK;
        match tok.kind {
            TK::Ident(_) if self.lexer.peek_token(1).kind == TK::Colon => {
                AstStmt::Decl(self.parse_decl())
            }
            TK::OpenCurly => AstStmt::Block(self.parse_block()),
            TK::For => {
                self.eat_token();

                let init_statement = self.parse_statement();
                self.expect_token(TK::Semicolon);

                let cond_statement = self.parse_statement();
                self.expect_token(TK::Semicolon);

                let incr_statement = self.parse_statement();

                let if_block = self.parse_block();

                AstStmt::For {
                    init: Some(Box::new(init_statement)),
                    cond: Some(Box::new(cond_statement)),
                    incr: Some(Box::new(incr_statement)),
                    body: if_block,
                }
            }
            TK::If => {
                self.eat_token();
                let cond = self.parse_expr();
                let then_block = self.parse_block();
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
        use lexer::TokenKind as TK;

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
        let tok = self.lexer.next_token();

        self.lexer.report_error_at(tok.loc, "");
        todo!()
    }

    fn parse_type_expr(&mut self) -> AstTypeExpr {
        let tok = self.lexer.next_token();
        use lexer::TokenKind as TK;
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
                    .report_error_at(tok.loc, &format!("Unexpected token {}", tok.kind));
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
                &format!(
                    "Expected {} but got {}",
                    expected_token_kind, next_token.kind
                ),
            );
            process::exit(1);
        }
    }
}
