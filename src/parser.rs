use crate::{
    ast, lexer,
    token::{TokenKind as TK, ValueKind as VK},
};

use std::{collections::HashMap, path, process};

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

    pub fn parse_top_level(&mut self) -> ast::TopLevel {
        let mut top_level = ast::TopLevel {
            decls: HashMap::new(),
        };

        loop {
            let tok = self.lexer.peek_next_token();
            match tok.kind {
                TK::Ident => {
                    let VK::Ident(id) = tok.val else {
                        unreachable!()
                    };
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

    fn parse_decl(&mut self) -> ast::Decl {
        let VK::Ident(name) = self.lexer.next_token().val else {
            unreachable!()
        };

        self.expect_token(TK::Colon);

        let ty = match self.lexer.peek_next_token().kind {
            TK::Colon | TK::Equals => {
                self.eat_token();
                ast::TypeExpr::Unknown
            }
            _ => self.parse_type_expr(),
        };

        if self.lexer.peek_next_token().kind == TK::Semicolon {
            return ast::Decl::Var(ast::VarDecl {
                name,
                ty,
                value: None,
            });
        }

        let tok = self.lexer.peek_next_token();
        match tok.kind {
            TK::Fn => {
                self.eat_token();

                let mut params: Vec<ast::Param> = vec![];
                if self.lexer.peek_next_token().kind == TK::OpenParen {
                    self.eat_token();
                    loop {
                        match self.lexer.peek_next_token().kind {
                            TK::Ident => {
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
                    ast::TypeExpr::U0
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

                ast::Decl::Fn(ast::FnDecl {
                    name,
                    params,
                    return_type,
                    body,
                })
            }
            _ => ast::Decl::Var(ast::VarDecl {
                name,
                ty,
                value: Some(self.parse_expr()),
            }),
        }
    }

    fn parse_fn_param(&mut self) -> ast::Param {
        let VK::Ident(name) = self.lexer.next_token().val else {
            unreachable!()
        };

        self.expect_token(TK::Colon);

        let ty = match self.lexer.peek_next_token().kind {
            TK::Colon | TK::Equals => {
                self.lexer.next_token();
                ast::TypeExpr::Unknown
            }
            _ => self.parse_type_expr(),
        };

        let value = match self.lexer.peek_next_token().kind {
            TK::Comma | TK::CloseParen => None,
            _ => Some(self.parse_expr()),
        };

        ast::Param { name, ty, value }
    }

    fn parse_block(&mut self) -> ast::Block {
        self.expect_token(TK::OpenCurly);
        let mut block: ast::Block = vec![];

        while self.lexer.peek_next_token().kind != TK::CloseCurly {
            block.push(self.parse_statement());
            if self.lexer.last_token().kind != TK::CloseCurly {
                self.expect_token(TK::Semicolon);
            }
        }
        self.expect_token(TK::CloseCurly);
        block
    }

    fn parse_statement(&mut self) -> ast::Stmt {
        let tok = self.lexer.peek_next_token();

        match tok.kind {
            TK::Ident if self.lexer.peek_token(1).kind == TK::Colon => {
                ast::Stmt::Decl(self.parse_decl())
            }
            TK::OpenCurly => ast::Stmt::Block(self.parse_block()),
            TK::For => {
                self.eat_token();

                let mut init_stmt: Option<ast::Stmt> = None;
                let mut cond_expr: Option<ast::Expr> = None;
                let mut incr_expr: Option<ast::Expr> = None;

                if self.lexer.peek_next_token().kind != TK::OpenCurly {
                    if self.lexer.peek_next_token().kind != TK::Semicolon {
                        init_stmt = Some(self.parse_statement());
                    }
                    self.expect_token(TK::Semicolon);

                    cond_expr = Some(self.parse_expr());
                    self.expect_token(TK::Semicolon);

                    if self.lexer.peek_next_token().kind != TK::OpenCurly {
                        incr_expr = Some(self.parse_expr());
                    }
                }

                let if_block = self.parse_block();

                ast::Stmt::For {
                    init: Box::new(init_stmt),
                    cond: cond_expr,
                    incr: incr_expr,
                    body: if_block,
                }
            }
            TK::If => {
                self.eat_token();

                let cond = self.parse_expr();

                let mut then_block: ast::Block = vec![];
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

                ast::Stmt::If {
                    cond,
                    then_block,
                    else_block,
                }
            }
            TK::Return => {
                self.eat_token();
                ast::Stmt::Return(self.parse_expr())
            }
            _ => ast::Stmt::Expr(self.parse_expr()),
        }
    }

    fn parse_var_decl(&mut self) -> ast::VarDecl {
        let VK::Ident(name) = self.lexer.next_token().val else {
            unreachable!()
        };

        self.expect_token(TK::Colon);

        let ty = match self.lexer.peek_next_token().kind {
            TK::Colon | TK::Equals => {
                self.lexer.next_token();
                ast::TypeExpr::Unknown
            }
            _ => self.parse_type_expr(),
        };

        let value = match self.lexer.peek_next_token().kind {
            TK::Semicolon | TK::OpenCurly => None,
            _ => Some(self.parse_expr()),
        };

        ast::VarDecl { name, ty, value }
    }

    fn parse_expr(&mut self) -> ast::Expr {
        self.parse_expr_bp(0)
    }

    fn parse_expr_bp(&mut self, bp: usize) -> ast::Expr {
        let tok = self.lexer.peek_next_token();

        let mut lhs = match tok.kind {
            TK::Str => {
                let VK::Str(s) = tok.val else { unreachable!() };
                self.eat_token();
                ast::Expr {
                    kind: ast::ExprKind::Str(s),
                    ty: ast::TypeExpr::String,
                }
            }
            TK::Char => {
                let VK::Char(c) = tok.val else { unreachable!() };
                self.eat_token();
                ast::Expr {
                    kind: ast::ExprKind::Char(c),
                    ty: ast::TypeExpr::U8,
                }
            }
            TK::Int => {
                let VK::Int(i) = tok.val else { unreachable!() };
                self.eat_token();
                ast::Expr {
                    kind: ast::ExprKind::Int(i),
                    ty: ast::TypeExpr::Unknown,
                }
            }
            TK::Float => {
                let VK::Float(f) = tok.val else {
                    unreachable!()
                };
                self.eat_token();
                ast::Expr {
                    kind: ast::ExprKind::Float(f),
                    ty: ast::TypeExpr::Unknown,
                }
            }
            TK::Bool => {
                let VK::Bool(b) = tok.val else { unreachable!() };
                self.eat_token();
                ast::Expr {
                    kind: ast::ExprKind::Bool(b),
                    ty: ast::TypeExpr::I0,
                }
            }
            TK::Ident if self.lexer.peek_token(1).kind == TK::OpenParen => self.parse_fn_call(),
            TK::Ident => {
                let VK::Ident(name) = tok.val else {
                    unreachable!()
                };
                self.eat_token();
                ast::Expr {
                    kind: ast::ExprKind::Var { name },
                    ty: ast::TypeExpr::Unknown,
                }
            }
            TK::OpenParen => {
                self.eat_token();
                let expr = self.parse_expr();
                self.expect_token(TK::CloseParen);
                expr
            }
            tk if let Some(op) = tk.to_prefix_unop() => {
                self.eat_token();
                ast::Expr {
                    kind: ast::ExprKind::UnaryOp {
                        op,
                        operand: Box::new(self.parse_expr_bp(30)),
                    },
                    ty: ast::TypeExpr::Unknown,
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
                lhs = ast::Expr {
                    kind: ast::ExprKind::UnaryOp {
                        op: ast::UnaryOpKind::Deref,
                        operand: Box::new(lhs),
                    },
                    ty: ast::TypeExpr::Unknown,
                };
                continue;
            }

            let (lbp, rbp) = match self.lexer.peek_next_token().kind {
                TK::Equals
                | TK::PlusEquals
                | TK::MinusEquals
                | TK::TimesEquals
                | TK::DivEquals
                | TK::ModEquals
                | TK::BitwiseAndEquals
                | TK::BitwiseOrEquals
                | TK::BitwiseNotEquals
                | TK::BitwiseXorEquals
                | TK::BitwiseShlEquals
                | TK::BitwiseShrEquals => (2, 1), // lbp > rbp means it's right associated.
                TK::LogicalOr => (3, 4),
                TK::LogicalAnd => (5, 6),
                TK::IsEqual | TK::IsNotEqual => (7, 8),
                TK::LessThan | TK::LessOrEqual | TK::GreaterThan | TK::GreaterOrEqual => (9, 10),
                TK::BitwiseOr => (11, 12),
                TK::BitwiseXor => (13, 14),
                TK::BitwiseAnd => (15, 16),
                TK::BitwiseShl | TK::BitwiseShr => (17, 18),
                TK::Plus | TK::Minus => (19, 20),
                TK::Star | TK::Div | TK::Mod => (21, 22),
                _ => break,
            };
            if lbp < bp {
                break;
            }

            let tok = self.lexer.next_token();
            let rhs = self.parse_expr_bp(rbp);

            if let Some(op) = tok.kind.to_assop() {
                lhs = ast::Expr {
                    kind: ast::ExprKind::AssignOp {
                        op,
                        lvalue: Box::new(lhs),
                        rvalue: Box::new(rhs),
                    },
                    ty: ast::TypeExpr::Unknown,
                };
                break;
            }

            lhs = ast::Expr {
                kind: ast::ExprKind::BinaryOp {
                    op: tok.kind.to_binop().unwrap(),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                ty: ast::TypeExpr::Unknown,
            };
        }

        lhs
    }

    fn parse_fn_call(&mut self) -> ast::Expr {
        let VK::Ident(name) = self.lexer.next_token().val else {
            unreachable!()
        };

        let mut args: Vec<ast::Expr> = vec![];
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

        ast::Expr {
            kind: ast::ExprKind::FnCall { name, args },
            ty: ast::TypeExpr::Unknown,
        }
    }

    fn parse_type_expr(&mut self) -> ast::TypeExpr {
        let tok = self.lexer.next_token();

        match tok.kind {
            TK::Ident => {
                let VK::Ident(name) = tok.val else {
                    unreachable!()
                };
                match name.as_str() {
                    "u0" | "void" => ast::TypeExpr::U0,
                    "u8" | "char" => ast::TypeExpr::U8,
                    "u16" => ast::TypeExpr::U16,
                    "u32" => ast::TypeExpr::U32,
                    "u64" => ast::TypeExpr::U64,
                    "u128" => ast::TypeExpr::U128,
                    "i0" | "bool" => ast::TypeExpr::I0,
                    "i8" => ast::TypeExpr::I8,
                    "i16" => ast::TypeExpr::I16,
                    "i32" | "int" => ast::TypeExpr::I32,
                    "i64" => ast::TypeExpr::I64,
                    "i128" => ast::TypeExpr::I128,
                    "f32" => ast::TypeExpr::F32,
                    "f64" => ast::TypeExpr::F64,
                    _ => todo!(),
                }
            }
            TK::BitwiseAnd => ast::TypeExpr::Ref(Box::new(self.parse_type_expr())),
            TK::OpenBracket => {
                self.expect_token(TK::CloseBracket);
                ast::TypeExpr::Array(Box::new(self.parse_type_expr()))
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

    fn expect_token(&mut self, expected_token_kind: TK) {
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
