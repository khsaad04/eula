use crate::{
    ast,
    lexer::{self, TokenKind as TK, ValueKind as VK},
};

use std::{collections::HashMap, path, process};

#[derive(Debug)]
pub struct Parser<'a> {
    pub lexer: lexer::Lexer<'a>,
    pub tokens_processed: usize,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, input_path: &'a path::Path) -> Self {
        Self {
            lexer: lexer::Lexer::new(input, input_path),
            tokens_processed: 0,
        }
    }

    pub fn parse_top_level(&mut self) -> ast::TopLevel {
        let mut top_level = ast::TopLevel {
            decls: HashMap::new(),
        };

        loop {
            let tok = self.peek_next_token();
            match tok.kind {
                TK::Ident => {
                    let VK::Ident(id) = tok.value else {
                        unreachable!()
                    };
                    if top_level
                        .decls
                        .insert(id.clone(), self.parse_decl())
                        .is_some()
                    {
                        self.lexer.report_error_at(
                            tok.span,
                            &format!("Redefintion of symbol `{}` at top level.", &id),
                        );
                    };
                }
                TK::Eof => break,
                _ => {
                    self.lexer.report_error_at(
                        tok.span,
                        &format!("Expected an identifier, found {}", tok),
                    );
                    break;
                }
            }
        }

        top_level
    }

    fn parse_decl(&mut self) -> ast::Decl {
        let pos0 = self.lexer.get_pos();
        let VK::Ident(name) = self.next_token().value else {
            unreachable!()
        };

        let maybe_colon_span = self.peek_next_token().span;
        self.expect_token(TK::Colon);

        let mut ty = match self.peek_next_token().kind {
            TK::Colon | TK::Equals => {
                self.eat_token();
                ast::TypeDef {
                    kind: ast::TypeKind::Unknown,
                    span: maybe_colon_span,
                }
            }
            _ => self.parse_type_expr(),
        };

        if self.peek_next_token().kind == TK::Semicolon {
            let span = self.lexer.get_span(pos0);
            return ast::Decl {
                kind: ast::DeclKind::Var(ast::VarDecl {
                    name,
                    ty,
                    value: None,
                    span,
                }),
                span,
            };
        }

        let tok = self.peek_next_token();
        match tok.kind {
            TK::Fn => {
                self.eat_token();

                let mut params: Vec<ast::Param> = vec![];
                if self.peek_next_token().kind == TK::OpenParen {
                    self.eat_token();
                    loop {
                        match self.peek_next_token().kind {
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
                                    tok.span,
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

                let last_tok_span = self.peek_next_token().span;
                let return_type = if self.peek_next_token().kind == TK::RightArrow {
                    self.eat_token();
                    self.parse_type_expr()
                } else {
                    ast::TypeDef {
                        kind: ast::TypeKind::U0,
                        span: last_tok_span,
                    }
                };

                let tok = self.peek_next_token();
                let body = if tok.kind == TK::OpenCurly {
                    Some(self.parse_block())
                } else if tok.kind == TK::Semicolon {
                    None
                } else {
                    self.lexer
                        .report_error_at(tok.span, &format!("Unexpected token {}", tok));
                    process::exit(1);
                };

                let span = self.lexer.get_span(pos0);
                ast::Decl {
                    kind: ast::DeclKind::Fn(ast::FnDecl {
                        name,
                        params,
                        return_type,
                        body,
                        span,
                    }),
                    span,
                }
            }
            _ => {
                let value = self.parse_expr();
                let span = self.lexer.get_span(pos0);
                if let ast::TypeKind::Unknown = ty.kind {
                    ty.kind = value.ty.clone();
                }
                ast::Decl {
                    kind: ast::DeclKind::Var(ast::VarDecl {
                        name,
                        ty,
                        value: Some(value),
                        span,
                    }),
                    span,
                }
            }
        }
    }

    fn parse_fn_param(&mut self) -> ast::Param {
        let tok = self.next_token();
        let pos0 = tok.span.0;

        let VK::Ident(name) = tok.value else {
            unreachable!()
        };

        let maybe_colon_span = self.peek_next_token().span;
        self.expect_token(TK::Colon);

        let ty = match self.peek_next_token().kind {
            TK::Colon | TK::Equals => {
                self.eat_token();
                ast::TypeDef {
                    kind: ast::TypeKind::Unknown,
                    span: maybe_colon_span,
                }
            }
            _ => self.parse_type_expr(),
        };

        let value = match self.peek_next_token().kind {
            TK::Comma | TK::CloseParen => None,
            _ => Some(self.parse_expr()),
        };

        let span = self.lexer.get_span(pos0);
        ast::Param {
            name,
            ty,
            value,
            span,
        }
    }

    fn parse_block(&mut self) -> ast::Block {
        let pos0 = self.lexer.peek_next_token().span.0;
        self.expect_token(TK::OpenCurly);

        let mut stmts: Vec<ast::Stmt> = vec![];

        while self.peek_next_token().kind != TK::CloseCurly {
            stmts.push(self.parse_statement());
            if let Some(tok) = self.lexer.last_token()
                && tok.kind != TK::CloseCurly
            {
                self.expect_token(TK::Semicolon);
            }
        }

        self.expect_token(TK::CloseCurly);
        let span = self.lexer.get_span(pos0);
        ast::Block { stmts, span }
    }

    fn parse_statement(&mut self) -> ast::Stmt {
        let tok = self.peek_next_token();
        let pos0 = tok.span.0;

        let kind = match tok.kind {
            TK::Ident if self.lexer.peek_token(1).kind == TK::Colon => {
                ast::StmtKind::Decl(self.parse_decl())
            }
            TK::OpenCurly => ast::StmtKind::Block(self.parse_block()),
            TK::For => {
                self.eat_token();

                let mut init_stmt: Option<ast::Stmt> = None;
                let mut cond_expr: Option<ast::Expr> = None;
                let mut incr_expr: Option<ast::Expr> = None;

                if self.peek_next_token().kind != TK::OpenCurly {
                    if self.peek_next_token().kind != TK::Semicolon {
                        init_stmt = Some(self.parse_statement());
                    }
                    self.expect_token(TK::Semicolon);

                    cond_expr = Some(self.parse_expr());
                    self.expect_token(TK::Semicolon);

                    if self.peek_next_token().kind != TK::OpenCurly {
                        incr_expr = Some(self.parse_expr());
                    }
                }

                let if_block = self.parse_block();

                ast::StmtKind::For {
                    init: Box::new(init_stmt),
                    cond: cond_expr,
                    incr: incr_expr,
                    body: if_block,
                }
            }
            TK::If => {
                self.eat_token();

                let cond = self.parse_expr();

                let last_tok_span = self.peek_next_token().span;
                let mut then_block = ast::Block {
                    stmts: vec![],
                    span: last_tok_span,
                };
                if self.peek_next_token().kind == TK::OpenCurly {
                    then_block = self.parse_block();
                } else {
                    then_block.stmts.push(self.parse_statement());
                    then_block.span.1 = self.lexer.get_pos();
                }

                let else_block = if self.peek_next_token().kind == TK::Else {
                    self.eat_token();
                    Some(self.parse_block())
                } else {
                    None
                };

                ast::StmtKind::If {
                    cond,
                    then_block,
                    else_block,
                }
            }
            TK::Return => {
                self.eat_token();
                ast::StmtKind::Return(self.parse_expr())
            }
            _ => ast::StmtKind::Expr(self.parse_expr()),
        };

        let span = self.lexer.get_span(pos0);
        ast::Stmt { kind, span }
    }

    fn parse_var_decl(&mut self) -> ast::VarDecl {
        let tok = self.next_token();
        let pos0 = tok.span.0;

        let VK::Ident(name) = tok.value else {
            unreachable!()
        };

        let maybe_colon_span = self.peek_next_token().span;
        self.expect_token(TK::Colon);

        let ty = match self.peek_next_token().kind {
            TK::Colon | TK::Equals => {
                self.eat_token();
                ast::TypeDef {
                    kind: ast::TypeKind::Unknown,
                    span: maybe_colon_span,
                }
            }
            _ => self.parse_type_expr(),
        };

        let value = match self.peek_next_token().kind {
            TK::Semicolon | TK::OpenCurly => None,
            _ => Some(self.parse_expr()),
        };

        let span = self.lexer.get_span(pos0);
        ast::VarDecl {
            name,
            ty,
            value,
            span,
        }
    }

    fn parse_expr(&mut self) -> ast::Expr {
        let mut expr = self.parse_expr_bp(0);
        match expr.kind {
            ast::ExprKind::Int(_) => expr.ty = ast::TypeKind::I32,
            ast::ExprKind::Float(_) => expr.ty = ast::TypeKind::F32,
            _ => {}
        }
        expr
    }

    fn parse_expr_bp(&mut self, bp: usize) -> ast::Expr {
        let tok = self.peek_next_token();
        let pos0 = tok.span.0;

        let mut lhs = match tok.kind {
            TK::Str => {
                let VK::Str(s) = tok.value else {
                    unreachable!()
                };
                self.eat_token();
                ast::Expr {
                    kind: ast::ExprKind::Str(s),
                    ty: ast::TypeKind::String,
                    span: self.lexer.get_span(pos0),
                }
            }
            TK::Char => {
                let VK::Char(c) = tok.value else {
                    unreachable!()
                };
                self.eat_token();
                ast::Expr {
                    kind: ast::ExprKind::Char(c),
                    ty: ast::TypeKind::U8,
                    span: self.lexer.get_span(pos0),
                }
            }
            TK::Int => {
                let VK::Int(i) = tok.value else {
                    unreachable!()
                };
                self.eat_token();
                ast::Expr {
                    kind: ast::ExprKind::Int(i),
                    ty: ast::TypeKind::Unknown,
                    span: self.lexer.get_span(pos0),
                }
            }
            TK::Float => {
                let VK::Float(f) = tok.value else {
                    unreachable!()
                };
                self.eat_token();
                ast::Expr {
                    kind: ast::ExprKind::Float(f),
                    ty: ast::TypeKind::Unknown,
                    span: self.lexer.get_span(pos0),
                }
            }
            TK::Bool => {
                let VK::Bool(b) = tok.value else {
                    unreachable!()
                };
                self.eat_token();
                ast::Expr {
                    kind: ast::ExprKind::Bool(b),
                    ty: ast::TypeKind::I0,
                    span: self.lexer.get_span(pos0),
                }
            }
            TK::Ident if self.lexer.peek_token(1).kind == TK::OpenParen => self.parse_fn_call(),
            TK::Ident => {
                let VK::Ident(name) = tok.value else {
                    unreachable!()
                };
                self.eat_token();
                ast::Expr {
                    kind: ast::ExprKind::Var { name },
                    ty: ast::TypeKind::Unknown,
                    span: self.lexer.get_span(pos0),
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
                let expr = self.parse_expr_bp(30);
                ast::Expr {
                    kind: ast::ExprKind::UnaryOp {
                        op,
                        operand: Box::new(expr),
                    },
                    ty: ast::TypeKind::Unknown,
                    span: self.lexer.get_span(pos0),
                }
            }
            _ => {
                self.lexer
                    .report_error_at(tok.span, &format!("Unexpected token {}", tok));
                process::exit(1);
            }
        };

        loop {
            let tok = self.peek_next_token();
            let pos0 = tok.span.0;

            if tok.kind == TK::Deref {
                self.eat_token();
                lhs = ast::Expr {
                    kind: ast::ExprKind::UnaryOp {
                        op: ast::UnaryOpKind::Deref,
                        operand: Box::new(lhs),
                    },
                    ty: ast::TypeKind::Unknown,
                    span: self.lexer.get_span(pos0),
                };
                continue;
            }

            let (lbp, rbp) = match self.peek_next_token().kind {
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

            let tok = self.next_token();
            let rhs = self.parse_expr_bp(rbp);

            if let Some(op) = tok.kind.to_assop() {
                lhs = ast::Expr {
                    kind: ast::ExprKind::AssignOp {
                        op,
                        lvalue: Box::new(lhs),
                        rvalue: Box::new(rhs),
                    },
                    ty: ast::TypeKind::Unknown,
                    span: self.lexer.get_span(pos0),
                };
                break;
            }

            lhs = ast::Expr {
                kind: ast::ExprKind::BinaryOp {
                    op: tok.kind.to_binop().unwrap(),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                ty: ast::TypeKind::Unknown,
                span: self.lexer.get_span(pos0),
            };
        }

        lhs
    }

    fn parse_fn_call(&mut self) -> ast::Expr {
        let tok = self.next_token();
        let pos0 = tok.span.0;

        let VK::Ident(name) = tok.value else {
            unreachable!()
        };

        let mut args: Vec<ast::Expr> = vec![];
        self.expect_token(TK::OpenParen);
        loop {
            match self.peek_next_token().kind {
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
            ty: ast::TypeKind::Unknown,
            span: self.lexer.get_span(pos0),
        }
    }

    fn parse_type_expr(&mut self) -> ast::TypeDef {
        let tok = self.next_token();
        let pos0 = tok.span.0;

        let kind = match tok.kind {
            TK::Ident => {
                let VK::Ident(name) = tok.value else {
                    unreachable!()
                };
                match name.as_str() {
                    "u0" | "void" => ast::TypeKind::U0,
                    "u8" | "char" => ast::TypeKind::U8,
                    "u16" => ast::TypeKind::U16,
                    "u32" => ast::TypeKind::U32,
                    "u64" => ast::TypeKind::U64,
                    "i0" | "bool" => ast::TypeKind::I0,
                    "i8" => ast::TypeKind::I8,
                    "i16" => ast::TypeKind::I16,
                    "i32" | "int" => ast::TypeKind::I32,
                    "i64" => ast::TypeKind::I64,
                    "f32" => ast::TypeKind::F32,
                    "f64" => ast::TypeKind::F64,
                    _ => todo!(),
                }
            }
            TK::BitwiseAnd => ast::TypeKind::Ref(Box::new(self.parse_type_expr())),
            TK::OpenBracket => {
                self.expect_token(TK::CloseBracket);
                ast::TypeKind::Array(Box::new(self.parse_type_expr()))
            }
            _ => {
                self.lexer
                    .report_error_at(tok.span, &format!("Unexpected token {}", tok));
                process::exit(1);
            }
        };

        ast::TypeDef {
            kind,
            span: self.lexer.get_span(pos0),
        }
    }

    fn next_token(&mut self) -> lexer::Token<'a> {
        self.tokens_processed += 1;
        self.lexer.next_token()
    }

    fn peek_next_token(&mut self) -> lexer::Token<'a> {
        self.lexer.peek_next_token()
    }

    fn eat_token(&mut self) {
        self.next_token();
    }

    fn expect_token(&mut self, expected_token_kind: TK) {
        let next_token = self.next_token();

        if next_token.kind != expected_token_kind {
            self.lexer.report_error_at(
                next_token.span,
                &format!("Expected `{}`, found {}", expected_token_kind, next_token),
            );
            process::exit(1);
        }
    }
}
