#![allow(dead_code)]

use crate::lexer;
use lexer::TokenKind as TK;

use std::{collections::HashMap, path};

#[derive(Debug, Clone)]
pub struct AstTopLevel {
    decls: HashMap<String, AstDecl>,
}

#[derive(Debug, Clone)]
pub enum AstDecl {
    Fn(AstFnDecl),
    Var(AstVarDecl),
}

#[derive(Debug, Clone)]
pub struct AstFnDecl {
    name: String,
    params: Vec<AstParam>,
    return_type: AstType,
    body: Option<AstBlock>,
}

#[derive(Debug, Clone)]
pub struct AstVarDecl {
    name: String,
    ty: Option<AstType>,
    value: Option<AstExpr>,
}

type AstParam = AstVarDecl;

#[derive(Debug, Clone)]
pub enum AstType {
    Void,
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    Array(Box<AstType>),
    Ref(Box<AstType>),
}

type AstBlock = Vec<AstStmt>;

#[derive(Debug, Clone)]
pub enum AstExpr {
    Str(String),
    FnCall { name: String, args: Vec<AstExpr> },
}

#[derive(Debug, Clone)]
pub enum AstStmt {
    Decl(AstDecl),
    Assign {
        op: AssignOp,
        lvalue: AstExpr,
        rvalue: AstExpr,
    },
    If {
        cond: AstExpr,
        block: AstBlock,
    },
    For {
        init: Option<Box<AstStmt>>,
        cond: Option<Box<AstStmt>>,
        incr: Option<Box<AstStmt>>,

        block: AstBlock,
    },
    Expr(AstExpr),
}

#[derive(Debug, Clone)]
pub enum AssignOp {
    Eq,  // =
    Add, // +=
}

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
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

        while self.lexer.peek_next_token().kind != lexer::TokenKind::Eof {
            self.parse_decl(&mut top_level);
        }

        top_level
    }

    fn parse_decl(&mut self, top_level: &mut AstTopLevel) {
        let mut tok = self.lexer.next_token();
        let mut tok_kind = tok.kind.clone();

        let decl_id_loc = tok.loc.clone();

        match tok_kind {
            TK::Ident(id) => {
                self.expect_token(TK::Colon);
                self.expect_token(TK::Colon);

                tok = self.lexer.next_token();
                tok_kind = tok.kind.clone();
                let tok_loc = tok.loc.clone();

                let decl: AstDecl = match tok_kind {
                    TK::Fn => {
                        // @Todo: Handle function parameters and return type.
                        let params: Vec<AstParam> = vec![];
                        let return_type = AstType::Void;

                        let mut body: Option<AstBlock> = None;
                        if self.lexer.peek_next_token().kind == TK::OpenCurly {
                            body = Some(self.parse_block());
                        }

                        AstDecl::Fn(AstFnDecl {
                            name: id.clone(),
                            params,
                            return_type,
                            body,
                        })
                    }
                    _ => {
                        self.lexer.report_error_at(tok_loc, "");
                        unimplemented!("Non-function declarations.")
                    }
                };

                if top_level.decls.contains_key(&id) {
                    return self.lexer.report_error_at(
                        decl_id_loc,
                        &format!("Redefintion of `{}` at top level.", &id),
                    );
                }

                top_level.decls.insert(id, decl);
            }
            _ => self
                .lexer
                .report_error_at(decl_id_loc, "Top level only supports declarations."),
        }
    }

    fn parse_block(&mut self) -> AstBlock {
        self.expect_token(TK::OpenCurly);
        let mut block: Vec<AstStmt> = Vec::new();

        while self.lexer.peek_next_token().kind != TK::CloseCurly {
            block.push(self.parse_statement());
        }
        self.expect_token(TK::CloseCurly);
        block
    }

    fn parse_statement(&mut self) -> AstStmt {
        let tok = self.lexer.peek_next_token();
        let tok_kind = tok.kind.clone();
        let tok_loc = tok.loc.clone();

        match tok_kind {
            TK::Ident(_) => {
                let tok = self.lexer.peek_token(1);
                let tok_kind = tok.kind.clone();
                let tok_loc = tok.loc.clone();
                if tok_kind == TK::OpenParen {
                    self.parse_fn_call_statement()
                } else {
                    self.lexer.report_error_at(tok_loc, "");
                    unimplemented!("All kinds of statements.")
                }
            }
            _ => {
                self.lexer.report_error_at(tok_loc, "");
                unimplemented!("All kinds of statements.")
            }
        }
    }

    fn parse_fn_call_statement(&mut self) -> AstStmt {
        let TK::Ident(id) = self.lexer.next_token().kind.clone() else {
            unreachable!()
        };
        self.expect_token(TK::OpenParen);

        let mut args: Vec<AstExpr> = Vec::new();

        loop {
            let tok = self.lexer.next_token();
            let tok_kind = tok.kind.clone();
            match tok_kind {
                TK::StrLiteral(s) => args.push(AstExpr::Str(s)),
                TK::Comma => continue,
                TK::CloseParen => break,
                _ => todo!(),
            }
        }
        self.expect_token(TK::Semicolon);
        AstStmt::Expr(AstExpr::FnCall { name: id, args })
    }

    fn expect_token(&mut self, expected_token_kind: lexer::TokenKind) {
        let next_token = self.lexer.next_token();
        let next_token_kind = next_token.kind.clone();
        let loc = next_token.loc.clone();

        if next_token_kind != expected_token_kind {
            self.lexer.report_error_at(
                loc,
                &format!(
                    "Expected `{:?}` but got `{:?}`.",
                    expected_token_kind, next_token_kind
                ),
            );
            std::process::exit(1);
        }
    }
}
