#![allow(dead_code)]

use crate::lexer;

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
    Expr(AstExpr),
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

    pub fn parse(&mut self) -> AstTopLevel {
        let mut result = AstTopLevel {
            decls: HashMap::new(),
        };

        let tok = self.lexer.next_token();
        let tok_kind = tok.kind.clone();

        use lexer::TokenKind as TK;
        match tok_kind {
            TK::Ident(id) => {
                self.expect_token(TK::Colon);
                self.expect_token(TK::Colon);

                self.expect_token(TK::Fn);
                self.expect_token(TK::OpenCurly);

                let mut block: Vec<AstStmt> = Vec::new();

                let tok = self.lexer.next_token();
                let tok_kind = tok.kind.clone();

                match tok_kind {
                    TK::Ident(id) => {
                        self.expect_token(TK::OpenParen);

                        let mut args: Vec<AstExpr> = Vec::new();

                        let tok = self.lexer.next_token();
                        let tok_kind = tok.kind.clone();

                        match tok_kind {
                            TK::StrLiteral(s) => {
                                args.push(AstExpr::Str(s.clone()));
                            }
                            _ => todo!(),
                        }

                        self.expect_token(TK::CloseParen);

                        let fn_call = AstExpr::FnCall {
                            name: id.clone(),
                            args,
                        };

                        self.expect_token(TK::Semicolon);
                        block.push(AstStmt::Expr(fn_call));
                    }
                    _ => todo!(),
                }

                result.decls.insert(
                    id.clone(),
                    AstDecl::Fn(AstFnDecl {
                        name: id.clone(),
                        params: Vec::new(),
                        return_type: AstType::Void,
                        body: Some(block),
                    }),
                );
            }
            _ => todo!(),
        }

        result
    }

    fn expect_token(&mut self, expected_token_kind: lexer::TokenKind) {
        let tok = self.lexer.next_token();
        let kind = tok.kind.clone();
        let loc = tok.loc;

        if kind != expected_token_kind {
            self.lexer.report_error_at(
                loc,
                format!("Expected {:?} but got {:?}.", expected_token_kind, kind).as_str(),
            );
            std::process::exit(1);
        }
    }
}
