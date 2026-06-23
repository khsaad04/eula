use crate::{ast, lexer};

use std::{
    fmt::{self, Write},
    fs, io,
};

#[derive(Debug)]
pub struct Codegen<'a> {
    ast: ast::TopLevel,
    lexer: lexer::Lexer<'a>,
    string_buffer: String,
}

impl<'a> Codegen<'a> {
    pub fn new(ast: ast::TopLevel, lexer: lexer::Lexer<'a>) -> Self {
        Self {
            ast,
            lexer,
            string_buffer: String::new(),
        }
    }

    pub fn write_entire_buffer_to_file(self, output_path: &str) -> io::Result<()> {
        fs::write(output_path, self.string_buffer)
    }

    pub fn generate(&mut self) -> fmt::Result {
        // Includes
        writeln!(self.string_buffer, "// includes")?;
        writeln!(self.string_buffer, "#include <stdio.h>")?;
        writeln!(self.string_buffer, "#include <stdint.h>")?;
        writeln!(self.string_buffer)?;

        // Declarations
        writeln!(self.string_buffer, "// declarations")?;
        for decl in self.ast.decls.values() {
            match &decl.kind {
                ast::DeclKind::Fn(decl) => {
                    write!(
                        self.string_buffer,
                        "{} {}(",
                        decl.return_type.to_c(),
                        decl.name
                    )?;
                    for (i, param) in decl.params.iter().enumerate() {
                        write!(self.string_buffer, "{}", &param.ty.to_c())?;
                        if i < decl.params.len() - 1 {
                            write!(self.string_buffer, ", ")?;
                        }
                    }
                    writeln!(self.string_buffer, ");")?;
                }
                ast::DeclKind::Var(decl) => {
                    writeln!(self.string_buffer, "{} {}", decl.ty.to_c(), decl.name)?;
                }
            }
        }
        writeln!(self.string_buffer)?;

        // Definitions
        writeln!(self.string_buffer, "// definitions")?;
        for decl in self.ast.decls.values() {
            match &decl.kind {
                ast::DeclKind::Fn(decl) => {
                    if let Some(body) = &decl.body {
                        write!(
                            self.string_buffer,
                            "{} {}(",
                            decl.return_type.to_c(),
                            decl.name
                        )?;
                        for (i, param) in decl.params.iter().enumerate() {
                            write!(self.string_buffer, "{} {}", param.ty.to_c(), param.name)?;
                            if i < decl.params.len() - 1 {
                                write!(self.string_buffer, ", ")?;
                            }
                        }
                        writeln!(self.string_buffer, ")")?;
                        // Function body
                        writeln!(self.string_buffer, "{}", body.to_c())?;
                    }
                }
                ast::DeclKind::Var(decl) => {
                    self.lexer
                        .report_error_at(decl.span, "Not implemented yet.");
                    unimplemented!("Variables at top level.")
                }
            }
        }
        writeln!(self.string_buffer)?;
        Ok(())
    }
}

impl ast::Block {
    pub fn to_c(&self) -> String {
        let mut buf = String::new();
        writeln!(buf, "{{").unwrap();
        for stmt in &self.stmts {
            writeln!(buf, "{};", stmt.to_c()).unwrap();
        }
        write!(buf, "}}").unwrap();
        buf
    }
}

impl ast::Decl {
    pub fn to_c(&self) -> String {
        let mut buf = String::new();
        match &self.kind {
            ast::DeclKind::Fn(_) => todo!(),
            ast::DeclKind::Var(var_decl) => {
                write!(buf, "{} {}", var_decl.ty.to_c(), var_decl.name).unwrap();
                if let Some(value) = &var_decl.value {
                    write!(buf, " = {}", value.to_c()).unwrap();
                }
                buf
            }
        }
    }
}

impl ast::Stmt {
    pub fn to_c(&self) -> String {
        let mut buf = String::new();
        match &self.kind {
            ast::StmtKind::Block(block) => block.to_c(),
            ast::StmtKind::Decl(decl) => decl.to_c(),
            ast::StmtKind::If {
                cond,
                then_stmt,
                else_stmt,
            } => {
                write!(buf, "if ({}) {}", cond.to_c(), then_stmt.to_c()).unwrap();
                if let Some(block) = else_stmt {
                    if let ast::StmtKind::Block(_) = then_stmt.kind {
                        // Don't write a semicolon if it's a block.
                    } else {
                        write!(buf, ";").unwrap()
                    }
                    write!(buf, " else {}", block.to_c()).unwrap();
                }
                buf
            }
            ast::StmtKind::For {
                init,
                cond,
                incr,
                body,
            } => {
                writeln!(
                    buf,
                    "for ({}; {}; {})",
                    if let Some(stmt) = &**init {
                        stmt.to_c()
                    } else {
                        "".to_string()
                    },
                    if let Some(expr) = cond {
                        expr.to_c()
                    } else {
                        "".to_string()
                    },
                    if let Some(expr) = incr {
                        expr.to_c()
                    } else {
                        "".to_string()
                    }
                )
                .unwrap();
                write!(buf, "{}", body.to_c()).unwrap();
                buf
            }
            ast::StmtKind::Return(expr) => format!("return {}", expr.to_c()),
            ast::StmtKind::Expr(expr) => expr.to_c(),
        }
    }
}

impl ast::Expr {
    pub fn to_c(&self) -> String {
        match &self.kind {
            ast::ExprKind::Str(s) => format!("{:?}", s),
            ast::ExprKind::Char(c) => format!("{:?}", *c as char),
            ast::ExprKind::Int(i) => format!("{}", i),
            ast::ExprKind::Float(f) => format!("{}", f),
            ast::ExprKind::Bool(b) => format!("{}", b),
            ast::ExprKind::Type(_) => todo!(),
            ast::ExprKind::Var { name } => name.clone(),
            ast::ExprKind::FnCall { name, args } => {
                let mut buf = String::new();
                buf.push_str(name);
                buf.push('(');
                for (i, arg) in args.iter().enumerate() {
                    buf.push_str(&arg.to_c());
                    if i < args.len() - 1 {
                        buf.push(',');
                    }
                }
                buf.push(')');
                buf
            }
            ast::ExprKind::UnaryOp { op, operand } => format!("({}{})", op.to_c(), operand.to_c()),
            ast::ExprKind::BinaryOp { op, lhs, rhs } => {
                format!("({}{}{})", lhs.to_c(), op.to_c(), rhs.to_c())
            }
            ast::ExprKind::AssignOp { op, lvalue, rvalue } => {
                format!("({}{}{})", lvalue.to_c(), op.to_c(), rvalue.to_c())
            }
        }
    }
}

impl ast::UnaryOpKind {
    pub fn to_c(&self) -> String {
        match self {
            Self::Plus => "+".to_string(),
            Self::Minus => "-".to_string(),
            Self::BitwiseNot => "~".to_string(),
            Self::LogicalNot => "!".to_string(),
            Self::Ref => "&".to_string(),
            Self::Deref => "*".to_string(),
        }
    }
}

impl ast::BinaryOpKind {
    pub fn to_c(&self) -> String {
        match self {
            Self::Plus => "+".to_string(),
            Self::Minus => "-".to_string(),
            Self::Times => "*".to_string(),
            Self::Div => "/".to_string(),
            Self::Mod => "%".to_string(),

            Self::BitwiseAnd => "&".to_string(),
            Self::BitwiseOr => "|".to_string(),
            Self::BitwiseXor => "^".to_string(),
            Self::BitwiseShl => "<<".to_string(),
            Self::BitwiseShr => ">>".to_string(),

            Self::LessThan => "<".to_string(),
            Self::GreaterThan => ">".to_string(),
            Self::LessOrEqual => "<=".to_string(),
            Self::GreaterOrEqual => ">=".to_string(),
            Self::IsEqual => "==".to_string(),
            Self::IsNotEqual => "!=".to_string(),
            Self::LogicalAnd => "&&".to_string(),
            Self::LogicalOr => "||".to_string(),
        }
    }
}

impl ast::AssignOpKind {
    pub fn to_c(&self) -> String {
        match self {
            Self::Equals => "=".to_string(),

            Self::PlusEquals => "+=".to_string(),
            Self::MinusEquals => "-=".to_string(),
            Self::TimesEquals => "*=".to_string(),
            Self::DivEquals => "/=".to_string(),
            Self::ModEquals => "%=".to_string(),

            Self::BitwiseAndEquals => "&=".to_string(),
            Self::BitwiseOrEquals => "|=".to_string(),
            Self::BitwiseNotEquals => "~=".to_string(),
            Self::BitwiseXorEquals => "^=".to_string(),
            Self::BitwiseShlEquals => "<<=".to_string(),
            Self::BitwiseShrEquals => ">>=".to_string(),
        }
    }
}

impl ast::TypeDef {
    pub fn to_c(&self) -> String {
        match &self.kind {
            ast::TypeKind::U0 => "void".to_string(),
            ast::TypeKind::U8 => "uint8_t".to_string(),
            ast::TypeKind::U16 => "uint16_t".to_string(),
            ast::TypeKind::U32 => "uint32_t".to_string(),
            ast::TypeKind::U64 => "uint64_t".to_string(),
            ast::TypeKind::I0 => "bool".to_string(),
            ast::TypeKind::I8 => "int8_t".to_string(),
            ast::TypeKind::I16 => "int16_t".to_string(),
            ast::TypeKind::I32 => "int32_t".to_string(),
            ast::TypeKind::I64 => "int64_t".to_string(),
            ast::TypeKind::F32 => "float".to_string(),
            ast::TypeKind::F64 => "double".to_string(),
            ast::TypeKind::Unknown => {
                dbg!(self);
                todo!()
            }
            _ => todo!(),
        }
    }
}
