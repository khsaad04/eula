use crate::ast;

use std::{
    fmt::{self, Write},
    fs, io,
};

#[derive(Debug)]
pub struct Codegen {
    ast: ast::TopLevel,
    string_buffer: String,
}

impl Codegen {
    pub fn new(ast: ast::TopLevel) -> Self {
        Self {
            ast,
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
            match &decl {
                ast::Decl::Fn(decl) => {
                    write!(
                        self.string_buffer,
                        "{} {}(",
                        decl.return_type.to_c_type(),
                        decl.name
                    )?;
                    for (i, param) in decl.params.iter().enumerate() {
                        write!(self.string_buffer, "{}", &param.ty.to_c_type())?;
                        if i < decl.params.len() - 1 {
                            write!(self.string_buffer, ", ")?;
                        }
                    }
                    writeln!(self.string_buffer, ");")?;
                }
                ast::Decl::Var(decl) => {
                    writeln!(self.string_buffer, "{} {}", decl.ty.to_c_type(), decl.name)?;
                }
            }
        }
        writeln!(self.string_buffer)?;

        // Definitions
        writeln!(self.string_buffer, "// definitions")?;
        for decl in self.ast.decls.values() {
            match &decl {
                ast::Decl::Fn(decl) => {
                    if let Some(body) = &decl.body {
                        write!(
                            self.string_buffer,
                            "{} {}(",
                            decl.return_type.to_c_type(),
                            decl.name
                        )?;
                        for (i, param) in decl.params.iter().enumerate() {
                            write!(
                                self.string_buffer,
                                "{} {}",
                                param.ty.to_c_type(),
                                param.name
                            )?;
                            if i < decl.params.len() - 1 {
                                write!(self.string_buffer, ", ")?;
                            }
                        }
                        writeln!(self.string_buffer, ") {{")?;

                        // Function body
                        for stmt in body {
                            match stmt {
                                ast::Stmt::Return(expr) => {
                                    writeln!(
                                        self.string_buffer,
                                        "    return {};",
                                        expr.to_c_expr()
                                    )?;
                                }
                                ast::Stmt::Expr(expr) => {
                                    writeln!(self.string_buffer, "    {};", expr.to_c_expr())?;
                                }
                                _ => todo!(),
                            }
                        }
                        writeln!(self.string_buffer, "}}")?;
                    }
                }
                ast::Decl::Var(_) => todo!(),
            }
        }
        writeln!(self.string_buffer)?;
        Ok(())
    }
}
