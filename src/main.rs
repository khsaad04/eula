#![allow(dead_code)]

mod ast;
mod codegen;
mod lexer;
mod parser;
mod token;

use std::{env, fs, path, process};

#[derive(Debug)]
struct Compiler;

impl Compiler {
    fn new() -> Self {
        Self {}
    }

    fn compile(&self, input_path: &str) {
        match fs::read_to_string(input_path) {
            Err(e) => {
                eprintln!("error: Could not read from `{input_path}`: {e}");
                process::exit(1);
            }
            Ok(input) => {
                let input_path = path::Path::new(input_path);
                let output_path = path::Path::new(
                    path::Path::new(input_path.file_name().unwrap().to_str().unwrap())
                        .file_stem()
                        .unwrap(),
                );

                let mut parser = parser::Parser::new(&input, input_path);
                let ast = parser.parse_top_level();

                let mut code_generator = codegen::Codegen::new(ast);
                code_generator.generate().unwrap();
                code_generator
                    .write_entire_buffer_to_file(output_path.with_extension("c").to_str().unwrap())
                    .unwrap();

                process::Command::new("gcc")
                    .arg("-o")
                    .arg(output_path)
                    .arg(output_path.with_extension("c"))
                    .status()
                    .unwrap();
            }
        }
    }
}

fn main() {
    let mut args = env::args();
    args.next(); // Eat the executable path.

    if let Some(input_path) = args.next() {
        let compiler = Compiler::new();
        compiler.compile(&input_path);
    } else {
        eprintln!("error: Missing input file path.");
        process::exit(1);
    }
}
