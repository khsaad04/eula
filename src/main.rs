#![allow(dead_code)]

mod lexer;
mod parser;

use std::{env, fs, path, process};

#[derive(Debug, Clone)]
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
                let mut parser = parser::Parser::new(&input, path::Path::new(input_path));
                let ast = parser.parse_top_level();
                dbg!(ast);
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
