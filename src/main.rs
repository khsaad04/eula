mod lexer;

use std::{fs, process::exit};

fn main() {
    let mut args = std::env::args();
    args.next(); // Consume the executable path.

    let input_path = args.next();
    if input_path.is_none() {
        eprintln!("ERROR: Missing input file path.");
        exit(1);
    }

    let input_path = input_path.unwrap();
    let input_string = fs::read_to_string(&input_path).unwrap();

    let mut lex = lexer::Lexer::new(&input_string);
    while let token = lex.next_token()
        && token.kind != lexer::TokenKind::Eof
    {
        println!(
            "{:>2}:{:>2} -> {:>2}:{:>2}: {:?}",
            token.r0, token.c0, token.r1, token.c1, token.kind
        );
    }
}
