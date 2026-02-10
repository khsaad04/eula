mod lexer;

use std::{fs, process::exit};

fn main() {
    let mut args = std::env::args();
    args.next(); // consume the executable path

    let input_path = args.next();
    if input_path.is_none() {
        eprintln!("missing input file path.");
        exit(1);
    }

    let input_path = input_path.unwrap();
    let input_string = fs::read_to_string(&input_path).unwrap();

    println!("Successfully read {}", &input_path);

    let mut lex = lexer::Lexer::new(&input_path, &input_string);

    while let token = lex.next_token()
        && token.kind != lexer::TokenKind::EOF
    {
        println!(
            "{}:{:>2}:{:>2} -> {:>2}:{:>2}: {:?}",
            token.file_path.display(),
            token.l0,
            token.c0,
            token.l1,
            token.c1,
            token.kind
        );
    }
}
