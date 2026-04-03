mod lexer;

use std::{fs, path::Path, process::exit};

fn main() {
    let mut args = std::env::args();
    args.next(); // Consume the executable path.

    let input_path = args.next();
    if input_path.is_none() {
        eprintln!("ERROR: Missing input file path.");
        exit(1);
    }

    let input_path = input_path.unwrap();
    let input = fs::read_to_string(&input_path).unwrap();

    let mut lex = lexer::Lexer::new(&input, Path::new(&input_path));

    let token = lex.peek_token(8);
    let loc = token.loc;
    lex.report_error_at(loc, "This is a test for reporting errors.");

    while let token = lex.next_token()
        && token.kind != lexer::TokenKind::Eof
    {
        println!(
            "{}:{:>2}:{:>2} -> {:>2}:{:>2}: {:?}",
            token.loc.input_path.display(),
            token.loc.l0 + 1,
            token.loc.c0 + 1,
            token.loc.l1 + 1,
            token.loc.c1 + 1,
            token.kind
        );
    }
}
