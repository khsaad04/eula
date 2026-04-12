mod lexer;

use std::{env, fs, path, process};

fn main() {
    let mut args = env::args();
    args.next(); // Eat the executable path.

    if let Some(input_path) = args.next() {
        if let Ok(input) = fs::read_to_string(&input_path) {
            let mut l = lexer::Lexer::new(&input, path::Path::new(&input_path));

            let token = l.peek_token(7);
            let loc = token.loc; // Copy the location.
            l.report_error_at(loc, "This is a test for reporting errors.");

            while let token = l.next_token()
                && token.kind != lexer::TokenKind::Eof
            {
                println!(
                    "{}:{:>2}:{:>2} -> {:>2}:{:>2}: {:?}",
                    token.loc.input_path.display(),
                    token.loc.l0 + 1,
                    token.loc.c0 + 1,
                    token.loc.l1 + 1,
                    token.loc.c1 + 1,
                    token.kind,
                );
            }
        } else {
            eprintln!("error: Could not read `{input_path}` to string.");
            process::exit(1);
        }
    } else {
        eprintln!("error: Missing input file path.");
        process::exit(1);
    }
}
