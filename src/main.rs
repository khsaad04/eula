mod lexer;

use std::fs;

fn main() {
    let input_path = "examples/hello.eula"; // @Temp: Hard-coded for now.
    let input_string = fs::read_to_string(input_path).unwrap();

    if !input_string.is_empty() {
        println!("\nSuccessfully read {input_path}\n");
    }

    let mut lex = lexer::Lexer::new(input_path, &input_string);

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
