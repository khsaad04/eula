mod lexer;

use std::fs;

fn main() {
    let input_path = "examples/hello.ext";
    let input_string = fs::read_to_string(input_path).unwrap();

    if !input_string.is_empty() {
        println!("Successfully read {input_path}");
        println!();
    }

    let mut lex = lexer::Lexer::new(input_path, &input_string);

    while let token = lex.next_token()
        && token.kind != lexer::TokenKind::EOF
    {
        println!("{:?}", token);
    }
}
