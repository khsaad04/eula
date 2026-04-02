#![allow(dead_code)]

use std::{io::IsTerminal, path::Path};

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    input_path: &'a Path,

    character_offset: usize,
    line_offset: usize,
    first_character_of_line_offset: usize,

    tokens_buffer: Vec<Token<'a>>,
    current_token_offset: usize,
    lookahead_token_offset: usize,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub loc: Location<'a>,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind<'a> {
    // Literals
    Ident(&'a str),
    StrLiteral(String),
    CharLiteral(u8),
    IntLiteral(u128),
    FloatLiteral(f64),
    BoolLiteral(bool),

    // Keywords
    Fn,
    Return,

    // Operators
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %

    BitwiseAnd, // &
    BitwiseOr,  // |
    BitwiseNot, // ~
    BitwiseXor, // ^
    BitwiseShl, // <<
    BitwiseShr, // >>

    AddEq, // +=
    SubEq, // -=
    MulEq, // *=
    DivEq, // /=
    ModEq, // %=

    BitwiseAndEq, // &=
    BitwiseOrEq,  // |=
    BitwiseNotEq, // ~=
    BitwiseXorEq, // ^=
    BitwiseShlEq, // <<=
    BitwiseShrEq, // >>=

    Eq,          // =
    Not,         // !
    LessThan,    // <
    GreaterThan, // >

    EqEq,       // ==
    NotEq,      // !=
    LessEq,     // <=
    GreaterEq,  // >=
    LogicalAnd, // &&
    LogicalOr,  // ||

    Dot,       // .
    Comma,     // ,
    Colon,     // :
    Semicolon, // ;

    OpenParen,    // (
    CloseParen,   // )
    OpenCurly,    // {
    CloseCurly,   // }
    OpenBracket,  // [
    CloseBracket, // ]

    Arrow,   // ->
    EqArrow, // =>
    Ref,     // *.
    Deref,   // .*
    DotDot,  // ..

    // @Todo: Provide more information about the error.
    LexError,
    Eof,
}

#[derive(Debug, Clone, Copy)]
pub struct Location<'a> {
    pub input_path: &'a Path,

    pub l0: usize,
    pub c0: usize,

    pub l1: usize,
    pub c1: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, input_path: &'a Path) -> Self {
        Self {
            input,
            input_path,

            character_offset: 0,
            line_offset: 0,
            first_character_of_line_offset: 0,

            // @Speedup: Reserve some amount of capacity by default to
            // avoid re-allocation in the beginning.
            tokens_buffer: vec![],
            current_token_offset: 0,
            lookahead_token_offset: 0,
        }
    }

    pub fn next_token(&mut self) -> &Token<'a> {
        if self.current_token_offset >= self.tokens_buffer.len() {
            self.advance_token();
        }
        let result = &self.tokens_buffer[self.current_token_offset];
        self.current_token_offset += 1;
        result
    }

    pub fn peek_next_token(&mut self) -> &Token<'a> {
        self.peek_token(1)
    }

    pub fn peek_token(&mut self, lookahead: usize) -> &Token<'a> {
        for _ in 0..(self.lookahead_token_offset + lookahead - self.current_token_offset) {
            self.advance_token();
        }
        self.lookahead_token_offset += lookahead;
        &self.tokens_buffer[self.lookahead_token_offset - 1]
    }

    pub fn error_at(&self, loc: Location, desc: &str) {
        let is_tty = std::io::stdout().is_terminal();
        let (cyan, red, reset) = if is_tty {
            ("\x1b[36m", "\x1b[31m", "\x1b[0m")
        } else {
            ("", "", "")
        };

        eprintln!(
            "{}:{}:{}: error: {}",
            loc.input_path.display(),
            loc.l0 + 1,
            loc.c0 + 1,
            desc
        );

        eprintln!();
        if loc.l0 > 0 {
            eprintln!(
                "{} | {cyan}{}{reset}",
                loc.l0,
                &self.input.lines().nth(loc.l0 - 1).unwrap()
            );
        }
        let current_line = &self.input.lines().nth(loc.l0).unwrap();
        eprintln!(
            "{} | {cyan}{}{reset}{red}{}{reset}{cyan}{}{reset}",
            loc.l0 + 1,
            &current_line[..loc.c0],
            &current_line[loc.c0..loc.c1+1],
            &current_line[loc.c1+1..],
        );
        if loc.l1 < self.input.lines().into_iter().count() - 1 {
            eprintln!(
                "{} | {cyan}{}{reset}",
                loc.l0 + 2,
                &self.input.lines().nth(loc.l1 + 1).unwrap()
            );
        }
        eprintln!();
    }

    fn advance_token(&mut self) {
        self.eat_whitespaces();
        self.eat_comments();

        let l0 = self.line_offset;
        let c0 = self.character_offset - self.first_character_of_line_offset;

        let token_kind = match self.next_char() {
            Some(b'+') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::AddEq
                }
                _ => TokenKind::Add,
            },
            Some(b'-') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::SubEq
                }
                Some(b'>') => {
                    self.next_char();
                    TokenKind::Arrow
                }
                _ => TokenKind::Sub,
            },
            Some(b'*') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::MulEq
                }
                Some(b'.') => {
                    if let Some(c) = self.peek_char(2)
                        && c.is_ascii_digit()
                    {
                        //
                        // This implies that there might be a float literal after the `*`
                        //
                        // Example: .2*.1
                        //
                        // here the `*` followed by `.` is not a referance because you can't take
                        // referance of an integer literal in this language. Instead, this is a
                        // binary multiplication of two floats. So, we leave the `.` untouched to
                        // be lexed as part of the float literal the next time around.
                        //
                        TokenKind::Mul
                    } else {
                        self.next_char();
                        TokenKind::Ref
                    }
                }
                _ => TokenKind::Mul,
            },
            Some(b'/') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::DivEq
                }
                _ => TokenKind::Div,
            },
            Some(b'%') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::ModEq
                }
                _ => TokenKind::Mod,
            },
            Some(b'&') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::BitwiseAndEq
                }
                Some(b'&') => {
                    self.next_char();
                    TokenKind::LogicalAnd
                }
                _ => TokenKind::BitwiseAnd,
            },
            Some(b'|') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::BitwiseOrEq
                }
                Some(b'|') => {
                    self.next_char();
                    TokenKind::LogicalOr
                }
                _ => TokenKind::BitwiseOr,
            },
            Some(b'~') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::BitwiseNotEq
                }
                _ => TokenKind::BitwiseNot,
            },
            Some(b'^') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::BitwiseXorEq
                }
                _ => TokenKind::BitwiseXor,
            },
            Some(b'<') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::LessEq
                }
                Some(b'<') => {
                    self.next_char();
                    match self.peek_next_char() {
                        Some(b'=') => {
                            self.next_char();
                            TokenKind::BitwiseShlEq
                        }
                        _ => TokenKind::BitwiseShl,
                    }
                }
                _ => TokenKind::LessThan,
            },
            Some(b'>') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::GreaterEq
                }
                Some(b'>') => {
                    self.next_char();
                    match self.peek_next_char() {
                        Some(b'=') => {
                            self.next_char();
                            TokenKind::BitwiseShrEq
                        }
                        _ => TokenKind::BitwiseShr,
                    }
                }
                _ => TokenKind::GreaterThan,
            },
            Some(b'=') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::EqEq
                }
                Some(b'>') => {
                    self.next_char();
                    TokenKind::EqArrow
                }
                _ => TokenKind::Eq,
            },
            Some(b'!') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::NotEq
                }
                _ => TokenKind::Not,
            },
            Some(b'.') => {
                match self.peek_next_char() {
                    Some(b'.') => {
                        self.next_char();
                        TokenKind::DotDot
                    }
                    Some(b'*') => {
                        self.next_char();
                        TokenKind::Deref
                    }
                    Some(c) if c.is_ascii_digit() => {
                        // We already consumed the `.` when calling `next_char()`.
                        // So, we need to step back a little.
                        self.character_offset -= 1;
                        self.lex_num_literal()
                    }
                    _ => TokenKind::Dot,
                }
            }
            Some(b',') => TokenKind::Comma,
            Some(b':') => TokenKind::Colon,
            Some(b';') => TokenKind::Semicolon,
            Some(b'(') => TokenKind::OpenParen,
            Some(b')') => TokenKind::CloseParen,
            Some(b'{') => TokenKind::OpenCurly,
            Some(b'}') => TokenKind::CloseCurly,
            Some(b'[') => TokenKind::OpenBracket,
            Some(b']') => TokenKind::CloseBracket,
            Some(b'\'') => self.lex_char_literal(),
            Some(b'"') => self.lex_str_literal(),
            Some(c) if c.is_ascii_alphabetic() || c == b'_' => {
                while let Some(c) = self.peek_next_char()
                    && (c.is_ascii_alphanumeric() || c == b'_')
                {
                    self.next_char();
                }

                let ident_or_keyword =
                    &self.input[c0 + self.first_character_of_line_offset..self.character_offset];

                match ident_or_keyword {
                    // Booleans
                    "true" => TokenKind::BoolLiteral(true),
                    "false" => TokenKind::BoolLiteral(false),

                    // Float literals
                    "inf" => TokenKind::FloatLiteral(f64::NAN),
                    "nan" => TokenKind::FloatLiteral(f64::INFINITY),

                    // Keywords
                    "fn" => TokenKind::Fn,
                    "return" => TokenKind::Return,

                    _ => TokenKind::Ident(ident_or_keyword),
                }
            }
            Some(c) if c.is_ascii_digit() => {
                // We already consumed the first digit when calling `next_char()`.
                // So, we need to step back a little.
                self.character_offset -= 1;
                self.lex_num_literal()
            }
            None => TokenKind::Eof,
            _ => TokenKind::LexError,
        };

        let l1 = self.line_offset;
        let c1 = self.character_offset - self.first_character_of_line_offset - 1;

        assert!(l0 <= l1);
        assert!(c0 <= c1);

        self.tokens_buffer.push(Token {
            kind: token_kind,
            loc: Location {
                input_path: self.input_path,
                l0,
                c0,
                l1,
                c1,
            },
        });
    }

    fn next_char(&mut self) -> Option<u8> {
        let result = self.peek_next_char();
        self.character_offset += 1;
        if let Some(c) = result
            && c == b'\n'
        {
            self.line_offset += 1;
            self.first_character_of_line_offset = self.character_offset;
        }
        result
    }

    fn peek_next_char(&mut self) -> Option<u8> {
        self.peek_char(1)
    }

    fn peek_char(&mut self, lookahead: usize) -> Option<u8> {
        self.input
            .as_bytes()
            .get(self.character_offset + lookahead - 1)
            .copied()
    }

    fn eat_whitespaces(&mut self) {
        while let Some(c) = self.peek_next_char()
            && c.is_ascii_whitespace()
        {
            self.next_char();
        }
    }

    fn eat_comments(&mut self) {
        loop {
            if self.input[self.character_offset..].starts_with("//") {
                // eat the leading `//`
                self.next_char();
                self.next_char();

                while let Some(c) = self.peek_next_char()
                    && c != b'\n'
                {
                    self.next_char();
                }
            } else if self.input[self.character_offset..].starts_with("/*") {
                // eat the leading `/*`
                self.next_char();
                self.next_char();

                let mut depth_count = 1;
                while let Some(c) = self.peek_next_char()
                    && depth_count > 0
                {
                    if c == b'*' {
                        self.next_char();
                        if self.peek_next_char() == Some(b'/') {
                            depth_count -= 1;
                        }
                    } else if c == b'/' {
                        self.next_char();
                        if self.peek_next_char() == Some(b'*') {
                            depth_count += 1;
                        }
                    }
                }
            } else {
                break;
            }
            self.eat_whitespaces();
        }
    }

    fn lex_num_literal(&mut self) -> TokenKind<'a> {
        let mut result = String::new();

        let mut base = 10;
        let mut float = false;
        let mut exp = false;
        let mut exp_sign = false;

        if let Some(c) = self.peek_next_char()
            && c == b'0'
        {
            match self.peek_char(2) {
                Some(b'x') => {
                    self.next_char();
                    self.next_char();
                    base = 16;
                }
                Some(b'o') => {
                    self.next_char();
                    self.next_char();
                    base = 8;
                }
                Some(b'b') => {
                    self.next_char();
                    self.next_char();
                    base = 2;
                }
                _ => base = 10,
            }
        }

        while let Some(c) = self.peek_next_char()
            && (c.is_ascii_digit() || c == b'_' || c == b'.' || c == b'e' || c == b'-' || c == b'+')
        {
            self.next_char();

            // We allow an indefinite amount of underscores in int literals (for now).
            if c == b'_' {
                continue;
            }

            if c == b'.' {
                if float {
                    break;
                }
                float = true;
            }

            if c == b'e' {
                if exp {
                    break;
                }
                if let Some(c) = self.peek_next_char()
                    && !c.is_ascii_digit()
                    && c != b'-'
                    && c != b'+'
                {
                    return TokenKind::LexError;
                }
                exp = true;
            }

            if c == b'-' || c == b'+' {
                if exp_sign {
                    break;
                }
                if let Some(c) = self.peek_next_char()
                    && !c.is_ascii_digit()
                {
                    return TokenKind::LexError;
                }
                exp_sign = true;
            }

            result.push(c as char);
        }

        if float {
            if base != 10 {
                return TokenKind::LexError;
            }
            match result.parse::<f64>() {
                Ok(v) => TokenKind::FloatLiteral(v),
                Err(_) => TokenKind::LexError,
            }
        } else {
            match u128::from_str_radix(&result, base) {
                Ok(v) => TokenKind::IntLiteral(v),
                Err(_) => TokenKind::LexError,
            }
        }
    }

    fn lex_str_literal(&mut self) -> TokenKind<'a> {
        let mut result = String::new();

        while let Some(c) = self.next_char()
            && c != b'"'
        {
            match c {
                b'\\' => match self.next_char() {
                    Some(b'n') => result.push('\n'),
                    Some(b'r') => result.push('\r'),
                    Some(b't') => result.push('\t'),
                    Some(b'\\') => result.push('\\'),
                    Some(b'"') => result.push('"'),
                    Some(b'x') => {
                        let mut v: u8 = 0;
                        for i in (0..=1).rev() {
                            match self.next_char() {
                                Some(c) if c.is_ascii_hexdigit() => {
                                    v |= ascii_to_hex_byte(c).unwrap_or(b'\0') << (4 * i);
                                }
                                _ => return TokenKind::LexError,
                            }
                        }
                        result.push(v as char);
                    }
                    Some(b'u') | Some(b'U') => {
                        todo!("Unicode code point support in string literals")
                    }
                    _ => {
                        return TokenKind::LexError;
                    }
                },
                _ => result.push(c as char),
            }
        }

        TokenKind::StrLiteral(result)
    }

    fn lex_char_literal(&mut self) -> TokenKind<'a> {
        let mut result = 0_u8;

        while let Some(c) = self.next_char()
            && c != b'\''
        {
            match c {
                b'\\' => match self.next_char() {
                    Some(b'n') => result = b'\n',
                    Some(b'r') => result = b'\r',
                    Some(b't') => result = b'\t',
                    Some(b'\\') => result = b'\\',
                    Some(b'\'') => result = b'"',
                    Some(b'x') => {
                        let mut v: u8 = 0;
                        for i in (0..=1).rev() {
                            match self.next_char() {
                                Some(c) if c.is_ascii_hexdigit() => {
                                    v |= ascii_to_hex_byte(c).unwrap_or(b'\0') << (4 * i);
                                }
                                _ => return TokenKind::LexError,
                            }
                        }
                        result = v;
                    }
                    _ => {
                        return TokenKind::LexError;
                    }
                },
                _ => result = c,
            }
        }

        TokenKind::CharLiteral(result)
    }
}

fn ascii_to_hex_byte(c: u8) -> Option<u8> {
    match c {
        b'0'..=b'9' => Some(c - b'0'),
        b'A'..=b'Z' => Some(c - b'A' + 10),
        b'a'..=b'z' => Some(c - b'a' + 10),
        _ => None,
    }
}
