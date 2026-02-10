#![allow(unused)] // @Temp: To avoid those annoying warnings until I implement everything.
#![allow(clippy::upper_case_acronyms)]

use std::path::Path;

pub struct Lexer<'a> {
    input_data: &'a str,
    file_path: &'a Path,

    current_character_index: usize,
    current_line_index: usize,
    beginning_of_current_line: usize,

    // For now we only store upto a maximum of 1 token for lookahead
    token_buffer: Option<Token<'a>>,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,

    // Location
    // maybe factor this out into a seperate struct?
    pub file_path: &'a Path,

    pub l0: usize,
    pub c0: usize,

    pub l1: usize,
    pub c1: usize,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind<'a> {
    Ident(&'a str),
    StringLiteral(&'a str),
    CharLiteral(u8),
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    // @Todo: Add hex, oct and bin literals.
    //        Or just merge them into `IntLiteral`.

    // Keywords
    Fn,

    // Single character tokens
    Bang,         // !
    Pound,        // #
    Dollar,       // $
    Percent,      // %
    Ampersand,    // &
    SingleQuote,  // '
    OpenParen,    // (
    CloseParen,   // )
    Star,         // *
    Plus,         // +
    Comma,        // ,
    Dash,         // -
    Dot,          // .
    Slash,        // /
    Colon,        // :
    Semicolon,    // ;
    LessThan,     // <
    Eq,           // =
    GreaterThan,  // >
    Question,     // ?
    At,           // @
    OpenBracket,  // [
    Backslash,    // \
    CloseBracket, // ]
    Caret,        // ^
    Underscore,   // _
    Backtick,     // `
    OpenCurly,    // {
    Bar,          // |
    CloseCurly,   // }
    Tilde,        // ~

    // Multiple character tokens
    EqEq,         // ==
    NotEq,        // !=
    LessEq,       // <=
    GreaterEq,    // >=
    AndAnd,       // &&
    OrOr,         // ||
    PlusEq,       // +=
    MinusEq,      // -=
    MulEq,        // *=
    DivEq,        // /=
    ModEq,        // %=
    BitwiseShl,   // <<
    BitwiseShr,   // >>
    BitwiseShlEq, // <<=
    BitwiseShrEq, // >>=
    BitwiseNotEq, // ~=
    BitwiseAndEq, // &=
    BitwiseOrEq,  // |=
    BitwiseXorEq, // ^=
    Arrow,        // ->
    EqArrow,      // =>
    Ref,          // *.
    Deref,        // .*
    DotDot,       // ..

    // @Todo: Should contain more information regarding the error.
    ParseError,
    EOF,
}

impl<'a> Lexer<'a> {
    pub fn new(path: &'a str, data: &'a str) -> Self {
        Self {
            file_path: Path::new(path),
            input_data: data,

            current_character_index: 0,
            current_line_index: 0,
            beginning_of_current_line: 0,

            token_buffer: None,
        }
    }

    pub fn next_token(&mut self) -> Token<'_> {
        if let Some(token) = self.token_buffer.take() {
            token
        } else {
            self.advance_token()
        }
    }

    pub fn peek_next_token(&mut self) -> &Token<'a> {
        if self.token_buffer.is_none() {
            self.token_buffer = Some(self.advance_token());
        }
        self.token_buffer.as_ref().unwrap()
    }

    fn advance_token(&mut self) -> Token<'a> {
        let mut token_kind = TokenKind::ParseError;

        self.eat_whitespaces();
        self.eat_comments();
        self.eat_whitespaces();

        let token_l0 = self.current_line_index;
        let token_c0 = self.current_character_index;

        let mut token_l1 = token_l0;
        let mut token_c1 = token_c0;

        match self.next_char() {
            Some(b'!') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::NotEq;
                    self.next_char();
                    token_c1 += 1;
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Bang,
            },
            Some(b'"') => {
                // @Temp: Naive implementation to get this over with quickly.
                // @Todo: Properly parse escape codes and everything.

                while let Some(c) = self.next_char()
                    && c != b'"'
                {
                    token_c1 += 1;
                }

                token_c1 += 1; // eat the trailing "

                let string_literal = &self.input_data[token_c0 + 1..token_c1];
                token_kind = TokenKind::StringLiteral(string_literal);
            }
            Some(b'#') => token_kind = TokenKind::Pound,
            Some(b'$') => token_kind = TokenKind::Dollar,
            Some(b'%') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::ModEq;
                    self.next_char();
                    token_c1 += 1;
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Percent,
            },
            Some(b'&') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::BitwiseAndEq;
                    self.next_char();
                    token_c1 += 1;
                }
                Some(b'&') => {
                    token_kind = TokenKind::AndAnd;
                    self.next_char();
                    token_c1 += 1;
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Ampersand,
            },
            Some(b'\'') => token_kind = TokenKind::SingleQuote, // @Todo: Handle character literals.
            Some(b'(') => token_kind = TokenKind::OpenParen,
            Some(b')') => token_kind = TokenKind::CloseParen,
            Some(b'*') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::MulEq;
                    self.next_char();
                    token_c1 += 1;
                }
                Some(b'.') => {
                    if let Some(c) = self.peek_char(1)
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
                        // be parsed as part of the float literal the next time around.
                        //

                        token_kind = TokenKind::Star;
                    } else {
                        token_kind = TokenKind::Ref;
                        self.next_char();
                        token_c1 += 1;
                    }
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Star,
            },
            Some(b'+') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::PlusEq;
                    self.next_char();
                    token_c1 += 1;
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Plus,
            },
            Some(b',') => token_kind = TokenKind::Comma,
            Some(b'-') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::MinusEq;
                    self.next_char();
                    token_c1 += 1;
                }
                Some(b'>') => {
                    token_kind = TokenKind::Arrow;
                    self.next_char();
                    token_c1 += 1;
                }
                Some(c) if c.is_ascii_digit() => {
                    self.next_char();
                    token_c1 += 1;
                    token_kind = self.parse_int_literal(token_c0, token_c1);
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Dash,
            },
            Some(b'.') => match self.peek_next_char() {
                // @Todo: Handle float literals.
                Some(b'.') => {
                    token_kind = TokenKind::DotDot;
                    self.next_char();
                    token_c1 += 1;
                }
                Some(b'*') => {
                    token_kind = TokenKind::Deref;
                    self.next_char();
                    token_c1 += 1;
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Dot,
            },
            Some(b'/') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::DivEq;
                    self.next_char();
                    token_c1 += 1;
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Slash,
            },
            Some(b':') => token_kind = TokenKind::Colon,
            Some(b';') => token_kind = TokenKind::Semicolon,
            Some(b'<') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::LessEq;
                    self.next_char();
                    token_c1 += 1;
                }
                Some(b'<') => {
                    match self.peek_next_char() {
                        Some(b'=') => {
                            token_kind = TokenKind::BitwiseShlEq;
                            self.next_char();
                            token_c1 += 1;
                        }
                        None => token_kind = TokenKind::EOF,
                        _ => token_kind = TokenKind::BitwiseShl,
                    }
                    self.next_char();
                    token_c1 += 1;
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::LessThan,
            },
            Some(b'=') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::EqEq;
                    self.next_char();
                    token_c1 += 1;
                }
                Some(b'>') => {
                    token_kind = TokenKind::EqArrow;
                    self.next_char();
                    token_c1 += 1;
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Eq,
            },
            Some(b'>') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::GreaterEq;
                    self.next_char();
                    token_c1 += 1;
                }
                Some(b'>') => {
                    match self.peek_next_char() {
                        Some(b'=') => {
                            token_kind = TokenKind::BitwiseShrEq;
                            self.next_char();
                            token_c1 += 1;
                        }
                        None => token_kind = TokenKind::EOF,
                        _ => token_kind = TokenKind::BitwiseShr,
                    }
                    self.next_char();
                    token_c1 += 1;
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::GreaterThan,
            },
            Some(b'?') => token_kind = TokenKind::Question,
            Some(b'@') => token_kind = TokenKind::At,
            Some(b'[') => token_kind = TokenKind::OpenBracket,
            Some(b'\\') => token_kind = TokenKind::Backslash,
            Some(b']') => token_kind = TokenKind::CloseBracket,
            Some(b'^') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::BitwiseXorEq;
                    self.next_char();
                    token_c1 += 1;
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Caret,
            },
            Some(b'_') => token_kind = TokenKind::Underscore,
            Some(b'`') => token_kind = TokenKind::Backtick,
            Some(b'{') => token_kind = TokenKind::OpenCurly,
            Some(b'|') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::BitwiseOrEq;
                    self.next_char();
                    token_c1 += 1;
                }
                Some(b'|') => {
                    token_kind = TokenKind::OrOr;
                    self.next_char();
                    token_c1 += 1;
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Bar,
            },
            Some(b'}') => token_kind = TokenKind::CloseCurly,
            Some(b'~') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::BitwiseNotEq;
                    self.next_char();
                    token_c1 += 1;
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Tilde,
            },
            Some(c) if c.is_ascii_alphabetic() || c == b'_' => {
                while let Some(c) = self.peek_next_char()
                    && c.is_ascii_alphanumeric()
                {
                    self.next_char();
                    token_c1 += 1;
                }

                let potential_identifier = &self.input_data[token_c0..=token_c1];
                match potential_identifier {
                    // Booleans
                    "true" => token_kind = TokenKind::BoolLiteral(true),
                    "false" => token_kind = TokenKind::BoolLiteral(false),
                    // Keywords
                    "fn" => token_kind = TokenKind::Fn,
                    _ => {
                        token_kind = TokenKind::Ident(potential_identifier);
                    }
                }
            }
            Some(c) if c.is_ascii_digit() => {
                token_kind = self.parse_int_literal(token_c0, token_c1);

                // @Todo: Handle float literals.
            }
            None => token_kind = TokenKind::EOF,
            _ => token_kind = TokenKind::ParseError,
        }

        Token {
            kind: token_kind,
            file_path: self.file_path,
            l0: token_l0 + 1,
            c0: token_c0 - self.beginning_of_current_line,
            l1: token_l1 + 1,
            c1: token_c1 - self.beginning_of_current_line,
        }
    }

    fn next_char(&mut self) -> Option<u8> {
        let result = self.peek_next_char();
        self.current_character_index += 1;
        result
    }

    fn peek_next_char(&self) -> Option<u8> {
        self.peek_char(0)
    }

    fn peek_char(&self, n: usize) -> Option<u8> {
        self.input_data
            .as_bytes()
            .get(self.current_character_index + n)
            .copied()
    }

    fn eat_whitespaces(&mut self) {
        while let Some(c) = self.peek_next_char()
            && is_space(c)
        {
            if c == b'\n' {
                self.current_line_index += 1;
                self.beginning_of_current_line = self.current_character_index;
            }
            self.next_char();
        }
    }

    fn eat_comments(&mut self) {
        // @Todo: Handle multi-line comments

        if self.input_data[self.current_character_index..].starts_with("//") {
            self.current_character_index += 2; // eat the //
            while let Some(c) = self.peek_next_char()
                && c != b'\n'
            {
                self.next_char();
            }
        }
    }

    fn parse_int_literal(&mut self, token_c0: usize, mut token_c1: usize) -> TokenKind<'a> {
        let mut v = String::new();

        while let Some(c) = self.peek_next_char()
            && (c.is_ascii_digit() || c == b'_')
        // We allow an indefinite amount of underscores inside int literals (for now);
        // even trailing underscores.
        {
            self.next_char();
            token_c1 += 1;
        }

        let potential_int_literal = &self.input_data[token_c0..=token_c1];
        for c in potential_int_literal.chars() {
            if c == '_' {
                continue;
            }
            v.push(c);
        }

        match v.parse::<i64>() {
            Ok(w) => TokenKind::IntLiteral(w),
            Err(_) => TokenKind::ParseError,
        }
    }
}

fn is_space(c: u8) -> bool {
    matches!(
        c, // According to the POSIX locale specifications
        b'\x20'| // space (' ')
        b'\x0C'| // form-feed ('\f')
        b'\x0A'| // newline ('\n')
        b'\x0D'| // carriage return ('\r')
        b'\x09'| // horizontal tab ('\t')
        b'\x0B'  // vertical tab ('\v')
    )
}
