#![allow(unused)] // @Temp: To avoid those annoying warnings until I implement everything.
#![allow(clippy::upper_case_acronyms)]

use std::path::Path;

pub struct Lexer<'src> {
    source: &'src str,
    pub source_path: &'src Path,

    character_cursor: usize,
    line_cursor: usize,
    line_begin: usize,

    // For now we only store upto a maximum of 1 token for lookahead.
    // In the future this might turn into `[Option<Token<'src>>;n]`.
    token_buffer: Option<Token<'src>>,
}

pub struct Token<'src> {
    pub kind: TokenKind<'src>,
    pub source_path: &'src Path,

    pub l0: usize,
    pub c0: usize,

    pub l1: usize,
    pub c1: usize,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind<'src> {
    Ident(&'src str),

    // Literals
    StrLiteral(&'src str),
    CharLiteral(u8),
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    // @Todo: Add hex, oct and bin literals; or merge them into `IntLiteral`.

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
    Arrow,        // ->
    EqArrow,      // =>
    Ref,          // *.
    Deref,        // .*
    DotDot,       // ..
    BitwiseShl,   // <<
    BitwiseShr,   // >>
    BitwiseShlEq, // <<=
    BitwiseShrEq, // >>=
    BitwiseNotEq, // ~=
    BitwiseAndEq, // &=
    BitwiseOrEq,  // |=
    BitwiseXorEq, // ^=

    // @Todo: Provide more information about the error.
    ParseError,
    EOF,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str, source_path: &'src str) -> Self {
        Self {
            source,
            source_path: Path::new(source_path),

            character_cursor: 0,
            line_cursor: 0,
            line_begin: 0,

            token_buffer: None,
        }
    }

    pub fn next_token(&mut self) -> Token<'src> {
        if let Some(token) = self.token_buffer.take() {
            token
        } else {
            self.advance_token()
        }
    }

    pub fn peek_next_token(&mut self) -> &Token<'src> {
        if self.token_buffer.is_none() {
            self.token_buffer = Some(self.advance_token());
        }
        self.token_buffer.as_ref().unwrap()
    }

    fn advance_token(&mut self) -> Token<'src> {
        self.eat_whitespaces();
        self.eat_comments();

        let l0 = self.line_cursor + 1;
        let c0 = self.character_cursor - self.line_begin + 1;

        let token_kind = match self.next_char() {
            Some(b'!') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::NotEq
                }
                None => TokenKind::EOF,
                _ => TokenKind::Bang,
            },
            Some(b'\'') => TokenKind::SingleQuote, // @Todo: Handle character literals.
            Some(b'"') => self.parse_str_literal(self.character_cursor),
            Some(b'%') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::ModEq
                }
                None => TokenKind::EOF,
                _ => TokenKind::Percent,
            },
            Some(b'&') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::BitwiseAndEq
                }
                Some(b'&') => {
                    self.next_char();
                    TokenKind::AndAnd
                }
                None => TokenKind::EOF,
                _ => TokenKind::Ampersand,
            },
            Some(b'*') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::MulEq
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

                        TokenKind::Star
                    } else {
                        self.next_char();
                        TokenKind::Ref
                    }
                }
                None => TokenKind::EOF,
                _ => TokenKind::Star,
            },
            Some(b'+') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::PlusEq
                }
                None => TokenKind::EOF,
                _ => TokenKind::Plus,
            },
            Some(b'-') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::MinusEq
                }
                Some(b'>') => {
                    self.next_char();
                    TokenKind::Arrow
                }
                Some(c) if c.is_ascii_digit() || c == b'.' => {
                    self.next_char();
                    self.parse_int_or_float_literal(self.character_cursor, true)
                }
                None => TokenKind::EOF,
                _ => TokenKind::Dash,
            },
            Some(b'.') => match self.peek_next_char() {
                Some(c) if c.is_ascii_digit() => {
                    self.next_char();
                    self.parse_float_literal(self.character_cursor, false, 0)
                }
                Some(b'.') => {
                    self.next_char();
                    TokenKind::DotDot
                }
                Some(b'*') => {
                    self.next_char();
                    TokenKind::Deref
                }
                None => TokenKind::EOF,
                _ => TokenKind::Dot,
            },
            Some(b'/') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::DivEq
                }
                None => TokenKind::EOF,
                _ => TokenKind::Slash,
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
                        None => TokenKind::EOF,
                        _ => TokenKind::BitwiseShl,
                    }
                }
                None => TokenKind::EOF,
                _ => TokenKind::LessThan,
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
                None => TokenKind::EOF,
                _ => TokenKind::Eq,
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
                        None => TokenKind::EOF,
                        _ => TokenKind::BitwiseShr,
                    }
                }
                None => TokenKind::EOF,
                _ => TokenKind::GreaterThan,
            },
            Some(b'^') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::BitwiseXorEq
                }
                None => TokenKind::EOF,
                _ => TokenKind::Caret,
            },
            Some(b'|') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::BitwiseOrEq
                }
                Some(b'|') => {
                    self.next_char();
                    TokenKind::OrOr
                }
                None => TokenKind::EOF,
                _ => TokenKind::Bar,
            },
            Some(b'~') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::BitwiseNotEq
                }
                None => TokenKind::EOF,
                _ => TokenKind::Tilde,
            },
            Some(c) if c.is_ascii_alphabetic() || c == b'_' => {
                while let Some(c) = self.peek_next_char()
                    && c.is_ascii_alphanumeric()
                {
                    self.next_char();
                }

                let potential_identifier =
                    &self.source[c0 + self.line_begin - 1..self.character_cursor];

                match potential_identifier {
                    // Booleans
                    "true" => TokenKind::BoolLiteral(true),
                    "false" => TokenKind::BoolLiteral(false),
                    // Keywords
                    "fn" => TokenKind::Fn,
                    _ => TokenKind::Ident(potential_identifier),
                }
            }
            Some(c) if c.is_ascii_digit() => {
                self.parse_int_or_float_literal(self.character_cursor, false)
            }
            Some(b'#') => TokenKind::Pound,
            Some(b'$') => TokenKind::Dollar,
            Some(b'(') => TokenKind::OpenParen,
            Some(b')') => TokenKind::CloseParen,
            Some(b',') => TokenKind::Comma,
            Some(b':') => TokenKind::Colon,
            Some(b';') => TokenKind::Semicolon,
            Some(b'?') => TokenKind::Question,
            Some(b'@') => TokenKind::At,
            Some(b'[') => TokenKind::OpenBracket,
            Some(b'\\') => TokenKind::Backslash,
            Some(b']') => TokenKind::CloseBracket,
            Some(b'_') => TokenKind::Underscore,
            Some(b'`') => TokenKind::Backtick,
            Some(b'{') => TokenKind::OpenCurly,
            Some(b'}') => TokenKind::CloseCurly,
            None => TokenKind::EOF,
            _ => TokenKind::ParseError,
        };

        let l1 = self.line_cursor + 1;
        let c1 = self.character_cursor - self.line_begin;

        Token {
            kind: token_kind,
            source_path: self.source_path,
            l0,
            c0,
            l1,
            c1,
        }
    }

    fn next_char(&mut self) -> Option<u8> {
        let result = self.peek_next_char();
        self.character_cursor += 1;
        result
    }

    fn peek_next_char(&self) -> Option<u8> {
        self.peek_char(0)
    }

    fn peek_char(&self, n: usize) -> Option<u8> {
        self.source
            .as_bytes()
            .get(self.character_cursor + n)
            .copied()
    }

    fn eat_whitespaces(&mut self) {
        while let Some(c) = self.peek_next_char()
            && c.is_ascii_whitespace()
        {
            self.next_char();
            if c == b'\n' {
                self.line_cursor += 1;
                self.line_begin = self.character_cursor;
            }
        }
    }

    fn eat_comments(&mut self) {
        loop {
            // Line comments
            if self.source[self.character_cursor..].starts_with("//") {
                self.character_cursor += 2; // consume the leading `//`
                while let Some(c) = self.peek_next_char()
                    && c != b'\n'
                {
                    self.next_char();
                }
                self.eat_whitespaces();
            } else {
                break;
            }
        }
        // @Todo: Handle block comments
    }

    fn parse_int_or_float_literal(
        &mut self,
        token_start_pos: usize,
        is_negative: bool,
    ) -> TokenKind<'src> {
        let mut value: i64 = 0;

        // We allow an indefinite amount of underscores inside int literals (for now);
        // even trailing underscores.
        while let Some(c) = self.peek_next_char()
            && (c.is_ascii_digit() || c == b'_')
        {
            self.next_char();
        }

        let literal = &self.source[token_start_pos..self.character_cursor];

        let mut factor = 1;
        for d in literal.bytes().rev() {
            if !d.is_ascii_digit() {
                continue;
            }

            value += (d - b'0') as i64 * factor as i64;
            factor *= 10;
        }

        if is_negative {
            value = -value;
        }

        if self.peek_next_char() == Some(b'.') {
            self.next_char();
            self.parse_float_literal(self.character_cursor, is_negative, value)
        } else {
            TokenKind::IntLiteral(value)
        }
    }

    fn parse_float_literal(
        &mut self,
        token_start_pos: usize,
        is_negative: bool,
        integer_part: i64,
    ) -> TokenKind<'src> {
        let mut value = integer_part as f64;

        // We allow an indefinite amount of underscores inside float literals (for now);
        // even trailing underscores.
        while let Some(c) = self.peek_next_char()
            && (c.is_ascii_digit() || c == b'_')
        {
            self.next_char();
        }

        let literal = &self.source[token_start_pos..self.character_cursor];

        let mut factor = 0.1;
        for d in literal.bytes() {
            if !d.is_ascii_digit() {
                continue;
            }

            if is_negative {
                value -= (d - b'0') as f64 * factor;
            } else {
                value += (d - b'0') as f64 * factor;
            }
            factor *= 0.1;
        }

        TokenKind::FloatLiteral(value)
    }

    fn parse_str_literal(&mut self, token_start_pos: usize) -> TokenKind<'src> {
        // @Temp: Naive implementation to get this over with quickly.
        // @Todo: Properly parse escape codes and everything.

        loop {
            if self.next_char() == Some(b'"') {
                break;
            }
        }

        // The -1 is to avoid pushing the trailing quotation mark into the string literal.
        TokenKind::StrLiteral(&self.source[token_start_pos..self.character_cursor - 1])
    }
}
