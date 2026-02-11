#![allow(unused)] // @Temp: To avoid those annoying warnings until I implement everything.
#![allow(clippy::upper_case_acronyms)]

use std::path::Path;

pub struct Lexer<'src> {
    source: &'src str,
    pub source_path: &'src Path,

    current_char_index: usize,
    current_line_index: usize,
    start_of_current_line: usize,

    // For now we only store upto a maximum of 1 token for lookahead.
    // In the future this might turn into a `[Option<Token<'src>>;n]`
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
    StrLiteral(&'src str),
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

    // @Todo: Should provide more information regarding the error.
    ParseError,
    EOF,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str, path: &'src str) -> Self {
        Self {
            source,
            source_path: Path::new(path),

            current_char_index: 0,
            current_line_index: 0,
            start_of_current_line: 0,

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

    pub fn peek_next_token(&mut self) -> &Token<'src> {
        if self.token_buffer.is_none() {
            self.token_buffer = Some(self.advance_token());
        }
        self.token_buffer.as_ref().unwrap()
    }

    fn advance_token(&mut self) -> Token<'src> {
        self.eat_whitespaces();
        self.eat_comments();

        let token_line_start = self.current_line_index;
        let token_col_start = self.current_char_index;

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
            Some(b'"') => self.parse_str_literal(token_col_start),
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
                    self.parse_int_or_float_literal(token_col_start, true)
                }
                None => TokenKind::EOF,
                _ => TokenKind::Dash,
            },
            Some(b'.') => match self.peek_next_char() {
                Some(c) if c.is_ascii_digit() => {
                    self.parse_float_literal(token_col_start, false, 0)
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

                let potential_identifier = &self.source[token_col_start..self.current_char_index];
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
                self.parse_int_or_float_literal(token_col_start, false)
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

        Token {
            kind: token_kind,
            source_path: self.source_path,
            l0: token_line_start + 1,
            c0: token_col_start - self.start_of_current_line,
            l1: token_line_start + 1,
            c1: self.current_char_index - self.start_of_current_line - 1,
        }
    }

    fn next_char(&mut self) -> Option<u8> {
        let result = self.peek_next_char();
        self.current_char_index += 1;
        result
    }

    fn peek_next_char(&self) -> Option<u8> {
        self.peek_char(0)
    }

    fn peek_char(&self, n: usize) -> Option<u8> {
        self.source
            .as_bytes()
            .get(self.current_char_index + n)
            .copied()
    }

    fn eat_whitespaces(&mut self) {
        while let Some(c) = self.peek_next_char()
            && c.is_ascii_whitespace()
        {
            if c == b'\n' {
                self.current_line_index += 1;
                self.start_of_current_line = self.current_char_index;
            }
            self.next_char();
        }
    }

    fn eat_comments(&mut self) {
        loop {
            // Line comments
            if self.source[self.current_char_index..].starts_with("//") {
                self.current_char_index += 2; // consume the leading `//`
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
        token_col_start: usize,
        is_negative: bool,
    ) -> TokenKind<'src> {
        let mut v: i64 = 0;
        let mut len = 0;

        while let Some(c) = self.peek_next_char()
            && (c.is_ascii_digit() || c == b'_')
        // We allow an indefinite amount of underscores inside int literals (for now);
        // even trailing underscores.
        {
            self.next_char();
            len += 1;
        }

        let token_col_start = if is_negative {
            token_col_start + 1
        } else {
            token_col_start
        };

        let literal_string = &self.source[token_col_start..self.current_char_index];

        let mut factor = 1;
        for w in literal_string.bytes().rev() {
            if w == b'_' {
                continue;
            }

            v += (w - b'0') as i64 * factor as i64;
            factor *= 10;
        }

        if is_negative {
            v = -v;
        }

        if self.peek_next_char() == Some(b'.') {
            self.next_char();
            self.parse_float_literal(token_col_start + len + 2, is_negative, v)
        } else {
            TokenKind::IntLiteral(v)
        }
    }

    fn parse_float_literal(
        &mut self,
        token_col_start: usize,
        is_negative: bool,
        integer_part: i64,
    ) -> TokenKind<'src> {
        let mut v = integer_part as f64;

        while let Some(c) = self.peek_next_char()
            && (c.is_ascii_digit() || c == b'_')
        // We allow an indefinite amount of underscores inside float literals (for now);
        // even trailing underscores.
        {
            self.next_char();
        }

        let literal_string = &self.source[token_col_start..self.current_char_index];

        let mut factor = 0.1;
        for w in literal_string.bytes() {
            if w == b'_' {
                continue;
            }

            if is_negative {
                v -= (w - b'0') as f64 * factor;
            } else {
                v += (w - b'0') as f64 * factor;
            }
            factor *= 0.1;
        }

        TokenKind::FloatLiteral(v)
    }

    fn parse_str_literal(&mut self, token_col_start: usize) -> TokenKind<'src> {
        // @Temp: Naive implementation to get this over with quickly.
        // @Todo: Properly parse escape codes and everything.

        loop {
            if self.next_char() == Some(b'"') {
                break;
            }
        }

        // The +1 and -1 are to avoid pushing the quotation marks into the string literal
        let string_literal = &self.source[token_col_start + 1..self.current_char_index - 1];
        TokenKind::StrLiteral(string_literal)
    }
}
