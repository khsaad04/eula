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
        let mut token_kind = TokenKind::ParseError;

        self.eat_whitespaces();
        self.eat_comments();

        let token_line_start = self.current_line_index;
        let token_col_start = self.current_char_index;

        match self.next_char() {
            Some(b'!') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::NotEq;
                    self.next_char();
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Bang,
            },
            Some(b'\'') => token_kind = TokenKind::SingleQuote, // @Todo: Handle character literals.
            Some(b'"') => {
                token_kind = self.parse_str_literal(token_col_start);
            }
            Some(b'%') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::ModEq;
                    self.next_char();
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Percent,
            },
            Some(b'&') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::BitwiseAndEq;
                    self.next_char();
                }
                Some(b'&') => {
                    token_kind = TokenKind::AndAnd;
                    self.next_char();
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Ampersand,
            },
            Some(b'*') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::MulEq;
                    self.next_char();
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
                    }
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Star,
            },
            Some(b'+') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::PlusEq;
                    self.next_char();
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Plus,
            },
            Some(b'-') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::MinusEq;
                    self.next_char();
                }
                Some(b'>') => {
                    token_kind = TokenKind::Arrow;
                    self.next_char();
                }
                Some(c) if c.is_ascii_digit() => {
                    self.next_char();
                    token_kind = self.parse_int_literal(token_col_start);
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Dash,
            },
            Some(b'.') => match self.peek_next_char() {
                // @Todo: Handle float literals.
                Some(b'.') => {
                    token_kind = TokenKind::DotDot;
                    self.next_char();
                }
                Some(b'*') => {
                    token_kind = TokenKind::Deref;
                    self.next_char();
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Dot,
            },
            Some(b'/') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::DivEq;
                    self.next_char();
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Slash,
            },
            Some(b'<') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::LessEq;
                    self.next_char();
                }
                Some(b'<') => {
                    match self.peek_next_char() {
                        Some(b'=') => {
                            token_kind = TokenKind::BitwiseShlEq;
                            self.next_char();
                        }
                        None => token_kind = TokenKind::EOF,
                        _ => token_kind = TokenKind::BitwiseShl,
                    }
                    self.next_char();
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::LessThan,
            },
            Some(b'=') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::EqEq;
                    self.next_char();
                }
                Some(b'>') => {
                    token_kind = TokenKind::EqArrow;
                    self.next_char();
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Eq,
            },
            Some(b'>') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::GreaterEq;
                    self.next_char();
                }
                Some(b'>') => {
                    match self.peek_next_char() {
                        Some(b'=') => {
                            token_kind = TokenKind::BitwiseShrEq;
                            self.next_char();
                        }
                        None => token_kind = TokenKind::EOF,
                        _ => token_kind = TokenKind::BitwiseShr,
                    }
                    self.next_char();
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::GreaterThan,
            },
            Some(b'^') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::BitwiseXorEq;
                    self.next_char();
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Caret,
            },
            Some(b'|') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::BitwiseOrEq;
                    self.next_char();
                }
                Some(b'|') => {
                    token_kind = TokenKind::OrOr;
                    self.next_char();
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Bar,
            },
            Some(b'~') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::BitwiseNotEq;
                    self.next_char();
                }
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Tilde,
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
                token_kind = self.parse_int_literal(token_col_start);
                // @Todo: Handle float literals.
            }
            Some(b'#') => token_kind = TokenKind::Pound,
            Some(b'$') => token_kind = TokenKind::Dollar,
            Some(b'(') => token_kind = TokenKind::OpenParen,
            Some(b')') => token_kind = TokenKind::CloseParen,
            Some(b',') => token_kind = TokenKind::Comma,
            Some(b':') => token_kind = TokenKind::Colon,
            Some(b';') => token_kind = TokenKind::Semicolon,
            Some(b'?') => token_kind = TokenKind::Question,
            Some(b'@') => token_kind = TokenKind::At,
            Some(b'[') => token_kind = TokenKind::OpenBracket,
            Some(b'\\') => token_kind = TokenKind::Backslash,
            Some(b']') => token_kind = TokenKind::CloseBracket,
            Some(b'_') => token_kind = TokenKind::Underscore,
            Some(b'`') => token_kind = TokenKind::Backtick,
            Some(b'{') => token_kind = TokenKind::OpenCurly,
            Some(b'}') => token_kind = TokenKind::CloseCurly,
            None => token_kind = TokenKind::EOF,
            _ => token_kind = TokenKind::ParseError,
        }

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

    fn parse_int_literal(&mut self, token_col_start: usize) -> TokenKind<'src> {
        let mut v: i64 = 0;

        while let Some(c) = self.peek_next_char()
            && (c.is_ascii_digit() || c == b'_')
        // We allow an indefinite amount of underscores inside int literals (for now);
        // even trailing underscores.
        {
            self.next_char();
        }

        let literal_string = &self.source[token_col_start..self.current_char_index];

        let mut factor = 1;
        for w in literal_string.bytes().rev() {
            if w == b'_' {
                continue;
            }

            v += (w - b'0') as i64 * factor as i64;
            factor *= 10;
        }

        TokenKind::IntLiteral(v)
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
