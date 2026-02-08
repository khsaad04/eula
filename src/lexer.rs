#![allow(unused)]

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
    CharLiteral(char),
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),

    // Keywords
    Fn,

    // Single character tokens
    Bang,         // !
    DoubleQuote,  // "
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

    // TODO: Should contain more information regarding the error.
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

        let token_l0 = self.current_line_index;
        let token_c0 = self.current_character_index - self.beginning_of_current_line;

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
            Some(b'"') => token_kind = TokenKind::DoubleQuote, // TODO: Handle string literals.
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
            Some(b'\'') => token_kind = TokenKind::SingleQuote, // TODO: Handle character literals.
            Some(b'(') => token_kind = TokenKind::OpenParen,
            Some(b')') => token_kind = TokenKind::CloseParen,
            Some(b'*') => match self.peek_next_char() {
                Some(b'=') => {
                    token_kind = TokenKind::MulEq;
                    self.next_char();
                    token_c1 += 1;
                }
                Some(b'.') => {
                    token_kind = TokenKind::Ref;
                    self.next_char();
                    token_c1 += 1;
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
                None => token_kind = TokenKind::EOF,
                _ => token_kind = TokenKind::Dash,
            },
            Some(b'.') => match self.peek_next_char() {
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
            Some(b'/') => match self.peek_next_char() { // TODO: Handle comments.
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
            None => token_kind = TokenKind::EOF,
            _ => todo!(),
            // TODO: Handle identifiers.
            // TODO: Handle keywords.
            // TODO: Handle integer literals.
            // TODO: Handle float literals.
            // TODO: Handle bool literals.
        }

        Token {
            kind: token_kind,
            file_path: self.file_path,
            l0: token_l0,
            c0: token_c0,
            l1: token_l1,
            c1: token_c1,
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
