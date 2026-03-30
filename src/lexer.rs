pub struct Lexer<'src> {
    // Read-only
    src: &'src str,

    // Mutable state
    char_offset: usize,
    line_offset: usize,
    beginning_of_line: usize,
}

pub struct Token<'src> {
    pub kind: TokenKind<'src>,

    // Location in terms of row and col
    pub r0: usize,
    pub c0: usize,

    pub r1: usize,
    pub c1: usize,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind<'src> {
    // Literals
    Ident(&'src str),
    StrLiteral(String),
    CharLiteral(u8),
    IntLiteral(i128),
    FloatLiteral(f64),
    BoolLiteral(bool),

    // Keywords
    Fn,
    Return,
    Defer,

    For,
    Break,
    Continue,

    If,
    Else,
    Case,

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

    LogicalAnd,  // &&
    LogicalOr,   // ||
    EqEq,        // ==
    LessThan,    // <
    GreaterThan, // >
    Eq,          // =
    Not,         // !

    NotEq,     // !=
    LessEq,    // <=
    GreaterEq, // >=

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

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src,

            char_offset: 0,
            line_offset: 0,
            beginning_of_line: 0,
        }
    }

    pub fn next_token(&mut self) -> Token<'src> {
        self.eat_whitespaces();
        self.eat_comments();

        let r0 = self.line_offset + 1;
        let c0 = self.char_offset - self.beginning_of_line + 1;

        let token_kind = match self.next_char() {
            Some(b'+') => match self.peek_next_char() {
                Some(b'=') => {
                    self.next_char();
                    TokenKind::AddEq
                }
                Some(c) if c.is_ascii_digit() => {
                    // We already consumed the `+` when calling `next_char()`.
                    // So, we need to step back a little.
                    self.char_offset -= 1;
                    self.lex_num_literal()
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
                Some(c) if c.is_ascii_digit() => {
                    // We already consumed the `-` when calling `next_char()`.
                    // So, we need to step back a little.
                    self.char_offset -= 1;
                    self.lex_num_literal()
                }
                _ => TokenKind::Sub,
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
                        self.char_offset -= 1;
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

                let ident_or_keyword = &self.src[c0 + self.beginning_of_line - 1..self.char_offset];

                match ident_or_keyword {
                    // Booleans
                    "true" => TokenKind::BoolLiteral(true),
                    "false" => TokenKind::BoolLiteral(false),

                    // Float literal
                    "inf" => TokenKind::FloatLiteral("inf".parse::<f64>().unwrap()), // Unwrapping here is fine because parsing these should
                    "nan" => TokenKind::FloatLiteral("nan".parse::<f64>().unwrap()), // never fail unless something went very wrong.

                    // Keywords
                    "fn" => TokenKind::Fn,
                    "return" => TokenKind::Return,
                    "defer" => TokenKind::Defer,
                    "for" => TokenKind::For,
                    "break" => TokenKind::Break,
                    "continue" => TokenKind::Continue,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "case" => TokenKind::Case,
                    _ => TokenKind::Ident(ident_or_keyword),
                }
            }
            Some(c) if c.is_ascii_digit() => {
                // We already consumed the first digit when calling `next_char()`.
                // So, we need to step back a little.
                self.char_offset -= 1;
                self.lex_num_literal()
            }
            None => TokenKind::Eof,
            _ => TokenKind::LexError,
        };

        let r1 = self.line_offset + 1;
        let c1 = self.char_offset - self.beginning_of_line;

        Token {
            kind: token_kind,
            r0,
            c0,
            r1,
            c1,
        }
    }

    fn next_char(&mut self) -> Option<u8> {
        let result = self.peek_next_char();
        self.char_offset += 1;
        if let Some(c) = result
            && c == b'\n'
        {
            self.line_offset += 1;
            self.beginning_of_line = self.char_offset;
        }
        result
    }

    fn peek_next_char(&mut self) -> Option<u8> {
        self.peek_char(0)
    }

    fn peek_char(&mut self, lookahead: usize) -> Option<u8> {
        self.src
            .as_bytes()
            .get(self.char_offset + lookahead)
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
            if self.src[self.char_offset..].starts_with("//") {
                // eat the leading `//`
                self.next_char();
                self.next_char();

                while let Some(c) = self.peek_next_char()
                    && c != b'\n'
                {
                    self.next_char();
                }
            } else if self.src[self.char_offset..].starts_with("/*") {
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

    fn lex_num_literal(&mut self) -> TokenKind<'src> {
        let mut result = String::new();

        let mut base = 10;
        let mut float = false;
        let mut exp = false;
        let mut exp_sign = false;

        if let Some(c) = self.peek_next_char()
            && (c == b'-' || c == b'+')
        {
            result.push(c as char);
            self.next_char();
        }

        if let Some(c) = self.peek_next_char()
            && c == b'0'
        {
            match self.peek_char(1) {
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
            match i128::from_str_radix(&result, base) {
                Ok(v) => TokenKind::IntLiteral(v),
                Err(_) => TokenKind::LexError,
            }
        }
    }

    fn lex_str_literal(&mut self) -> TokenKind<'src> {
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

    fn lex_char_literal(&mut self) -> TokenKind<'src> {
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
