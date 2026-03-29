pub struct Lexer<'src> {
    // Read-only
    src: &'src str,

    // Mutable state
    current_char: Option<u8>,
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

            current_char: src.as_bytes().first().copied(),
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

        let token_kind = match self.current_char {
            Some(b'+') => {
                self.advance();
                match self.current_char {
                    Some(b'=') => TokenKind::AddEq,
                    _ => {
                        self.rewind();
                        TokenKind::Add
                    }
                }
            }
            Some(b'-') => {
                self.advance();
                match self.current_char {
                    Some(b'=') => TokenKind::SubEq,
                    Some(b'>') => TokenKind::Arrow,
                    _ => {
                        self.rewind();
                        TokenKind::Sub
                    }
                }
            }
            Some(b'*') => {
                self.advance();
                match self.current_char {
                    Some(b'=') => TokenKind::MulEq,
                    Some(b'.') => {
                        self.advance();
                        if let Some(c) = self.current_char
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

                            self.rewind(); // for the `c`
                            self.rewind(); // for the `.`
                            TokenKind::Mul
                        } else {
                            TokenKind::Ref
                        }
                    }
                    _ => {
                        self.rewind();
                        TokenKind::Mul
                    }
                }
            }
            Some(b'/') => {
                self.advance();
                match self.current_char {
                    Some(b'=') => TokenKind::DivEq,
                    _ => {
                        self.rewind();
                        TokenKind::Div
                    }
                }
            }
            Some(b'%') => {
                self.advance();
                match self.current_char {
                    Some(b'=') => TokenKind::ModEq,
                    _ => {
                        self.rewind();
                        TokenKind::Mod
                    }
                }
            }
            Some(b'&') => {
                self.advance();
                match self.current_char {
                    Some(b'=') => TokenKind::BitwiseAndEq,
                    Some(b'&') => TokenKind::LogicalAnd,
                    _ => {
                        self.rewind();
                        TokenKind::BitwiseAnd
                    }
                }
            }
            Some(b'|') => {
                self.advance();
                match self.current_char {
                    Some(b'=') => TokenKind::BitwiseOrEq,
                    Some(b'|') => TokenKind::LogicalOr,
                    _ => {
                        self.rewind();
                        TokenKind::BitwiseOr
                    }
                }
            }
            Some(b'~') => {
                self.advance();
                match self.current_char {
                    Some(b'=') => TokenKind::BitwiseNotEq,
                    _ => {
                        self.rewind();
                        TokenKind::BitwiseNot
                    }
                }
            }
            Some(b'^') => {
                self.advance();
                match self.current_char {
                    Some(b'=') => TokenKind::BitwiseXorEq,
                    _ => {
                        self.rewind();
                        TokenKind::BitwiseXor
                    }
                }
            }
            Some(b'<') => {
                self.advance();
                match self.current_char {
                    Some(b'=') => TokenKind::LessEq,
                    Some(b'<') => {
                        self.advance();
                        match self.current_char {
                            Some(b'=') => TokenKind::BitwiseShlEq,
                            _ => {
                                self.rewind();
                                TokenKind::BitwiseShl
                            }
                        }
                    }
                    _ => {
                        self.rewind();
                        TokenKind::LessThan
                    }
                }
            }
            Some(b'>') => {
                self.advance();
                match self.current_char {
                    Some(b'=') => TokenKind::GreaterEq,
                    Some(b'>') => {
                        self.advance();
                        match self.current_char {
                            Some(b'=') => TokenKind::BitwiseShrEq,
                            _ => {
                                self.rewind();
                                TokenKind::BitwiseShr
                            }
                        }
                    }
                    _ => {
                        self.rewind();
                        TokenKind::GreaterThan
                    }
                }
            }
            Some(b'=') => {
                self.advance();
                match self.current_char {
                    Some(b'=') => TokenKind::EqEq,
                    Some(b'>') => TokenKind::EqArrow,
                    _ => {
                        self.rewind();
                        TokenKind::Eq
                    }
                }
            }
            Some(b'!') => {
                self.advance();
                match self.current_char {
                    Some(b'=') => TokenKind::NotEq,
                    _ => {
                        self.rewind();
                        TokenKind::Not
                    }
                }
            }
            Some(b'.') => {
                self.advance();
                match self.current_char {
                    Some(b'.') => TokenKind::DotDot,
                    Some(b'*') => TokenKind::Deref,
                    Some(c) if c.is_ascii_digit() => {
                        // We already consumed the `.` when calling `advance()`.
                        // So, we need to step back a little.
                        self.rewind();
                        self.lex_num_literal()
                    }
                    _ => {
                        self.rewind();
                        TokenKind::Dot
                    }
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
                self.advance();
                while let Some(c) = self.current_char
                    && (c.is_ascii_alphanumeric() || c == b'_')
                {
                    self.advance();
                }

                let ident_or_keyword = &self.src[c0 + self.beginning_of_line - 1..self.char_offset];
                self.rewind();

                match ident_or_keyword {
                    // Booleans
                    "true" => TokenKind::BoolLiteral(true),
                    "false" => TokenKind::BoolLiteral(false),

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
            Some(c) if c.is_ascii_digit() => self.lex_num_literal(),
            None => TokenKind::Eof,
            _ => TokenKind::LexError,
        };

        self.advance();

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

    fn advance(&mut self) {
        self.char_offset += 1;
        self.current_char = self.src.as_bytes().get(self.char_offset).copied();
    }

    fn rewind(&mut self) {
        if self.char_offset > 0 {
            self.char_offset -= 1;
            self.current_char = self.src.as_bytes().get(self.char_offset).copied();
        }
    }

    fn eat_whitespaces(&mut self) {
        while let Some(c) = self.current_char
            && c.is_ascii_whitespace()
        {
            self.advance();
            if c == b'\n' {
                self.line_offset += 1;
                self.beginning_of_line = self.char_offset;
            }
        }
    }

    fn eat_comments(&mut self) {
        loop {
            if self.src[self.char_offset..].starts_with("//") {
                // eat the leading `//`
                self.advance();
                self.advance();

                while let Some(c) = self.current_char
                    && c != b'\n'
                {
                    self.advance();
                }
            } else if self.src[self.char_offset..].starts_with("/*") {
                // eat the leading `/*`
                self.advance();
                self.advance();

                let mut depth_count = 1;
                while let Some(c) = self.current_char
                    && depth_count > 0
                {
                    if c == b'\n' {
                        self.advance();
                        self.line_offset += 1;
                        self.beginning_of_line = self.char_offset;
                    } else if c == b'*' {
                        self.advance();
                        if self.current_char == Some(b'/') {
                            depth_count -= 1;
                        }
                    } else if c == b'/' {
                        self.advance();
                        if self.current_char == Some(b'*') {
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
        let mut buf = String::new();

        let base: u32 = if let Some(c) = self.current_char
            && c == b'0'
        {
            self.advance();
            match self.current_char {
                Some(b'x') => {
                    self.advance();
                    16
                }
                Some(b'o') => {
                    self.advance();
                    8
                }
                Some(b'b') => {
                    self.advance();
                    2
                }
                _ => {
                    self.rewind();
                    10
                }
            }
        } else {
            10
        };

        let mut is_float = false;

        // We allow an indefinite amount of underscores inside int literals (for now);
        // even trailing underscores.
        while let Some(c) = self.current_char
            && (c.is_ascii_digit() || c == b'_' || c == b'.')
        {
            self.advance();
            if c == b'_' {
                continue;
            }
            if c == b'.' {
                if is_float {
                    break;
                }
                is_float = true;
            }
            buf.push(c as char);
        }

        if is_float {
            if base != 10 {
                return TokenKind::LexError;
            }
            match buf.parse::<f64>() {
                Ok(v) => TokenKind::FloatLiteral(v),
                Err(_) => TokenKind::LexError,
            }
        } else {
            match i128::from_str_radix(&buf, base) {
                Ok(v) => TokenKind::IntLiteral(v),
                Err(_) => TokenKind::LexError,
            }
        }
    }

    fn lex_str_literal(&mut self) -> TokenKind<'src> {
        let mut result = String::new();

        self.advance();
        while let Some(c) = self.current_char
            && c != b'"'
        {
            match c {
                b'\\' => {
                    self.advance();
                    match self.current_char {
                        Some(b'n') => result.push('\n'),
                        Some(b'r') => result.push('\r'),
                        Some(b't') => result.push('\t'),
                        Some(b'\\') => result.push('\\'),
                        Some(b'"') => result.push('"'),
                        Some(b'x') => {
                            let mut v: u8 = 0;
                            for i in (0..=1).rev() {
                                self.advance();
                                match self.current_char {
                                    Some(c) if c.is_ascii_hexdigit() => {
                                        v |= ascii_to_hex_byte(c) << (4 * i);
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
                            self.rewind();
                            return TokenKind::LexError;
                        }
                    }
                }
                _ => result.push(c as char),
            }
            self.advance();
        }

        TokenKind::StrLiteral(result)
    }

    fn lex_char_literal(&mut self) -> TokenKind<'src> {
        let mut result = 0_u8;

        self.advance();
        while let Some(c) = self.current_char
            && c != b'"'
        {
            match c {
                b'\\' => {
                    self.advance();
                    match self.current_char {
                        Some(b'n') => result = b'\n',
                        Some(b'r') => result = b'\r',
                        Some(b't') => result = b'\t',
                        Some(b'\\') => result = b'\\',
                        Some(b'"') => result = b'"',
                        Some(b'x') => {
                            let mut v: u8 = 0;
                            for i in (0..=1).rev() {
                                self.advance();
                                match self.current_char {
                                    Some(c) if c.is_ascii_hexdigit() => {
                                        v |= ascii_to_hex_byte(c) << (4 * i);
                                    }
                                    _ => return TokenKind::LexError,
                                }
                            }
                            result = v;
                        }
                        _ => {
                            self.rewind();
                            return TokenKind::LexError;
                        }
                    }
                }
                _ => result = c,
            }
            self.advance();
        }

        TokenKind::CharLiteral(result)
    }
}

fn ascii_to_hex_byte(c: u8) -> u8 {
    match c {
        b'0'..=b'9' => c - b'0',
        b'a'..=b'z' => c - b'a' + 10,
        b'A'..=b'Z' => c - b'A' + 10,
        _ => 0,
    }
}
