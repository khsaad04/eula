#![allow(dead_code)]

use std::{io::IsTerminal, path::Path, str::Chars};

#[derive(Debug)]
pub struct Lexer<'a> {
    // Read-only data
    input: &'a str,
    input_path: &'a Path,

    // Lexing state
    chars_iter: Chars<'a>,
    char_index: usize,
    line_index: usize,
    line_start: usize,

    tokens_buffer: Vec<Token<'a>>,
    current_token_offset: usize,
    lookahead_token_offset: usize,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub loc: Location<'a>,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    // Literals
    Ident(String),
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

            chars_iter: input.chars(),
            char_index: 0,
            line_index: 0,
            line_start: 0,

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

    pub fn report_error_at(&self, loc: Location, msg: &str) {
        assert!(
            loc.l0 <= loc.l1,
            "Invalid location found while reporting error, check call site."
        );

        eprintln!(
            "{}:{}:{}: error: {}",
            loc.input_path.display(),
            loc.l0 + 1,
            loc.c0 + 1,
            msg
        );

        eprintln!();
        {
            let is_tty = std::io::stdout().is_terminal(); // To ensure ansi escape codes are supported.

            let (ansi_code_cyan, ansi_code_red, ansi_code_reset) = if is_tty {
                ("\x1b[36m", "\x1b[31m", "\x1b[0m")
            } else {
                ("", "", "")
            };

            let padding = format!("{}", loc.l1 + 2).len();

            // Previous line
            if let Some(previous_line) = &self.input.lines().nth(loc.l0 - 1) {
                eprintln!(
                    "{LINE_NO:>PAD$} | {CYAN}{}{RESET}",
                    previous_line,
                    LINE_NO = loc.l0,
                    PAD = padding,
                    CYAN = ansi_code_cyan,
                    RESET = ansi_code_reset,
                );
            }

            // Actually relevant line(s)
            //
            // @Note: It's okay to panic if any of the unwraps fail here.
            // That would mean there is a bug somewhere else.
            //
            if loc.l0 == loc.l1 {
                let current_line = &self.input.lines().nth(loc.l0).unwrap();
                eprintln!(
                    "{LINE_NO:>PAD$} | {CYAN}{}{RED}{}{CYAN}{}{RESET}",
                    &current_line[..loc.c0],
                    &current_line[loc.c0..loc.c1 + 1],
                    &current_line[loc.c1 + 1..],
                    LINE_NO = loc.l0 + 1,
                    PAD = padding,
                    CYAN = ansi_code_cyan,
                    RED = ansi_code_red,
                    RESET = ansi_code_reset,
                );
                if !is_tty {
                    eprintln!(
                        "{LINE_NO:>PAD$} | {SPACES}{ARROWS}",
                        LINE_NO = "",
                        PAD = padding,
                        SPACES = " ".repeat(loc.c0),
                        ARROWS = "^".repeat(loc.c1 - loc.c0 + 1)
                    );
                }
            } else {
                let first_line = &self.input.lines().nth(loc.l0).unwrap();
                eprintln!(
                    "{LINE_NO:>PAD$} | {CYAN}{}{RED}{}{RESET}",
                    &first_line[..loc.c0],
                    &first_line[loc.c0..],
                    LINE_NO = loc.l0 + 1,
                    PAD = padding,
                    CYAN = ansi_code_cyan,
                    RED = ansi_code_red,
                    RESET = ansi_code_reset,
                );
                if !is_tty {
                    eprintln!(
                        "{LINE_NO:>PAD$} | {SPACES}{ARROWS}",
                        LINE_NO = "",
                        PAD = padding,
                        SPACES = " ".repeat(loc.c0),
                        ARROWS = "^".repeat(first_line.chars().count() - loc.c0)
                    );
                }

                for i in 1..(loc.l1 - loc.l0) {
                    let middle_line = &self.input.lines().nth(loc.l0 + i).unwrap();
                    eprintln!(
                        "{LINE_NO:>PAD$} | {RED}{}{RESET}",
                        &middle_line[..],
                        LINE_NO = loc.l0 + i + 1,
                        PAD = padding,
                        RED = ansi_code_red,
                        RESET = ansi_code_reset,
                    );
                    if !is_tty {
                        eprintln!(
                            "{LINE_NO:>PAD$} | {ARROWS}",
                            LINE_NO = "",
                            PAD = padding,
                            ARROWS = "^".repeat(middle_line.chars().count())
                        );
                    }
                }

                let last_line = &self.input.lines().nth(loc.l1).unwrap();
                eprintln!(
                    "{LINE_NO:>PAD$} | {RED}{}{CYAN}{}{RESET}",
                    &last_line[..=loc.c1],
                    &last_line[loc.c1 + 1..],
                    LINE_NO = loc.l1 + 1,
                    PAD = padding,
                    CYAN = ansi_code_cyan,
                    RED = ansi_code_red,
                    RESET = ansi_code_reset,
                );
                if !is_tty {
                    eprintln!(
                        "{LINE_NO:>PAD$} | {ARROWS}",
                        LINE_NO = "",
                        PAD = padding,
                        ARROWS = "^".repeat(loc.c1 + 1)
                    );
                }
            }

            // Next line
            if let Some(next_line) = &self.input.lines().nth(loc.l1 + 1) {
                eprintln!(
                    "{LINE_NO:>PAD$} | {CYAN}{}{RESET}",
                    next_line,
                    LINE_NO = loc.l1 + 2,
                    PAD = padding,
                    CYAN = ansi_code_cyan,
                    RESET = ansi_code_reset,
                );
            }
        }
        eprintln!();
    }

    fn advance_token(&mut self) {
        self.eat_whitespaces();
        self.eat_comments();

        let l0 = self.line_index;
        let c0 = self.char_index - self.line_start;

        let token_kind = match self.peek_next_char() {
            Some(c) if c.is_alphabetic() || c == '_' => {
                let mut ident_or_keyword = String::from(self.next_char().unwrap());

                while let Some(c) = self.peek_next_char()
                    && (c.is_alphanumeric() || c == '_')
                {
                    ident_or_keyword.push(self.next_char().unwrap());
                }

                match &ident_or_keyword[..] {
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
            Some(c) if c.is_ascii_digit() => self.lex_num_literal(),
            Some('+') => {
                self.next_char();
                match self.peek_next_char() {
                    Some('=') => {
                        self.next_char();
                        TokenKind::AddEq
                    }
                    _ => TokenKind::Add,
                }
            }
            Some('-') => {
                self.next_char();
                match self.peek_next_char() {
                    Some('=') => {
                        self.next_char();
                        TokenKind::SubEq
                    }
                    Some('>') => {
                        self.next_char();
                        TokenKind::Arrow
                    }
                    _ => TokenKind::Sub,
                }
            }
            Some('*') => {
                self.next_char();
                match self.peek_next_char() {
                    Some('=') => {
                        self.next_char();
                        TokenKind::MulEq
                    }
                    Some('.') => {
                        if let Some(c) = self.peek_second_char()
                            && c.is_numeric()
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
                }
            }
            Some('/') => {
                self.next_char();
                match self.peek_next_char() {
                    Some('=') => {
                        self.next_char();
                        TokenKind::DivEq
                    }
                    _ => TokenKind::Div,
                }
            }
            Some('%') => {
                self.next_char();
                match self.peek_next_char() {
                    Some('=') => {
                        self.next_char();
                        TokenKind::ModEq
                    }
                    _ => TokenKind::Mod,
                }
            }
            Some('&') => {
                self.next_char();
                match self.peek_next_char() {
                    Some('=') => {
                        self.next_char();
                        TokenKind::BitwiseAndEq
                    }
                    Some('&') => {
                        self.next_char();
                        TokenKind::LogicalAnd
                    }
                    _ => TokenKind::BitwiseAnd,
                }
            }
            Some('|') => {
                self.next_char();
                match self.peek_next_char() {
                    Some('=') => {
                        self.next_char();
                        TokenKind::BitwiseOrEq
                    }
                    Some('|') => {
                        self.next_char();
                        TokenKind::LogicalOr
                    }
                    _ => TokenKind::BitwiseOr,
                }
            }
            Some('~') => {
                self.next_char();
                match self.peek_next_char() {
                    Some('=') => {
                        self.next_char();
                        TokenKind::BitwiseNotEq
                    }
                    _ => TokenKind::BitwiseNot,
                }
            }
            Some('^') => {
                self.next_char();
                match self.peek_next_char() {
                    Some('=') => {
                        self.next_char();
                        TokenKind::BitwiseXorEq
                    }
                    _ => TokenKind::BitwiseXor,
                }
            }
            Some('<') => {
                self.next_char();
                match self.peek_next_char() {
                    Some('=') => {
                        self.next_char();
                        TokenKind::LessEq
                    }
                    Some('<') => {
                        self.next_char();
                        match self.peek_next_char() {
                            Some('=') => {
                                self.next_char();
                                TokenKind::BitwiseShlEq
                            }
                            _ => TokenKind::BitwiseShl,
                        }
                    }
                    _ => TokenKind::LessThan,
                }
            }
            Some('>') => {
                self.next_char();
                match self.peek_next_char() {
                    Some('=') => {
                        self.next_char();
                        TokenKind::GreaterEq
                    }
                    Some('>') => {
                        self.next_char();
                        match self.peek_next_char() {
                            Some('=') => {
                                self.next_char();
                                TokenKind::BitwiseShrEq
                            }
                            _ => TokenKind::BitwiseShr,
                        }
                    }
                    _ => TokenKind::GreaterThan,
                }
            }
            Some('=') => {
                self.next_char();
                match self.peek_next_char() {
                    Some('=') => {
                        self.next_char();
                        TokenKind::EqEq
                    }
                    Some('>') => {
                        self.next_char();
                        TokenKind::EqArrow
                    }
                    _ => TokenKind::Eq,
                }
            }
            Some('!') => {
                self.next_char();
                match self.peek_next_char() {
                    Some('=') => {
                        self.next_char();
                        TokenKind::NotEq
                    }
                    _ => TokenKind::Not,
                }
            }
            Some('.') => {
                self.next_char();
                match self.peek_next_char() {
                    Some('.') => {
                        self.next_char();
                        TokenKind::DotDot
                    }
                    Some('*') => {
                        self.next_char();
                        TokenKind::Deref
                    }
                    Some(c) if c.is_numeric() => {
                        // We already consumed the `.` when calling `next_char()`.
                        // So, we need to step back a little.
                        self.char_index -= 1;
                        self.lex_num_literal()
                    }
                    _ => TokenKind::Dot,
                }
            }
            Some(',') => {
                self.next_char();
                TokenKind::Comma
            }
            Some(':') => {
                self.next_char();
                TokenKind::Colon
            }
            Some(';') => {
                self.next_char();
                TokenKind::Semicolon
            }
            Some('(') => {
                self.next_char();
                TokenKind::OpenParen
            }
            Some(')') => {
                self.next_char();
                TokenKind::CloseParen
            }
            Some('{') => {
                self.next_char();
                TokenKind::OpenCurly
            }
            Some('}') => {
                self.next_char();
                TokenKind::CloseCurly
            }
            Some('[') => {
                self.next_char();
                TokenKind::OpenBracket
            }
            Some(']') => {
                self.next_char();
                TokenKind::CloseBracket
            }
            Some('\'') => {
                self.next_char();
                self.lex_char_literal()
            }
            Some('"') => {
                self.next_char();
                self.lex_str_literal()
            }
            None => TokenKind::Eof,
            _ => TokenKind::LexError,
        };

        let l1 = self.line_index;

        // @Cleanup: Do something better!
        let c1 = if self.char_index > self.line_start {
            self.char_index - self.line_start - 1
        } else {
            0
        };

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

    fn next_char(&mut self) -> Option<char> {
        let result = self.chars_iter.next();
        self.char_index += 1;
        if let Some(c) = result
            && c == '\n'
        {
            self.line_index += 1;
            self.line_start = self.char_index;
        }
        result
    }

    fn peek_next_char(&self) -> Option<char> {
        self.chars_iter.clone().next()
    }

    fn peek_second_char(&self) -> Option<char> {
        let mut iter = self.chars_iter.clone();
        iter.next();
        iter.next()
    }

    fn eat_whitespaces(&mut self) {
        while let Some(c) = self.peek_next_char()
            && c.is_whitespace()
        {
            self.next_char();
        }
    }

    fn eat_comments(&mut self) {
        loop {
            if self.input[self.char_index..].starts_with("//") {
                // eat the leading `//`
                self.next_char();
                self.next_char();

                while let Some(c) = self.peek_next_char()
                    && c != '\n'
                {
                    self.next_char();
                }
            } else if self.input[self.char_index..].starts_with("/*") {
                // eat the leading `/*`
                self.next_char();
                self.next_char();

                let mut depth_count = 1;
                while let Some(c) = self.peek_next_char()
                    && depth_count > 0
                {
                    if c == '*' {
                        self.next_char();
                        if self.peek_next_char() == Some('/') {
                            depth_count -= 1;
                        }
                    } else if c == '/' {
                        self.next_char();
                        if self.peek_next_char() == Some('*') {
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

    fn lex_num_literal(&mut self) -> TokenKind {
        let mut result = String::new();

        let mut base = 10;
        let mut float = false;
        let mut exp = false;
        let mut exp_sign = false;

        if let Some(c) = self.peek_next_char()
            && c == '0'
        {
            match self.peek_second_char() {
                Some('x') => {
                    self.next_char();
                    self.next_char();
                    base = 16;
                }
                Some('o') => {
                    self.next_char();
                    self.next_char();
                    base = 8;
                }
                Some('b') => {
                    self.next_char();
                    self.next_char();
                    base = 2;
                }
                _ => base = 10,
            }
        }

        while let Some(c) = self.peek_next_char()
            && (c.is_numeric() || c == '_' || c == '.' || c == 'e' || c == '-' || c == '+')
        {
            self.next_char();

            // We allow an indefinite amount of underscores in int literals (for now).
            if c == '_' {
                continue;
            }

            if c == '.' {
                if float {
                    break;
                }
                float = true;
            }

            if c == 'e' {
                if exp {
                    break;
                }
                if let Some(c) = self.peek_next_char()
                    && !c.is_numeric()
                    && c != '-'
                    && c != '+'
                {
                    return TokenKind::LexError;
                }
                exp = true;
            }

            if c == '-' || c == '+' {
                if exp_sign {
                    break;
                }
                if let Some(c) = self.peek_next_char()
                    && !c.is_numeric()
                {
                    return TokenKind::LexError;
                }
                exp_sign = true;
            }

            result.push(c);
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

    fn lex_str_literal(&mut self) -> TokenKind {
        let mut result = String::new();

        while let Some(c) = self.next_char()
            && c != '"'
        {
            match c {
                '\\' => match self.next_char() {
                    Some('n') => result.push('\n'),
                    Some('r') => result.push('\r'),
                    Some('t') => result.push('\t'),
                    Some('\\') => result.push('\\'),
                    Some('"') => result.push('"'),
                    Some('x') => {
                        let mut v: u8 = 0;
                        for i in (0..=1).rev() {
                            match self.next_char() {
                                Some(c) if c.is_ascii_hexdigit() => {
                                    v |= ascii_to_hex_byte(c).unwrap_or(0) << (4 * i);
                                }
                                _ => return TokenKind::LexError,
                            }
                        }
                        result.push(v as char);
                    }
                    Some('u') | Some('U') => {
                        todo!("Unicode code point support in string literals")
                    }
                    _ => {
                        return TokenKind::LexError;
                    }
                },
                _ => result.push(c),
            }
        }

        TokenKind::StrLiteral(result)
    }

    fn lex_char_literal(&mut self) -> TokenKind {
        let mut result = 0_u8;

        while let Some(c) = self.next_char()
            && c != '\''
        {
            match c {
                '\\' => match self.next_char() {
                    Some('n') => result = b'\n',
                    Some('r') => result = b'\r',
                    Some('t') => result = b'\t',
                    Some('\\') => result = b'\\',
                    Some('\'') => result = b'"',
                    Some('x') => {
                        let mut v: u8 = 0;
                        for i in (0..=1).rev() {
                            match self.next_char() {
                                Some(c) if c.is_ascii_hexdigit() => {
                                    v |= ascii_to_hex_byte(c).unwrap_or(0) << (4 * i);
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
                _ => result = c as u8,
            }
        }

        TokenKind::CharLiteral(result)
    }
}

fn ascii_to_hex_byte(c: char) -> Option<u8> {
    let c = c as u8;
    match c {
        b'0'..=b'9' => Some(c - b'0'),
        b'A'..=b'Z' => Some(c - b'A' + 10),
        b'a'..=b'z' => Some(c - b'a' + 10),
        _ => None,
    }
}
