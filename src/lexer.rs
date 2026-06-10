use crate::token::{self, TokenKind as TK, ValueKind as VK};

use std::{
    io::{self, IsTerminal},
    path, str,
};

#[derive(Debug)]
pub struct Lexer<'a> {
    // Read-only data
    pub input: &'a str,
    pub input_path: &'a path::Path,

    // Lexing state
    input_iter: str::Chars<'a>,
    current_character_index: usize,
    current_line_index: usize,
    current_line_begin_offset: usize,

    tokens_buffer: Vec<token::Token<'a>>,
    current_token_offset: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, input_path: &'a path::Path) -> Self {
        Self {
            input,
            input_path,

            input_iter: input.chars(),
            current_character_index: 0,
            current_line_index: 0,
            current_line_begin_offset: 0,

            tokens_buffer: Vec::with_capacity(128),
            current_token_offset: 0,
        }
    }

    pub fn next_token(&mut self) -> token::Token<'a> {
        if self.current_token_offset >= self.tokens_buffer.len() {
            self.advance_token();
        }
        let result = self.tokens_buffer[self.current_token_offset].clone();
        self.current_token_offset += 1;
        result
    }

    pub fn peek_token(&mut self, lookahead_index: usize) -> token::Token<'a> {
        while self.current_token_offset + lookahead_index >= self.tokens_buffer.len() {
            self.advance_token();
        }
        self.tokens_buffer[self.current_token_offset + lookahead_index].clone()
    }

    pub fn peek_next_token(&mut self) -> token::Token<'a> {
        self.peek_token(0)
    }

    pub fn last_token(&self) -> token::Token<'a> {
        self.tokens_buffer[self.current_token_offset - 1].clone()
    }

    pub fn report_error_at(&self, loc: token::Location, msg: &str) {
        assert!(
            (if loc.l0 == loc.l1 {
                loc.c0 <= loc.c1
            } else {
                true
            }) && (loc.l0 <= loc.l1),
            "Invalid location found while reporting error, check call site."
        );

        let is_tty = io::stdout().is_terminal(); // To ensure ansi escape codes are supported.
        let (ansi_code_cyan, ansi_code_red, ansi_code_reset) = if is_tty {
            ("\x1b[36m", "\x1b[31m", "\x1b[0m")
        } else {
            ("", "", "")
        };

        let padding = format!("{}", loc.l1 + 2).len();

        eprintln!(
            "{}:{}:{}: error: {}",
            loc.input_path.display(),
            loc.l0 + 1,
            loc.c0 + 1,
            msg
        );

        // Previous line
        eprintln!();
        if loc.l0 > 0
            && let Some(previous_line) = &self.input.lines().nth(loc.l0 - 1)
        {
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
        // It's okay to panic if any of the unwraps fail here.
        // That would mean there is a bug somewhere else.
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
        eprintln!();
    }

    fn advance_token(&mut self) {
        self.eat_whitespaces();
        self.eat_comments();

        let start = self.current_character_index;
        let l0 = self.current_line_index;
        let c0 = self.current_character_index - self.current_line_begin_offset;

        let mut token_val = VK::None;
        let token_kind = match self.peek_next_character() {
            Some(c) if c.is_alphabetic() || c == '_' => {
                let mut ident_or_keyword = String::from(self.next_character().unwrap());

                while let Some(c) = self.peek_next_character()
                    && (c.is_alphanumeric() || c == '_')
                {
                    ident_or_keyword.push(self.next_character().unwrap());
                }

                match &ident_or_keyword[..] {
                    // Booleans
                    "true" => {
                        token_val = VK::Bool(true);
                        TK::Bool
                    }
                    "false" => {
                        token_val = VK::Bool(false);
                        TK::Bool
                    }

                    // Floats
                    "inf" => {
                        token_val = VK::Float(f64::INFINITY);
                        TK::Float
                    }
                    "nan" => {
                        token_val = VK::Float(f64::NAN);
                        TK::Float
                    }

                    // Keywords
                    "fn" => TK::Fn,
                    "return" => TK::Return,
                    "for" => TK::For,
                    "break" => TK::Break,
                    "continue" => TK::Continue,
                    "if" => TK::If,
                    "else" => TK::Else,

                    _ => {
                        token_val = VK::Ident(ident_or_keyword);
                        TK::Ident
                    }
                }
            }
            Some(c) if c.is_ascii_digit() => {
                let (k, v) = self.lex_numeric_literal();
                token_val = v;
                k
            }
            Some('+') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TK::PlusEquals
                    }
                    _ => TK::Plus,
                }
            }
            Some('-') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TK::MinusEquals
                    }
                    Some('>') => {
                        self.next_character();
                        TK::RightArrow
                    }
                    _ => TK::Minus,
                }
            }
            Some('*') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TK::TimesEquals
                    }
                    Some('.') => {
                        if let Some(c) = self.peek_character(1)
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
                            TK::Star
                        } else {
                            self.next_character();
                            TK::Ref
                        }
                    }
                    _ => TK::Star,
                }
            }
            Some('/') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TK::DivEquals
                    }
                    _ => TK::Div,
                }
            }
            Some('%') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TK::ModEquals
                    }
                    _ => TK::Mod,
                }
            }
            Some('&') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TK::BitwiseAndEquals
                    }
                    Some('&') => {
                        self.next_character();
                        TK::LogicalAnd
                    }
                    _ => TK::BitwiseAnd,
                }
            }
            Some('|') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TK::BitwiseOrEquals
                    }
                    Some('|') => {
                        self.next_character();
                        TK::LogicalOr
                    }
                    _ => TK::BitwiseOr,
                }
            }
            Some('~') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TK::BitwiseNotEquals
                    }
                    _ => TK::BitwiseNot,
                }
            }
            Some('^') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TK::BitwiseXorEquals
                    }
                    _ => TK::BitwiseXor,
                }
            }
            Some('<') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TK::LessOrEqual
                    }
                    Some('<') => {
                        self.next_character();
                        match self.peek_next_character() {
                            Some('=') => {
                                self.next_character();
                                TK::BitwiseShlEquals
                            }
                            _ => TK::BitwiseShl,
                        }
                    }
                    _ => TK::LessThan,
                }
            }
            Some('>') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TK::GreaterOrEqual
                    }
                    Some('>') => {
                        self.next_character();
                        match self.peek_next_character() {
                            Some('=') => {
                                self.next_character();
                                TK::BitwiseShrEquals
                            }
                            _ => TK::BitwiseShr,
                        }
                    }
                    _ => TK::GreaterThan,
                }
            }
            Some('=') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TK::IsEqual
                    }
                    _ => TK::Equals,
                }
            }
            Some('!') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TK::IsNotEqual
                    }
                    _ => TK::LogicalNot,
                }
            }
            Some('.') => match self.peek_character(1) {
                Some('.') => {
                    self.next_character();
                    self.next_character();
                    TK::DoubleDot
                }
                Some('*') => {
                    self.next_character();
                    self.next_character();
                    TK::Deref
                }
                Some(c) if c.is_numeric() => {
                    let (k, v) = self.lex_numeric_literal();
                    token_val = v;
                    k
                }
                _ => {
                    self.next_character();
                    TK::Dot
                }
            },
            Some(',') => {
                self.next_character();
                TK::Comma
            }
            Some(':') => {
                self.next_character();
                TK::Colon
            }
            Some(';') => {
                self.next_character();
                TK::Semicolon
            }
            Some('(') => {
                self.next_character();
                TK::OpenParen
            }
            Some(')') => {
                self.next_character();
                TK::CloseParen
            }
            Some('{') => {
                self.next_character();
                TK::OpenCurly
            }
            Some('}') => {
                self.next_character();
                TK::CloseCurly
            }
            Some('[') => {
                self.next_character();
                TK::OpenBracket
            }
            Some(']') => {
                self.next_character();
                TK::CloseBracket
            }
            Some('\'') => {
                self.next_character();
                let (k, v) = self.lex_character_literal();
                token_val = v;
                k
            }
            Some('"') => {
                self.next_character();
                let (k, v) = self.lex_string_literal();
                token_val = v;
                k
            }
            None => TK::Eof,
            _ => {
                token_val = VK::LexError("Illegal token detected.".to_string());
                TK::Invalid
            }
        };

        let l1 = self.current_line_index;

        // Do something better!
        let c1 = if self.current_character_index > self.current_line_begin_offset {
            self.current_character_index - self.current_line_begin_offset - 1
        } else {
            0
        };

        let end = self.current_character_index;
        if l0 == l1 {
            assert!(c0 <= c1);
        }
        assert!(l0 <= l1);

        self.tokens_buffer.push(token::Token {
            kind: token_kind,
            val: token_val,
            loc: token::Location {
                input_path: self.input_path,
                l0,
                c0,
                l1,
                c1,
            },
            lexeme: &self.input[start..end],
        });
    }

    fn lex_numeric_literal(&mut self) -> (TK, VK) {
        let mut result = String::new();

        let mut base = 10;
        let mut float = false;
        let mut exp = false;
        let mut exp_sign = false;

        if let Some(c) = self.peek_next_character()
            && c == '0'
        {
            match self.peek_character(1) {
                Some('x') => {
                    self.next_character();
                    self.next_character();
                    base = 16;
                }
                Some('o') => {
                    self.next_character();
                    self.next_character();
                    base = 8;
                }
                Some('b') => {
                    self.next_character();
                    self.next_character();
                    base = 2;
                }
                _ => base = 10,
            }
        }

        while let Some(c) = self.peek_next_character()
            && (c.is_numeric() || c == '_' || c == '.' || c == 'e' || c == '-' || c == '+')
        {
            self.next_character();

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
                if let Some(c) = self.peek_next_character()
                    && !c.is_ascii_digit()
                    && c != '-'
                    && c != '+'
                {
                    return (TK::Invalid, VK::LexError("Must have at least one optionally signed digit as exponent (e.g., '1.32e5', '0.9e-5', '4.5e+5').".to_string()));
                }
                exp = true;
            }

            if c == '-' || c == '+' {
                if exp_sign {
                    break;
                }
                if let Some(c) = self.peek_next_character()
                    && !c.is_ascii_digit()
                {
                    return (TK::Invalid, VK::LexError("Must have at least one digit as exponent after sign (e.g., '0.9e-5', '4.5e+5').".to_string()));
                }
                exp_sign = true;
            }

            result.push(c);
        }

        if float {
            if base != 10 {
                return (
                    TK::Invalid,
                    VK::LexError("Only base-10 float literals are supported.".to_string()),
                );
            }
            match result.parse::<f64>() {
                Ok(v) => (TK::Float, VK::Float(v)),
                Err(e) => (
                    TK::Invalid,
                    VK::LexError(format!(
                        "Error while parsing float literal: `{}`: {}",
                        &result, e
                    )),
                ),
            }
        } else {
            match u128::from_str_radix(&result, base) {
                Ok(v) => (TK::Int, VK::Int(v)),
                Err(e) => (
                    TK::Invalid,
                    VK::LexError(
                        format!("Error while parsing integer literal: `{}`: {}", &result, e)
                            .to_string(),
                    ),
                ),
            }
        }
    }

    fn lex_string_literal(&mut self) -> (TK, VK) {
        let mut result = String::new();

        loop {
            match self.next_character() {
                None => {
                    return (
                        TK::Invalid,
                        VK::LexError("Unclosed string literal (unexpected EOF).".to_string()),
                    );
                }
                Some('"') => break,
                Some('\\') => match self.next_character() {
                    Some('n') => result.push('\n'),
                    Some('r') => result.push('\r'),
                    Some('t') => result.push('\t'),
                    Some('\\') => result.push('\\'),
                    Some('"') => result.push('"'),
                    Some('x') => match self.make_possible_character_from_hex_digits(2) {
                        Ok(c) => result.push(c),
                        Err(()) => {
                            return (
                                TK::Invalid,
                                VK::LexError(
                                    "Must have exactly 2 hex digits after '\\x'.".to_string(),
                                ),
                            );
                        }
                    },
                    Some('u') => match self.make_possible_character_from_hex_digits(4) {
                        Ok(c) => result.push(c),
                        Err(()) => {
                            return (
                                TK::Invalid,
                                VK::LexError(
                                    "Must have exactly 4 hex digits after '\\u'.".to_string(),
                                ),
                            );
                        }
                    },
                    Some('U') => match self.make_possible_character_from_hex_digits(8) {
                        Ok(c) => result.push(c),
                        Err(()) => {
                            return (
                                TK::Invalid,
                                VK::LexError(
                                    "Must have exactly 8 hex digits after '\\U'.".to_string(),
                                ),
                            );
                        }
                    },
                    Some(c) => {
                        return (
                            TK::Invalid,
                            VK::LexError(format!("Invalid escape code in string literal: `{}`", c)),
                        );
                    }
                    None => {
                        return (
                            TK::Invalid,
                            VK::LexError("Unclosed string literal (unexpected EOF).".to_string()),
                        );
                    }
                },
                Some(c) => result.push(c),
            }
        }

        (TK::Str, VK::Str(result))
    }

    fn lex_character_literal(&mut self) -> (TK, VK) {
        let result: u8 = match self.next_character() {
            None | Some('\'') => {
                return (
                    TK::Invalid,
                    VK::LexError("Empty character literal.".to_string()),
                );
            }
            Some('\\') => match self.next_character() {
                Some('n') => b'\n',
                Some('r') => b'\r',
                Some('t') => b'\t',
                Some('\\') => b'\\',
                Some('\'') => b'\'',
                Some('x') => match self.make_possible_character_from_hex_digits(2) {
                    Ok(c) => c as u8,
                    Err(()) => {
                        return (
                            TK::Invalid,
                            VK::LexError("Must have exactly 2 hex digits after '\\x'.".to_string()),
                        );
                    }
                },
                Some(c) => {
                    return (
                        TK::Invalid,
                        VK::LexError(format!("Invalid escape code in character literal: `{}`", c)),
                    );
                }
                None => {
                    return (
                        TK::Invalid,
                        VK::LexError("Unexpected EOF in character literal.".to_string()),
                    );
                }
            },
            Some(c) => c as u8,
        };

        match self.next_character() {
            Some('\'') => (TK::Char, VK::Char(result)),
            None => (
                TK::Invalid,
                VK::LexError("Unclosed character literal (unexpected EOF).".to_string()),
            ),
            Some(_) => (
                TK::Invalid,
                VK::LexError("Character literal contains more than one character.".to_string()),
            ),
        }
    }

    fn make_possible_character_from_hex_digits(&mut self, n: usize) -> Result<char, ()> {
        let mut result: u32 = 0;
        for i in (0..=n - 1).rev() {
            match self.next_character() {
                Some(c) if c.is_ascii_hexdigit() => {
                    result |=
                        (get_hex_value_from_character(c).unwrap_or(0) as u32) << (4 * i as u32);
                }
                _ => return Err(()),
            }
        }
        Ok(char::from_u32(result).unwrap_or('\0'))
    }

    fn next_character(&mut self) -> Option<char> {
        if let Some(c) = self.peek_next_character() {
            self.current_character_index += 1;
            if c == '\n' {
                self.current_line_index += 1;
                self.current_line_begin_offset = self.current_character_index;
            }
        }
        self.input_iter.next()
    }

    fn peek_next_character(&self) -> Option<char> {
        self.peek_character(0)
    }

    fn peek_character(&self, lookahead_index: usize) -> Option<char> {
        let mut iter = self.input_iter.clone();
        iter.nth(lookahead_index)
    }

    fn eat_whitespaces(&mut self) {
        while let Some(c) = self.peek_next_character()
            && c.is_whitespace()
        {
            self.next_character();
        }
    }

    fn eat_comments(&mut self) {
        loop {
            if self.input_iter.as_str().starts_with("//") {
                // Eat the leading `//`.
                self.next_character();
                self.next_character();

                while let Some(c) = self.peek_next_character()
                    && c != '\n'
                {
                    self.next_character();
                }
            } else if self.input_iter.as_str().starts_with("/*") {
                // Eat the leading `/*`.
                self.next_character();
                self.next_character();

                let mut depth_count = 1;
                while let Some(c) = self.peek_next_character()
                    && depth_count > 0
                {
                    if c == '*' {
                        self.next_character();
                        if self.peek_next_character() == Some('/') {
                            depth_count -= 1;
                        }
                    } else if c == '/' {
                        self.next_character();
                        if self.peek_next_character() == Some('*') {
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
}

fn get_hex_value_from_character(c: char) -> Option<u8> {
    let c = c as u8; // Cast to ascii value.
    match c {
        b'0'..=b'9' => Some(c - b'0'),
        b'A'..=b'Z' => Some(c - b'A' + 10),
        b'a'..=b'z' => Some(c - b'a' + 10),
        _ => None,
    }
}
