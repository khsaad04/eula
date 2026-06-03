use std::{
    fmt,
    io::{self, IsTerminal},
    path, str,
};

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub loc: Location<'a>,
    pub lexeme: &'a str,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;
        match self.kind {
            Ident(_) | IntLit(_) | FloatLit(_) | BoolLit(_) | Fn | Return | For | Break
            | Continue | If | Else | U0 | U8 | U16 | U32 | U64 | U128 | I0 | I8 | I16 | I32
            | I64 | I128 | F32 | F64 => write!(f, "{} `{}`", self.kind, self.lexeme),
            StrLit(_) | CharLit(_) => write!(f, "{} {}", self.kind, self.lexeme),
            Eof | LexError { .. } => write!(f, "{}", self.kind),
            _ => write!(f, "`{}`", self.kind),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Literals
    Ident(String),
    StrLit(String),
    CharLit(u8),
    IntLit(u128),
    FloatLit(f64),
    BoolLit(bool),

    // Keywords
    Fn,
    Return,
    For,
    Break,
    Continue,
    If,
    Else,

    U0, // alias: void
    U8, // alias: char
    U16,
    U32,
    U64,
    U128,
    I0, // alias: bool
    I8,
    I16,
    I32, // alias: int
    I64,
    I128,
    F32, // alias: float
    F64,

    // Operators
    Plus,  // +
    Minus, // -
    Star,  // *
    Div,   // /
    Mod,   // %

    AddEquals,   // +=
    SubEquals,   // -=
    TimesEquals, // *=
    DivEquals,   // /=
    ModEquals,   // %=

    BitwiseAnd, // &
    BitwiseOr,  // |
    BitwiseNot, // ~
    BitwiseXor, // ^
    BitwiseShl, // <<
    BitwiseShr, // >>

    BitwiseAndEquals, // &=
    BitwiseOrEquals,  // |=
    BitwiseNotEquals, // ~=
    BitwiseXorEquals, // ^=
    BitwiseShlEquals, // <<=
    BitwiseShrEquals, // >>=

    LessThan,      // <
    GreaterThan,   // >
    LessEquals,    // <=
    GreaterEquals, // >=
    IsEqual,       // ==
    IsNotEqual,    // !=
    LogicalAnd,    // &&
    LogicalOr,     // ||
    LogicalNot,    // !

    Equals,    // =
    Dot,       // .
    DoubleDot, // ..
    Comma,     // ,
    Colon,     // :
    Semicolon, // ;

    OpenParen,    // (
    CloseParen,   // )
    OpenCurly,    // {
    CloseCurly,   // }
    OpenBracket,  // [
    CloseBracket, // ]

    RightArrow, // ->
    Ref,        // *.
    Deref,      // .*

    LexError { msg: String },
    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;
        match self {
            // Literals
            Ident(_) => write!(f, "identifier"),
            StrLit(_) => write!(f, "string"),
            CharLit(_) => write!(f, "character"),
            IntLit(_) => write!(f, "integer"),
            FloatLit(_) => write!(f, "float"),
            BoolLit(_) => write!(f, "boolean"),

            // Keywords
            Fn | Return | For | Break | Continue | If | Else | U0 | U8 | U16 | U32 | U64 | U128
            | I0 | I8 | I16 | I32 | I64 | I128 | F32 | F64 => write!(f, "keyword"),

            LexError { msg } => write!(f, "lexical error: {}", msg),
            Eof => write!(f, "end of file"),

            // Operators
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Star => write!(f, "*"),
            Div => write!(f, "/"),
            Mod => write!(f, "%"),

            AddEquals => write!(f, "+="),
            SubEquals => write!(f, "-="),
            TimesEquals => write!(f, "*="),
            DivEquals => write!(f, "/="),
            ModEquals => write!(f, "%="),

            BitwiseAnd => write!(f, "&"),
            BitwiseOr => write!(f, "|"),
            BitwiseNot => write!(f, "~"),
            BitwiseXor => write!(f, "^"),
            BitwiseShl => write!(f, "<<"),
            BitwiseShr => write!(f, ">>"),

            BitwiseAndEquals => write!(f, "&="),
            BitwiseOrEquals => write!(f, "|="),
            BitwiseNotEquals => write!(f, "~="),
            BitwiseXorEquals => write!(f, "^="),
            BitwiseShlEquals => write!(f, "<<="),
            BitwiseShrEquals => write!(f, ">>="),

            LessThan => write!(f, "<"),
            GreaterThan => write!(f, ">"),
            LessEquals => write!(f, "<="),
            GreaterEquals => write!(f, ">="),
            IsEqual => write!(f, "=="),
            IsNotEqual => write!(f, "!="),
            LogicalAnd => write!(f, "&&"),
            LogicalOr => write!(f, "||"),
            LogicalNot => write!(f, "!"),

            Equals => write!(f, "="),
            Dot => write!(f, "."),
            DoubleDot => write!(f, ".."),
            Comma => write!(f, ","),
            Colon => write!(f, ":"),
            Semicolon => write!(f, ";"),

            OpenParen => write!(f, "("),
            CloseParen => write!(f, ")"),
            OpenCurly => write!(f, "{{"),
            CloseCurly => write!(f, "}}"),
            OpenBracket => write!(f, "["),
            CloseBracket => write!(f, "]"),

            RightArrow => write!(f, "->"),
            Ref => write!(f, "*."),
            Deref => write!(f, ".*"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Location<'a> {
    pub input_path: &'a path::Path,

    pub l0: usize,
    pub c0: usize,

    pub l1: usize,
    pub c1: usize,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    // Read-only data
    input: &'a str,
    input_path: &'a path::Path,

    // Lexing state
    input_iter: str::Chars<'a>,
    current_character_index: usize,
    current_line_index: usize,
    current_line_begin_offset: usize,

    tokens_buffer: Vec<Token<'a>>,
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

            // The initial capacity is just a heuristic and is prone to change.
            tokens_buffer: Vec::with_capacity(128),
            current_token_offset: 0,
        }
    }

    pub fn next_token(&mut self) -> Token<'a> {
        if self.current_token_offset >= self.tokens_buffer.len() {
            self.advance_token();
        }
        let result = self.tokens_buffer[self.current_token_offset].clone();
        self.current_token_offset += 1;
        result
    }

    pub fn peek_token(&mut self, lookahead_index: usize) -> Token<'a> {
        while self.current_token_offset + lookahead_index >= self.tokens_buffer.len() {
            self.advance_token();
        }
        self.tokens_buffer[self.current_token_offset + lookahead_index].clone()
    }

    pub fn peek_next_token(&mut self) -> Token<'a> {
        self.peek_token(0)
    }

    pub fn last_token(&self) -> Token<'a> {
        self.tokens_buffer[self.current_token_offset - 1].clone()
    }

    pub fn report_error_at(&self, loc: Location, msg: &str) {
        if loc.l0 == loc.l1 {
            assert!(
                loc.c0 <= loc.c1,
                "Invalid location found while reporting error, check call site."
            );
        }
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
        let is_tty = io::stdout().is_terminal(); // To ensure ansi escape codes are supported.

        let (ansi_code_cyan, ansi_code_red, ansi_code_reset) = if is_tty {
            ("\x1b[36m", "\x1b[31m", "\x1b[0m")
        } else {
            ("", "", "")
        };

        let padding = format!("{}", loc.l1 + 2).len();

        // Previous line
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

        // @Note: It's okay to panic if any of the unwraps fail here.
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
                    "true" => TokenKind::BoolLit(true),
                    "false" => TokenKind::BoolLit(false),

                    // Floats
                    "inf" => TokenKind::FloatLit(f64::INFINITY),
                    "nan" => TokenKind::FloatLit(f64::NAN),

                    // Keywords
                    "fn" => TokenKind::Fn,
                    "return" => TokenKind::Return,
                    "for" => TokenKind::For,
                    "break" => TokenKind::Break,
                    "continue" => TokenKind::Continue,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,

                    // Types
                    "u0" | "void" => TokenKind::U0,
                    "u8" | "char" => TokenKind::U8,
                    "u16" => TokenKind::U16,
                    "u32" => TokenKind::U32,
                    "u64" => TokenKind::U64,
                    "u128" => TokenKind::U128,
                    "i0" | "bool" => TokenKind::I0,
                    "i8" => TokenKind::I8,
                    "i16" => TokenKind::I16,
                    "i32" | "int" => TokenKind::I32,
                    "i64" => TokenKind::I64,
                    "i128" => TokenKind::I128,
                    "f32" | "float" => TokenKind::F32,
                    "f64" => TokenKind::F64,

                    _ => TokenKind::Ident(ident_or_keyword),
                }
            }
            Some(c) if c.is_ascii_digit() => self.lex_numeric_literal(),
            Some('+') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TokenKind::AddEquals
                    }
                    _ => TokenKind::Plus,
                }
            }
            Some('-') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TokenKind::SubEquals
                    }
                    Some('>') => {
                        self.next_character();
                        TokenKind::RightArrow
                    }
                    _ => TokenKind::Minus,
                }
            }
            Some('*') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TokenKind::TimesEquals
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
                            TokenKind::Star
                        } else {
                            self.next_character();
                            TokenKind::Ref
                        }
                    }
                    _ => TokenKind::Star,
                }
            }
            Some('/') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TokenKind::DivEquals
                    }
                    _ => TokenKind::Div,
                }
            }
            Some('%') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TokenKind::ModEquals
                    }
                    _ => TokenKind::Mod,
                }
            }
            Some('&') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TokenKind::BitwiseAndEquals
                    }
                    Some('&') => {
                        self.next_character();
                        TokenKind::LogicalAnd
                    }
                    _ => TokenKind::BitwiseAnd,
                }
            }
            Some('|') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TokenKind::BitwiseOrEquals
                    }
                    Some('|') => {
                        self.next_character();
                        TokenKind::LogicalOr
                    }
                    _ => TokenKind::BitwiseOr,
                }
            }
            Some('~') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TokenKind::BitwiseNotEquals
                    }
                    _ => TokenKind::BitwiseNot,
                }
            }
            Some('^') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TokenKind::BitwiseXorEquals
                    }
                    _ => TokenKind::BitwiseXor,
                }
            }
            Some('<') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TokenKind::LessEquals
                    }
                    Some('<') => {
                        self.next_character();
                        match self.peek_next_character() {
                            Some('=') => {
                                self.next_character();
                                TokenKind::BitwiseShlEquals
                            }
                            _ => TokenKind::BitwiseShl,
                        }
                    }
                    _ => TokenKind::LessThan,
                }
            }
            Some('>') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TokenKind::GreaterEquals
                    }
                    Some('>') => {
                        self.next_character();
                        match self.peek_next_character() {
                            Some('=') => {
                                self.next_character();
                                TokenKind::BitwiseShrEquals
                            }
                            _ => TokenKind::BitwiseShr,
                        }
                    }
                    _ => TokenKind::GreaterThan,
                }
            }
            Some('=') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TokenKind::IsEqual
                    }
                    _ => TokenKind::Equals,
                }
            }
            Some('!') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        TokenKind::IsNotEqual
                    }
                    _ => TokenKind::LogicalNot,
                }
            }
            Some('.') => match self.peek_character(1) {
                Some('.') => {
                    self.next_character();
                    self.next_character();
                    TokenKind::DoubleDot
                }
                Some('*') => {
                    self.next_character();
                    self.next_character();
                    TokenKind::Deref
                }
                Some(c) if c.is_numeric() => self.lex_numeric_literal(),
                _ => {
                    self.next_character();
                    TokenKind::Dot
                }
            },
            Some(',') => {
                self.next_character();
                TokenKind::Comma
            }
            Some(':') => {
                self.next_character();
                TokenKind::Colon
            }
            Some(';') => {
                self.next_character();
                TokenKind::Semicolon
            }
            Some('(') => {
                self.next_character();
                TokenKind::OpenParen
            }
            Some(')') => {
                self.next_character();
                TokenKind::CloseParen
            }
            Some('{') => {
                self.next_character();
                TokenKind::OpenCurly
            }
            Some('}') => {
                self.next_character();
                TokenKind::CloseCurly
            }
            Some('[') => {
                self.next_character();
                TokenKind::OpenBracket
            }
            Some(']') => {
                self.next_character();
                TokenKind::CloseBracket
            }
            Some('\'') => {
                self.next_character();
                self.lex_character_literal()
            }
            Some('"') => {
                self.next_character();
                self.lex_string_literal()
            }
            None => TokenKind::Eof,
            _ => TokenKind::LexError {
                msg: "Illegal token detected.".to_string(),
            },
        };

        let l1 = self.current_line_index;

        // @Cleanup: Do something better!
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

        self.tokens_buffer.push(Token {
            kind: token_kind,
            loc: Location {
                input_path: self.input_path,
                l0,
                c0,
                l1,
                c1,
            },
            lexeme: &self.input[start..end],
        });
    }

    fn lex_numeric_literal(&mut self) -> TokenKind {
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
                    return TokenKind::LexError{msg:"Must have at least one optionally signed digit as exponent (e.g., '1.32e5', '0.9e-5', '4.5e+5').".to_string()};
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
                    return TokenKind::LexError{msg:"Must have at least one digit as exponent after sign (e.g., '0.9e-5', '4.5e+5').".to_string()};
                }
                exp_sign = true;
            }

            result.push(c);
        }

        if float {
            if base != 10 {
                return TokenKind::LexError {
                    msg: "Only base-10 float literals are supported.".to_string(),
                };
            }
            match result.parse::<f64>() {
                Ok(v) => TokenKind::FloatLit(v),
                Err(e) => TokenKind::LexError {
                    msg: format!("Error while parsing float literal: `{}`: {}", &result, e)
                        .to_string(),
                },
            }
        } else {
            match u128::from_str_radix(&result, base) {
                Ok(v) => TokenKind::IntLit(v),
                Err(e) => TokenKind::LexError {
                    msg: format!("Error while parsing integer literal: `{}`: {}", &result, e)
                        .to_string(),
                },
            }
        }
    }

    fn lex_string_literal(&mut self) -> TokenKind {
        let mut result = String::new();

        loop {
            match self.next_character() {
                None => {
                    return TokenKind::LexError {
                        msg: "Unclosed string literal (unexpected EOF).".to_string(),
                    };
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
                            return TokenKind::LexError {
                                msg: "Must have exactly 2 hex digits after '\\x'.".to_string(),
                            };
                        }
                    },
                    Some('u') => match self.make_possible_character_from_hex_digits(4) {
                        Ok(c) => result.push(c),
                        Err(()) => {
                            return TokenKind::LexError {
                                msg: "Must have exactly 4 hex digits after '\\u'.".to_string(),
                            };
                        }
                    },
                    Some('U') => match self.make_possible_character_from_hex_digits(8) {
                        Ok(c) => result.push(c),
                        Err(()) => {
                            return TokenKind::LexError {
                                msg: "Must have exactly 8 hex digits after '\\U'.".to_string(),
                            };
                        }
                    },
                    Some(c) => {
                        return TokenKind::LexError {
                            msg: format!("Invalid escape code in string literal: `{}`", c),
                        };
                    }
                    None => {
                        return TokenKind::LexError {
                            msg: "Unclosed string literal (unexpected EOF).".to_string(),
                        };
                    }
                },
                Some(c) => result.push(c),
            }
        }

        TokenKind::StrLit(result)
    }

    fn lex_character_literal(&mut self) -> TokenKind {
        let result: u8 = match self.next_character() {
            None | Some('\'') => {
                return TokenKind::LexError {
                    msg: "Empty character literal.".to_string(),
                };
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
                        return TokenKind::LexError {
                            msg: "Must have exactly 2 hex digits after '\\x'.".to_string(),
                        };
                    }
                },
                Some(c) => {
                    return TokenKind::LexError {
                        msg: format!("Invalid escape code in character literal: `{}`", c),
                    };
                }
                None => {
                    return TokenKind::LexError {
                        msg: "Unexpected EOF in character literal.".to_string(),
                    };
                }
            },
            Some(c) => c as u8,
        };

        match self.next_character() {
            Some('\'') => TokenKind::CharLit(result),
            None => TokenKind::LexError {
                msg: "Unclosed character literal (unexpected EOF).".to_string(),
            },
            Some(_) => TokenKind::LexError {
                msg: "Character literal contains more than one character.".to_string(),
            },
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
                // eat the leading `//`
                self.next_character();
                self.next_character();

                while let Some(c) = self.peek_next_character()
                    && c != '\n'
                {
                    self.next_character();
                }
            } else if self.input_iter.as_str().starts_with("/*") {
                // eat the leading `/*`
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
