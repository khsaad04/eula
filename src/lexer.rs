use std::{
    fmt,
    io::{self, IsTerminal},
    path, str,
};

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub value: ValueKind,
    pub span: Span,
    pub lexeme: &'a str,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            TokenKind::Invalid | TokenKind::Eof => write!(f, "{}", self.kind),
            // Literals and keywords
            TokenKind::Ident
            | TokenKind::Int
            | TokenKind::Float
            | TokenKind::Bool
            | TokenKind::Fn
            | TokenKind::Return
            | TokenKind::For
            | TokenKind::Break
            | TokenKind::Continue
            | TokenKind::If
            | TokenKind::Else => write!(f, "{} `{}`", self.kind, self.lexeme),
            TokenKind::Str | TokenKind::Char => write!(f, "{} {}", self.kind, self.lexeme), // These look kind of weird enclosed in backticks
            // Operators and punctuations
            _ => write!(f, "`{}`", self.kind),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    Invalid,
    Eof,

    // Literals
    Ident,
    Str,
    Char,
    Int,
    Float,
    Bool,

    // Keywords
    Fn,
    Return,

    For,
    Break,
    Continue,

    If,
    Else,

    // Operators and punctuations
    Plus,  // +
    Minus, // -
    Star,  // *
    Div,   // /
    Mod,   // %

    PlusEquals,  // +=
    MinusEquals, // -=
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

    LessThan,       // <
    GreaterThan,    // >
    LessOrEqual,    // <=
    GreaterOrEqual, // >=
    IsEqual,        // ==
    IsNotEqual,     // !=
    LogicalAnd,     // &&
    LogicalOr,      // ||
    LogicalNot,     // !

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
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Invalid => write!(f, "an invalid token"),
            Self::Eof => write!(f, "end of file"),

            Self::Ident => write!(f, "an identifier"),
            Self::Str => write!(f, "a string literal"),
            Self::Char => write!(f, "a character literal"),
            Self::Int => write!(f, "an integer literal"),
            Self::Float => write!(f, "a float literal"),
            Self::Bool => write!(f, "a boolean literal"),

            Self::Fn
            | Self::Return
            | Self::For
            | Self::Break
            | Self::Continue
            | Self::If
            | Self::Else => write!(f, "a keyword"),

            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),

            Self::PlusEquals => write!(f, "+="),
            Self::MinusEquals => write!(f, "-="),
            Self::TimesEquals => write!(f, "*="),
            Self::DivEquals => write!(f, "/="),
            Self::ModEquals => write!(f, "%="),

            Self::BitwiseAnd => write!(f, "&"),
            Self::BitwiseOr => write!(f, "|"),
            Self::BitwiseNot => write!(f, "~"),
            Self::BitwiseXor => write!(f, "^"),
            Self::BitwiseShl => write!(f, "<<"),
            Self::BitwiseShr => write!(f, ">>"),

            Self::BitwiseAndEquals => write!(f, "&="),
            Self::BitwiseOrEquals => write!(f, "|="),
            Self::BitwiseNotEquals => write!(f, "~="),
            Self::BitwiseXorEquals => write!(f, "^="),
            Self::BitwiseShlEquals => write!(f, "<<="),
            Self::BitwiseShrEquals => write!(f, ">>="),

            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessOrEqual => write!(f, "<="),
            Self::GreaterOrEqual => write!(f, ">="),
            Self::IsEqual => write!(f, "=="),
            Self::IsNotEqual => write!(f, "!="),
            Self::LogicalAnd => write!(f, "&&"),
            Self::LogicalOr => write!(f, "||"),
            Self::LogicalNot => write!(f, "!"),

            Self::Equals => write!(f, "="),
            Self::Dot => write!(f, "."),
            Self::DoubleDot => write!(f, ".."),
            Self::Comma => write!(f, ","),
            Self::Colon => write!(f, ":"),
            Self::Semicolon => write!(f, ";"),

            Self::OpenParen => write!(f, "("),
            Self::CloseParen => write!(f, ")"),
            Self::OpenCurly => write!(f, "{{"),
            Self::CloseCurly => write!(f, "}}"),
            Self::OpenBracket => write!(f, "["),
            Self::CloseBracket => write!(f, "]"),

            Self::RightArrow => write!(f, "->"),
            Self::Ref => write!(f, "*."),
            Self::Deref => write!(f, ".*"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ValueKind {
    Ident(String),
    Str(String),
    Char(u8),
    Int(u128),
    Float(f64),
    Bool(bool),
    None,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Span(pub Position, pub Position); // (pos0, pos1]

#[derive(Debug, Default, Clone, Copy)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: ColumnPosition,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct ColumnPosition {
    bytes: usize,
    chars: usize,
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    // Read-only data
    pub input: &'a str,
    pub input_path: &'a path::Path,

    // Lexing state
    input_iter: str::Chars<'a>,
    pos: Position,
    last_token: Option<Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, input_path: &'a path::Path) -> Self {
        Self {
            input,
            input_path,
            input_iter: input.chars(),
            pos: Position::default(),
            last_token: None,
        }
    }

    pub fn next_token(&mut self) -> Token<'a> {
        let token = self.scan_token();
        self.last_token = Some(token.clone());
        token
    }

    pub fn peek_next_token(&mut self) -> Token<'a> {
        self.peek_token(0)
    }

    pub fn peek_token(&mut self, n: usize) -> Token<'a> {
        let mut l = self.clone();
        for _ in 0..n {
            l.scan_token();
        }
        l.scan_token()
    }

    pub fn last_token(&mut self) -> Option<Token<'a>> {
        self.last_token.clone()
    }

    pub fn get_pos(&self) -> Position {
        self.pos
    }

    pub fn get_span(&self, pos0: Position) -> Span {
        let mut pos1 = self.pos;
        // Increment `pos1.line` by 1 to make the span an exclusive range (p0, p1].
        // All the other fields of `pos1` are already exclusive.
        pos1.line += 1;
        Span(pos0, pos1)
    }

    pub fn report_error_at(&self, span: Span, message: &'a str) {
        assert!(span.0.line < span.1.line);
        if span.0.line == span.1.line - 1 {
            assert!(span.0.column.bytes <= span.1.column.bytes);
            assert!(span.0.column.chars <= span.1.column.chars);
        }

        let is_tty = io::stdout().is_terminal(); // To ensure ansi escape codes are supported.
        let (ansi_code_cyan, ansi_code_red, ansi_code_reset) = if is_tty {
            ("\x1b[36m", "\x1b[31m", "\x1b[0m")
        } else {
            ("", "", "")
        };

        let padding = format!("{}", span.1.line + 1).len();

        eprintln!(
            "{}:{}:{}: error: {}",
            self.input_path.display(),
            span.0.line + 1,
            span.0.column.chars + 1,
            message
        );

        let lines = self.input.lines().collect::<Vec<_>>();

        // Previous line
        eprintln!();
        if span.0.line > 0
            && let Some(previous_line) = lines.get(span.0.line - 1)
        {
            eprintln!(
                "{LINE_NO:>PAD$} | {CYAN}{}{RESET}",
                previous_line,
                LINE_NO = span.0.line,
                PAD = padding,
                CYAN = ansi_code_cyan,
                RESET = ansi_code_reset,
            );
        }

        //
        // Actually relevant line(s)
        //
        // It's okay to panic if any of the unwraps fail here.
        // That would mean there is a bug somewhere else.
        //
        if span.0.line == span.1.line - 1 {
            let current_line = lines.get(span.0.line).unwrap();
            eprintln!(
                "{LINE_NO:>PAD$} | {CYAN}{}{RED}{}{CYAN}{}{RESET}",
                &current_line[..span.0.column.bytes],
                &current_line[span.0.column.bytes..span.1.column.bytes],
                &current_line[span.1.column.bytes..],
                LINE_NO = span.0.line + 1,
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
                    SPACES = " ".repeat(span.0.column.bytes),
                    ARROWS = "^".repeat(span.1.column.bytes - span.0.column.bytes)
                );
            }
        } else {
            let first_line = lines.get(span.0.line).unwrap();
            eprintln!(
                "{LINE_NO:>PAD$} | {CYAN}{}{RED}{}{RESET}",
                &first_line[..span.0.column.bytes],
                &first_line[span.0.column.bytes..],
                LINE_NO = span.0.line + 1,
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
                    SPACES = " ".repeat(span.0.column.bytes),
                    ARROWS = "^".repeat(first_line.chars().count() - span.0.column.bytes)
                );
            }

            for i in 1..(span.1.line - span.0.line - 1) {
                let middle_line = lines.get(span.0.line + i).unwrap();
                eprintln!(
                    "{LINE_NO:>PAD$} | {RED}{}{RESET}",
                    &middle_line[..],
                    LINE_NO = span.0.line + i + 1,
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

            let last_line = lines.get(span.1.line - 1).unwrap();
            eprintln!(
                "{LINE_NO:>PAD$} | {RED}{}{CYAN}{}{RESET}",
                &last_line[..span.1.column.bytes],
                &last_line[span.1.column.bytes..],
                LINE_NO = span.1.line,
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
                    ARROWS = "^".repeat(span.1.column.bytes)
                );
            }
        }

        // Next line
        if let Some(next_line) = lines.get(span.1.line) {
            eprintln!(
                "{LINE_NO:>PAD$} | {CYAN}{}{RESET}",
                next_line,
                LINE_NO = span.1.line + 1,
                PAD = padding,
                CYAN = ansi_code_cyan,
                RESET = ansi_code_reset,
            );
        }
        eprintln!();
    }

    fn scan_token(&mut self) -> Token<'a> {
        self.eat_whitespaces();
        self.eat_comments();

        let pos0 = self.pos;

        match self.peek_next_character() {
            None => self.make_token(TokenKind::Eof, ValueKind::None, pos0),
            Some(c) if c.is_alphabetic() || c == '_' => self.make_ident_or_keyword(pos0),
            Some(c) if c.is_ascii_digit() => self.make_numeric_literal(pos0),
            Some('"') => self.make_string_literal(pos0),
            Some('\'') => self.make_character_literal(pos0),
            Some('+') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        self.make_operator_or_punctuation(TokenKind::PlusEquals, pos0)
                    }
                    _ => self.make_operator_or_punctuation(TokenKind::Plus, pos0),
                }
            }
            Some('-') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        self.make_operator_or_punctuation(TokenKind::MinusEquals, pos0)
                    }
                    Some('>') => {
                        self.next_character();
                        self.make_operator_or_punctuation(TokenKind::RightArrow, pos0)
                    }
                    _ => self.make_operator_or_punctuation(TokenKind::Minus, pos0),
                }
            }
            Some('*') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        self.make_operator_or_punctuation(TokenKind::TimesEquals, pos0)
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
                            self.make_operator_or_punctuation(TokenKind::Star, pos0)
                        } else {
                            self.next_character();
                            self.make_operator_or_punctuation(TokenKind::Ref, pos0)
                        }
                    }
                    _ => self.make_operator_or_punctuation(TokenKind::Star, pos0),
                }
            }
            Some('/') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        self.make_operator_or_punctuation(TokenKind::DivEquals, pos0)
                    }
                    _ => self.make_operator_or_punctuation(TokenKind::Div, pos0),
                }
            }
            Some('%') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        self.make_operator_or_punctuation(TokenKind::ModEquals, pos0)
                    }
                    _ => self.make_operator_or_punctuation(TokenKind::Mod, pos0),
                }
            }
            Some('&') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        self.make_operator_or_punctuation(TokenKind::BitwiseAndEquals, pos0)
                    }
                    Some('&') => {
                        self.next_character();
                        self.make_operator_or_punctuation(TokenKind::LogicalAnd, pos0)
                    }
                    _ => self.make_operator_or_punctuation(TokenKind::BitwiseAnd, pos0),
                }
            }
            Some('|') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        self.make_operator_or_punctuation(TokenKind::BitwiseOrEquals, pos0)
                    }
                    Some('|') => {
                        self.next_character();
                        self.make_operator_or_punctuation(TokenKind::LogicalOr, pos0)
                    }
                    _ => self.make_operator_or_punctuation(TokenKind::BitwiseOr, pos0),
                }
            }
            Some('~') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        self.make_operator_or_punctuation(TokenKind::BitwiseNotEquals, pos0)
                    }
                    _ => self.make_operator_or_punctuation(TokenKind::BitwiseNot, pos0),
                }
            }
            Some('^') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        self.make_operator_or_punctuation(TokenKind::BitwiseXorEquals, pos0)
                    }
                    _ => self.make_operator_or_punctuation(TokenKind::BitwiseXor, pos0),
                }
            }
            Some('<') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        self.make_operator_or_punctuation(TokenKind::LessOrEqual, pos0)
                    }
                    Some('<') => {
                        self.next_character();
                        match self.peek_next_character() {
                            Some('=') => {
                                self.next_character();
                                self.make_operator_or_punctuation(TokenKind::BitwiseShlEquals, pos0)
                            }
                            _ => self.make_operator_or_punctuation(TokenKind::BitwiseShl, pos0),
                        }
                    }
                    _ => self.make_operator_or_punctuation(TokenKind::LessThan, pos0),
                }
            }
            Some('>') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        self.make_operator_or_punctuation(TokenKind::GreaterOrEqual, pos0)
                    }
                    Some('>') => {
                        self.next_character();
                        match self.peek_next_character() {
                            Some('=') => {
                                self.next_character();
                                self.make_operator_or_punctuation(TokenKind::BitwiseShrEquals, pos0)
                            }
                            _ => self.make_operator_or_punctuation(TokenKind::BitwiseShr, pos0),
                        }
                    }
                    _ => self.make_operator_or_punctuation(TokenKind::GreaterThan, pos0),
                }
            }
            Some('=') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        self.make_operator_or_punctuation(TokenKind::IsEqual, pos0)
                    }
                    _ => self.make_operator_or_punctuation(TokenKind::Equals, pos0),
                }
            }
            Some('!') => {
                self.next_character();
                match self.peek_next_character() {
                    Some('=') => {
                        self.next_character();
                        self.make_operator_or_punctuation(TokenKind::IsNotEqual, pos0)
                    }
                    _ => self.make_operator_or_punctuation(TokenKind::LogicalNot, pos0),
                }
            }
            Some('.') => match self.peek_character(1) {
                Some('.') => {
                    self.next_character();
                    self.next_character();
                    self.make_operator_or_punctuation(TokenKind::DoubleDot, pos0)
                }
                Some('*') => {
                    self.next_character();
                    self.next_character();
                    self.make_operator_or_punctuation(TokenKind::Deref, pos0)
                }
                Some(c) if c.is_numeric() => self.make_numeric_literal(pos0),
                _ => {
                    self.next_character();
                    self.make_operator_or_punctuation(TokenKind::Dot, pos0)
                }
            },
            Some(',') => {
                self.next_character();
                self.make_operator_or_punctuation(TokenKind::Comma, pos0)
            }
            Some(':') => {
                self.next_character();
                self.make_operator_or_punctuation(TokenKind::Colon, pos0)
            }
            Some(';') => {
                self.next_character();
                self.make_operator_or_punctuation(TokenKind::Semicolon, pos0)
            }
            Some('(') => {
                self.next_character();
                self.make_operator_or_punctuation(TokenKind::OpenParen, pos0)
            }
            Some(')') => {
                self.next_character();
                self.make_operator_or_punctuation(TokenKind::CloseParen, pos0)
            }
            Some('{') => {
                self.next_character();
                self.make_operator_or_punctuation(TokenKind::OpenCurly, pos0)
            }
            Some('}') => {
                self.next_character();
                self.make_operator_or_punctuation(TokenKind::CloseCurly, pos0)
            }
            Some('[') => {
                self.next_character();
                self.make_operator_or_punctuation(TokenKind::OpenBracket, pos0)
            }
            Some(']') => {
                self.next_character();
                self.make_operator_or_punctuation(TokenKind::CloseBracket, pos0)
            }
            _ => {
                self.report_error_at(self.get_span(pos0), "Illegal token detected.");
                self.make_token(TokenKind::Invalid, ValueKind::None, pos0)
            }
        }
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

    fn make_ident_or_keyword(&mut self, pos0: Position) -> Token<'a> {
        let mut ident_or_keyword = String::from(self.next_character().unwrap());

        while let Some(c) = self.peek_next_character()
            && (c.is_alphanumeric() || c == '_')
        {
            ident_or_keyword.push(self.next_character().unwrap());
        }

        let (kind, value) = match ident_or_keyword.as_str() {
            // Booleans
            "true" => (TokenKind::Bool, ValueKind::Bool(true)),
            "false" => (TokenKind::Bool, ValueKind::Bool(false)),

            // Floats
            "inf" => (TokenKind::Float, ValueKind::Float(f64::INFINITY)),
            "nan" => (TokenKind::Float, ValueKind::Float(f64::NAN)),

            // Keywords
            "fn" => (TokenKind::Fn, ValueKind::None),
            "return" => (TokenKind::Return, ValueKind::None),
            "for" => (TokenKind::For, ValueKind::None),
            "break" => (TokenKind::Break, ValueKind::None),
            "continue" => (TokenKind::Continue, ValueKind::None),
            "if" => (TokenKind::If, ValueKind::None),
            "else" => (TokenKind::Else, ValueKind::None),

            _ => (TokenKind::Ident, ValueKind::Ident(ident_or_keyword)),
        };

        self.make_token(kind, value, pos0)
    }

    fn make_numeric_literal(&mut self, pos0: Position) -> Token<'a> {
        let mut buf = String::new();

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
                    let span = self.get_span(pos0);
                    self.report_error_at(
                        span,
                     "Must have at least one optionally signed digit as exponent (e.g., '1.32e5', '0.9e-5', '4.5e+5').");
                    return self.make_token(TokenKind::Invalid, ValueKind::None, pos0);
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
                    let span = self.get_span(pos0);
                    self.report_error_at(
                        span,
                    "Must have at least one digit as exponent after sign (e.g., '0.9e-5', '4.5e+5').");
                    return self.make_token(TokenKind::Invalid, ValueKind::None, pos0);
                }
                exp_sign = true;
            }

            buf.push(c);
        }

        let span = self.get_span(pos0);
        if float {
            if base != 10 {
                self.report_error_at(span, "Only base-10 float literals are supported.");
                return self.make_token(TokenKind::Invalid, ValueKind::None, pos0);
            }
            let value = match buf.parse::<f64>() {
                Ok(v) => ValueKind::Float(v),
                Err(e) => {
                    self.report_error_at(
                        span,
                        &format!("Error while parsing float literal: `{}`: {}", &buf, e),
                    );
                    return self.make_token(TokenKind::Invalid, ValueKind::None, pos0);
                }
            };
            self.make_token(TokenKind::Float, value, pos0)
        } else {
            let value = match u128::from_str_radix(&buf, base) {
                Ok(v) => ValueKind::Int(v),
                Err(e) => {
                    self.report_error_at(
                        span,
                        &format!("Error while parsing integer literal: `{}`: {}", &buf, e),
                    );
                    return self.make_token(TokenKind::Invalid, ValueKind::None, pos0);
                }
            };
            self.make_token(TokenKind::Int, value, pos0)
        }
    }

    fn make_string_literal(&mut self, pos0: Position) -> Token<'a> {
        self.next_character(); // Eat the leading "
        let mut string_buf = String::new();

        loop {
            match self.next_character() {
                None => {
                    self.report_error_at(
                        self.get_span(pos0),
                        "Unclosed string literal (unexpected EOF).",
                    );
                    return self.make_token(TokenKind::Invalid, ValueKind::None, pos0);
                }
                Some('"') => break,
                Some('\\') => match self.next_character() {
                    Some('n') => string_buf.push('\n'),
                    Some('r') => string_buf.push('\r'),
                    Some('t') => string_buf.push('\t'),
                    Some('\\') => string_buf.push('\\'),
                    Some('"') => string_buf.push('"'),
                    Some('x') => match self.make_possible_character_from_hex_digits(2) {
                        Ok(c) => string_buf.push(c),
                        Err(()) => {
                            self.report_error_at(
                                self.get_span(pos0),
                                "Must have exactly 2 hex digits after '\\x'.",
                            );
                            return self.make_token(TokenKind::Invalid, ValueKind::None, pos0);
                        }
                    },
                    Some('u') => match self.make_possible_character_from_hex_digits(4) {
                        Ok(c) => string_buf.push(c),
                        Err(()) => {
                            self.report_error_at(
                                self.get_span(pos0),
                                "Must have exactly 4 hex digits after '\\x'.",
                            );
                            return self.make_token(TokenKind::Invalid, ValueKind::None, pos0);
                        }
                    },
                    Some('U') => match self.make_possible_character_from_hex_digits(8) {
                        Ok(c) => string_buf.push(c),
                        Err(()) => {
                            self.report_error_at(
                                self.get_span(pos0),
                                "Must have exactly 8 hex digits after '\\x'.",
                            );
                            return self.make_token(TokenKind::Invalid, ValueKind::None, pos0);
                        }
                    },
                    Some(c) => {
                        self.report_error_at(
                            self.get_span(pos0),
                            &format!("Invalid escape code in string literal: `{}`", c),
                        );
                        return self.make_token(TokenKind::Invalid, ValueKind::None, pos0);
                    }
                    None => {
                        self.report_error_at(
                            self.get_span(pos0),
                            "Unclosed string literal (unexpected EOF).",
                        );
                        return self.make_token(TokenKind::Invalid, ValueKind::None, pos0);
                    }
                },
                Some(c) => string_buf.push(c),
            }
        }

        self.make_token(TokenKind::Str, ValueKind::Str(string_buf), pos0)
    }

    fn make_character_literal(&mut self, pos0: Position) -> Token<'a> {
        let ch: u8 = match self.next_character() {
            None | Some('\'') => {
                let span = self.get_span(pos0);
                self.report_error_at(span, "Empty character literal.");
                return self.make_token(TokenKind::Invalid, ValueKind::None, pos0);
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
                        let span = self.get_span(pos0);
                        self.report_error_at(span, "Must have exactly 2 hex digits after '\\x'.");
                        return self.make_token(TokenKind::Invalid, ValueKind::None, pos0);
                    }
                },
                Some(c) => {
                    let span = self.get_span(pos0);
                    self.report_error_at(
                        span,
                        &format!("Invalid escape code in character literal: `{}`", c),
                    );
                    return self.make_token(TokenKind::Invalid, ValueKind::None, pos0);
                }
                None => {
                    let span = self.get_span(pos0);
                    self.report_error_at(span, "Unexpected EOF in character literal.");
                    return self.make_token(TokenKind::Invalid, ValueKind::None, pos0);
                }
            },
            Some(c) => c as u8,
        };

        match self.next_character() {
            Some('\'') => self.make_token(TokenKind::Char, ValueKind::Char(ch), pos0),
            None => {
                let span = self.get_span(pos0);
                self.report_error_at(span, "Unclosed character literal (unexpected EOF).");
                self.make_token(TokenKind::Invalid, ValueKind::None, pos0)
            }
            Some(_) => {
                let span = self.get_span(pos0);
                self.report_error_at(span, "Character literal contains more than one character.");
                self.make_token(TokenKind::Invalid, ValueKind::None, pos0)
            }
        }
    }

    fn make_operator_or_punctuation(&mut self, kind: TokenKind, pos0: Position) -> Token<'a> {
        self.make_token(kind, ValueKind::None, pos0)
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

    fn make_token(&mut self, kind: TokenKind, value: ValueKind, pos0: Position) -> Token<'a> {
        let span = self.get_span(pos0);
        Token {
            kind,
            value,
            span,
            lexeme: &self.input[span.0.offset..span.1.offset],
        }
    }

    fn next_character(&mut self) -> Option<char> {
        let c = self.input_iter.next()?;
        self.pos.offset += 1;
        self.pos.column.bytes += c.len_utf8();
        self.pos.column.chars += 1;
        if c == '\n' {
            self.pos.line += 1;
            self.pos.column = ColumnPosition::default();
        }
        Some(c)
    }

    fn peek_next_character(&mut self) -> Option<char> {
        self.peek_character(0)
    }

    fn peek_character(&mut self, n: usize) -> Option<char> {
        let mut iter = self.input_iter.clone();
        iter.nth(n)
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
