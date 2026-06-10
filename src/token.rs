use std::{fmt, path};

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub val: ValueKind,
    pub loc: Location<'a>,
    pub lexeme: &'a str,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind as TK;
        match &self.kind {
            TK::Ident
            | TK::Int
            | TK::Float
            | TK::Bool
            | TK::Fn
            | TK::Return
            | TK::For
            | TK::Break
            | TK::Continue
            | TK::If
            | TK::Else => write!(f, "{} `{}`", self.kind, self.lexeme),
            // These look kind of weird enclosed in backticks because they already have their own delimiters...
            TK::Str | TK::Char => write!(f, "{} {}", self.kind, self.lexeme),
            TK::Eof => write!(f, "{}", self.kind),
            TK::Invalid => {
                let ValueKind::LexError(msg) = &self.val else {
                    unreachable!()
                };
                write!(f, "lexical error: {}", msg)
            }
            _ => write!(f, "`{}`", self.kind),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Literals
    // The corresponding literal values are stored in `tok.val` as `ValueKind`.
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

    // Operators & puncts
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

    // Misc
    Invalid, // The error msg can be found in `tok.val` as `ValueKind`.
    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
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

            Self::Invalid => write!(f, "an invalid token"),
            Self::Eof => write!(f, "end of file"),
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
    LexError(String),
    None,
}

#[derive(Debug, Clone, Copy)]
pub struct Location<'a> {
    pub input_path: &'a path::Path,

    pub l0: usize,
    pub c0: usize,

    pub l1: usize,
    pub c1: usize,
}
