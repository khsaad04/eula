use crate::token::TokenKind as TK;

use std::collections::HashMap;

#[derive(Debug)]
pub struct TopLevel {
    pub decls: HashMap<String, Decl>,
}

#[derive(Debug)]
pub enum Decl {
    Fn(FnDecl),
    Var(VarDecl),
}

#[derive(Debug)]
pub struct FnDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: TypeExpr,
    pub body: Option<Block>, // `None` means it's just a declaration with no body.
}

#[derive(Debug)]
pub struct VarDecl {
    pub name: String,
    pub ty: TypeExpr,
    pub value: Option<Expr>,
}

// Parameters might have different properties in the future.
pub type Param = VarDecl;

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: TypeExpr,
}

impl Expr {
    pub fn to_c_expr(&self) -> String {
        match &self.kind {
            ExprKind::Int(i) => format!("{}", i),
            ExprKind::Str(s) => format!("{:?}", s),
            ExprKind::FnCall { name, args } => {
                let mut buf = String::new();
                buf.push_str(name);
                buf.push('(');
                for (i, arg) in args.iter().enumerate() {
                    buf.push_str(&arg.to_c_expr());
                    if i < args.len() - 1 {
                        buf.push(',');
                    }
                }
                buf.push(')');
                buf
            }
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Str(String),
    Char(u8),
    Int(u128),
    Float(f64),
    Bool(bool),
    Type(TypeExpr),
    Var {
        name: String,
    },
    FnCall {
        name: String,
        args: Vec<Expr>,
    },
    UnaryOp {
        op: UnaryOpKind,
        operand: Box<Expr>,
    },
    BinaryOp {
        op: BinaryOpKind,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    AssignOp {
        op: AssignOpKind,
        lvalue: Box<Expr>,
        rvalue: Box<Expr>,
    },
}

#[derive(Debug)]
pub enum TypeExpr {
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
    String,
    Array(Box<TypeExpr>),
    Ref(Box<TypeExpr>),
    Type,
    Unknown, // This should get resolved at the typing phase.
}

impl TypeExpr {
    pub fn to_c_type(&self) -> String {
        match self {
            Self::U0 => "void".to_string(),
            Self::I32 => "int32_t".to_string(),
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    Block(Block),
    Decl(Decl),
    If {
        cond: Expr,
        then_block: Block,
        else_block: Option<Block>,
    },
    For {
        init: Box<Option<Stmt>>,
        cond: Option<Expr>,
        incr: Option<Expr>,

        body: Block,
    },
    Return(Expr),
    Expr(Expr),
}

pub type Block = Vec<Stmt>;

#[derive(Debug)]
pub enum AssignOpKind {
    Equals, // =

    PlusEquals,  // +=
    MinusEquals, // -=
    TimesEquals, // *=
    DivEquals,   // /=
    ModEquals,   // %=

    BitwiseAndEquals, // &=
    BitwiseOrEquals,  // |=
    BitwiseNotEquals, // ~=
    BitwiseXorEquals, // ^=
    BitwiseShlEquals, // <<=
    BitwiseShrEquals, // >>=
}

#[derive(Debug)]
pub enum BinaryOpKind {
    Plus,  // +
    Minus, // -
    Times, // *
    Div,   // /
    Mod,   // %

    BitwiseAnd, // &
    BitwiseOr,  // |
    BitwiseXor, // ^
    BitwiseShl, // <<
    BitwiseShr, // >>

    LessThan,       // <
    GreaterThan,    // >
    LessOrEqual,    // <=
    GreaterOrEqual, // >=
    IsEqual,        // ==
    IsNotEqual,     // !=
    LogicalAnd,     // &&
    LogicalOr,      // ||
}

#[derive(Debug)]
pub enum UnaryOpKind {
    Plus,       // +
    Minus,      // -
    BitwiseNot, // ~
    LogicalNot, // !
    Ref,        // *.
    Deref,      // .*
}

impl TK {
    pub fn to_assop(&self) -> Option<AssignOpKind> {
        match self {
            Self::Equals => Some(AssignOpKind::Equals),

            Self::PlusEquals => Some(AssignOpKind::PlusEquals),
            Self::MinusEquals => Some(AssignOpKind::MinusEquals),
            Self::TimesEquals => Some(AssignOpKind::TimesEquals),
            Self::DivEquals => Some(AssignOpKind::DivEquals),
            Self::ModEquals => Some(AssignOpKind::ModEquals),

            Self::BitwiseAndEquals => Some(AssignOpKind::BitwiseAndEquals),
            Self::BitwiseOrEquals => Some(AssignOpKind::BitwiseOrEquals),
            Self::BitwiseNotEquals => Some(AssignOpKind::BitwiseNotEquals),
            Self::BitwiseXorEquals => Some(AssignOpKind::BitwiseXorEquals),
            Self::BitwiseShlEquals => Some(AssignOpKind::BitwiseShlEquals),
            Self::BitwiseShrEquals => Some(AssignOpKind::BitwiseShrEquals),

            _ => None,
        }
    }

    pub fn to_binop(&self) -> Option<BinaryOpKind> {
        match self {
            Self::Plus => Some(BinaryOpKind::Plus),
            Self::Minus => Some(BinaryOpKind::Minus),
            Self::Star => Some(BinaryOpKind::Times),
            Self::Div => Some(BinaryOpKind::Div),
            Self::Mod => Some(BinaryOpKind::Mod),

            Self::BitwiseAnd => Some(BinaryOpKind::BitwiseAnd),
            Self::BitwiseOr => Some(BinaryOpKind::BitwiseOr),
            Self::BitwiseXor => Some(BinaryOpKind::BitwiseXor),
            Self::BitwiseShl => Some(BinaryOpKind::BitwiseShl),
            Self::BitwiseShr => Some(BinaryOpKind::BitwiseShr),

            Self::LessThan => Some(BinaryOpKind::LessThan),
            Self::GreaterThan => Some(BinaryOpKind::GreaterThan),
            Self::LessOrEqual => Some(BinaryOpKind::LessOrEqual),
            Self::GreaterOrEqual => Some(BinaryOpKind::GreaterOrEqual),
            Self::IsEqual => Some(BinaryOpKind::IsEqual),
            Self::IsNotEqual => Some(BinaryOpKind::IsNotEqual),
            Self::LogicalAnd => Some(BinaryOpKind::LogicalAnd),
            Self::LogicalOr => Some(BinaryOpKind::LogicalOr),

            _ => None,
        }
    }

    // Currently there's only a single postfix unary op `Deref`,
    // which is handled differently than the rest.
    // So, we only need a method for prefix ops.
    pub fn to_prefix_unop(&self) -> Option<UnaryOpKind> {
        match self {
            Self::Plus => Some(UnaryOpKind::Plus),
            Self::Minus => Some(UnaryOpKind::Minus),
            Self::BitwiseNot => Some(UnaryOpKind::BitwiseNot),
            Self::LogicalNot => Some(UnaryOpKind::LogicalNot),
            Self::Ref => Some(UnaryOpKind::Ref),
            // Self::Deref => Some(UnaryOpKind::Deref),
            _ => None,
        }
    }
}
