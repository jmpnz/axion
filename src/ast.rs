//! AST representation for Axion programs and other compiler passes.
use crate::types;

/// Function parameters are identifiers with a type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter(pub String, pub types::DeclType);

/// Binary operators.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Gte,
    Lt,
    Lte,
    Equ,
    Neq,
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Gt => write!(f, ">"),
            Self::Gte => write!(f, ">="),
            Self::Lt => write!(f, "<"),
            Self::Lte => write!(f, "<="),
            Self::Equ => write!(f, "=="),
            Self::Neq => write!(f, "!="),
        }
    }
}

/// Unary operators.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Not => write!(f, "!"),
            Self::Neg => write!(f, "-"),
        }
    }
}

/// Logical operators.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LogicalOp {
    And,
    Or,
}

impl std::fmt::Display for LogicalOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
        }
    }
}

/// Statements in axion are no different than statements in C or JavaScript
/// they include the usual control flow statements and declarartions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    // Variable declarations associate an identifier with a declaration type
    // and a value.
    Var(String, types::DeclType, Option<Expr>),
    // Function declarations associate an identifier with a return type, args
    // and a body.
    Function(String, types::DeclType, Vec<Parameter>, Vec<Stmt>),
    // Expression statement are statements that induce side effects.
    Expr(Expr),
    // Return statements are statements that return expressions.
    Return(Expr),
    // Block statements are sequences of statements.
    Block(Vec<Stmt>),
    // If statements evaluate an expression and executing a branch depending
    // on the evaluation result, the else branch is optional.
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    // "While" statements are for looping control flow, the body of the loop
    // is executed as long as the condition is fullfilled i.e evaluates to
    // true.
    // While we support "for" loops at the language level for representation
    // we can get away by apply desugaring, essentially representing "for"
    // loops at the syntax level as "While" loops at the ast level.
    While(Expr, Box<Stmt>),
    // Break statements break out of loops.
    Break,
}

/// Expressions in axion can be literals, unary, binary, assignments...
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    // Literal expressions simply return the literal value.
    Literal(LiteralValue),
    // Unary expressions associate a unary operator with an expression.
    Unary(UnaryOp, Box<Expr>),
    // Binary expressions associate an infix operator with two expressions.
    Binary(BinOp, Box<Expr>, Box<Expr>),
    // Logical expressions associate an infix logical operator with two
    // expressions
    Logical(LogicalOp, Box<Expr>, Box<Expr>),
    // Assignment expressions associate an identifier with an expression.
    Assign(String, Box<Expr>),
    // Variable expressions associate an identifier to an expression.
    Var(String),
    // Grouping expressions are expressions enclosed in parenthesis to denote
    // their grouping.
    Grouping(Box<Expr>),
    // Index expressions associate a variable with an index expression.
    Index(Box<Expr>, Box<Expr>),
    // Call expressions.
    Call(Box<Expr>, Vec<Expr>),
}

/// Literal values used to represent primitive types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralValue {
    Int(i64),
    Str(String),
    Char(char),
    Boolean(bool),
}

impl LiteralValue {
    // Return the type of the literal value as a `DeclType`.
    pub const fn get_type(&self) -> types::DeclType {
        match self {
            Self::Int(_) => types::DeclType::Integer,
            Self::Str(_) => types::DeclType::String,
            Self::Char(_) => types::DeclType::Char,
            Self::Boolean(_) => types::DeclType::Boolean,
        }
    }
}
