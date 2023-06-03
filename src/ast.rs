//! AST module provides definitions and implementations of AST nodes.
//! We define AST nodes as enums with data.
use crate::token;
use crate::types;

/// Declarations involve variable and function declarations.
pub enum Decl {
    // Variable declarations associate an identifier with a declaration type
    // and a value.
    Var(String, types::DeclType, Expr),
    // Function declarations associate an identifier with a return type, args
    // and a body.
    Function(String, types::DeclType),
}

/// Expressions in axion can be literals, unary, binary or assignments.
pub enum Expr {
    // Literal expressions simply return the literal value.
    Literal(LiteralValue),
    // Unary expressions associate a unary operator with an expression.
    Unary(token::Token, Box<Expr>),
    // Binary expressions associate an infix operator with two expressions.
    Binary(token::Token, Box<Expr>, Box<Expr>),
    // Logical expressions associate an infix logical operator with two
    // expressions
    Logical(token::Token, Box<Expr>, Box<Expr>),
    // Assignment expressions associate an identifier with an expression.
    Assign(token::Token, Box<Expr>),
    // Variable expressions associate an identifier to an expression.
    Var(token::Token),
    // Grouping expressions are expressions enclosed in parenthesis to denote
    // their grouping.
    Grouping(Box<Expr>),
    // Index expressions associate a variable with an index expression.
    Index(Box<Expr>, Box<Expr>),
}

/// Literal values used to represent primitive types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralValue {
    Int(i64),
    Str(String),
    Char(char),
    Boolean(bool),
}
