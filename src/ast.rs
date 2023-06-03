//! AST module provides definitions and implementations of AST nodes.
//! We define AST nodes as enums with data.
use crate::types
use crate::token;

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
    Literal,
    // Unary expressions associate a unary operator with an expression.
    Unary(token::Token, Box<Expr>),
    // Binary expressions associate an infix operator with two expressions.
    Binary(token::Token, Box<Expr>, Box<Expr>),
}
