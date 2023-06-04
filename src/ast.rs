//! AST module provides definitions and implementations of AST nodes.
//! We define AST nodes as enums with data.
use crate::token;
use crate::types;

/// Statements in axion are no different than statements in C or JavaScript
/// they include the usual control flow statements and declarartions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    // Variable declarations associate an identifier with a declaration type
    // and a value.
    Var(String, types::DeclType, Option<Expr>),
    // Function declarations associate an identifier with a return type, args
    // and a body.
    Function(String, types::DeclType),
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
}

/// Expressions in axion can be literals, unary, binary, assignments...
#[derive(Debug, Clone, PartialEq, Eq)]
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
    Assign(String, Box<Expr>),
    // Variable expressions associate an identifier to an expression.
    Var(String),
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
