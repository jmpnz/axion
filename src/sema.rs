//! Semantic analyzer responsible for doing semantic analysis, type checking
//! and rewriting the AST to include type annotations for the next step.
use crate::ast;
use crate::token::Token;
use crate::types;

use std::collections::HashMap;

/// `Scope` defines the scope of a symbol.
#[derive(Debug, Copy, Clone)]
pub enum Scope {
    Local,
    Global,
}

/// `Symbol` represents a symbol in the AST. Symbols include variables
/// and functions.
#[derive(Debug, Clone)]
pub struct Symbol {
    // Symbol name.
    name: String,
    // Symbol type.
    stype: types::DeclType,
    // Symbol scope.
    scope: Scope,
}

impl Symbol {
    pub fn new(name: &str, stype: types::DeclType, scope: Scope) -> Self {
        Self {
            name: name.to_string(),
            stype,
            scope,
        }
    }

    // Returns the symbol name as an immutable reference.
    fn name(&self) -> &str {
        self.name.as_str()
    }
}

/// `SymTable` represents a stack of symbol tables, the idea is that within
/// each scope we push a new symbol table to the stack and all declarations
/// within this scope end up there. Once we leave the scope we restore the
/// stack pointer to the previous scope.
pub struct SymTable {
    tables: Vec<HashMap<String, Symbol>>,
}

impl Default for SymTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymTable {
    pub fn new() -> Self {
        Self { tables: Vec::new() }
    }

    // Resolve a new symbol.
    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        None
    }

    // Bind a new symbol.
    pub fn bind(&mut self, name: &str, sym: Symbol) {
        match self.tables.last_mut() {
            None => self.tables.push(HashMap::new()),
            Some(tbl) => {
                tbl.entry(name.to_string()).or_insert(sym);
            }
        }
    }
}

/// `SemanticAnalyzer` implements the `ASTConsumer` trait and runs a semantic
/// analysis pass on the initial AST created by the `Parser`.
pub struct SemanticAnalyzer {
    // AST to process, the AST which represents the program to compile
    // is a vector of declarations.
    ast: Vec<ast::Stmt>,
    // Symbol table created during semantic analysis, it collects all
    // the existing symbols (variables and functions), their types
    // and their scopes.
    sym_table: SymTable,
}

impl SemanticAnalyzer {
    pub fn new(ast: Vec<ast::Stmt>) -> Self {
        Self {
            ast,
            sym_table: SymTable::new(),
        }
    }
}

/// Implementation of the `ASTConsumer` trait for `SemanticAnalyzer`
impl ast::ASTConsumer<types::DeclType> for SemanticAnalyzer {
    /// When we visit an expression we try to infer it's type based on a few
    /// heuristics.
    /// 1. If the expression is an assignment or variable we resolve it from
    /// the symbol table and gets it's declaration type.
    /// 2. If the expression is a literal we use the `typeof` function to get
    /// its type.
    /// 3. If the expression we visit is a logical, unary or binary operation
    /// we resolve the types of the leaves and propagate them back.
    fn visit_expr(&mut self, expr: &ast::Expr) -> types::DeclType {
        match expr {
            ast::Expr::Literal(v) => v.get_type(),
            _ => todo!(),
        }
    }

    /// When we visit a statement the type checking is done on expressions
    /// encapsulated within the statement.
    /// For example visiting an `Stmt::If` will type check the conditional
    /// to ensure it's a `Boolean` expression.
    fn visit_stmt(&mut self, stmt: &ast::Stmt) -> types::DeclType {
        match stmt {
            _ => todo!(),
        }
    }
}

/// Type checking pass is handled by the `TypeChecker` struct that implements
/// the `ASTConsumer` trait. Once the semantic analysis validates the AST it
/// creates a symbol table with annoted types for existing declarations in the
/// program.
/// Type checking is done by walking the AST and propagating the types from
/// the leaves to the root of the expression.
#[derive(Debug)]
pub struct TypeChecker;
