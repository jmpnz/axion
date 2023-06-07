//! Semantic analyzer responsible for doing semantic analysis, type checking
//! and rewriting the AST to include type annotations for the next step.
use crate::ast;
use crate::token::Token;
use crate::types::DeclType;

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
    stype: DeclType,
    // Symbol scope.
    scope: Scope,
}

impl Symbol {
    pub fn new(name: &str, stype: DeclType, scope: Scope) -> Self {
        Self {
            name: name.to_string(),
            stype: stype,
            scope: scope,
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
        match self.tables.last() {
            None => self.tables.push(HashMap::new()),
            Some(tbl) => tbl.try_insert(name.to_string(), sym).unwrap(),
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

/// Type checking pass is handled by the `TypeChecker` struct that implements
/// the `ASTConsumer` trait. Once the semantic analysis validates the AST it
/// creates a symbol table with annoted types for existing declarations in the
/// program.
/// Type checking is done by walking the AST and propagating the types from
/// the leaves to the root of the expression.
#[derive(Debug)]
pub struct TypeChecker;
