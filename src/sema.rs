//! Semantic analyzer responsible for doing semantic analysis, type checking
//! and rewriting the AST to include type annotations for the next step.
use crate::ast;
use crate::ast::ASTConsumer;
use crate::token::Token;
use crate::types;

use std::collections::HashMap;

/// `Scope` defines the scope of a symbol.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Scope {
    Local,
    Global,
}

/// `Symbol` represents a symbol in the AST. Symbols include variables
/// and functions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    // Symbol name.
    name: String,
    // Symbol type.
    stype: types::DeclType,
    // Symbol scope.
    scope: Scope,
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Symbol({}, {}, {:?})", self.name, self.stype, self.scope)
    }
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

/// `SymbolTable` represents a stack of symbol tables, the global scope
/// sits at `root_idx` and is always the 0th entry. Each time we enter
/// a new scope (scopes are enclosed within `ast::Stmt::Block`) we push
/// a new symbol table into the stack and all declarations within the
/// scope end up in the most recent symbol table.
pub struct SymbolTable {
    // Root index in the stack represents the global scope, the root index
    // is immutable.
    root_idx: usize,
    // Current index in the stack represents the current scope.
    curr_idx: usize,
    // Parent index represents the index of the parent scope of the scope
    parent_idx: usize,
    // pointed at by `curr_idx`.
    tables: Vec<HashMap<String, Symbol>>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            root_idx: 0,
            curr_idx: 0,
            parent_idx: 0,
            tables: vec![HashMap::new()],
        }
    }

    // Resolve a new symbol.
    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        // Get a reference to the current scope we're processing
        let mut current_scope_table = &self.tables[self.curr_idx];
        let mut idx = self.curr_idx;
        loop {
            // Try and find the declaration in the current scope.
            for (ident, symbol) in current_scope_table {
                if ident == name {
                    return Some(symbol.clone());
                }
            }
            // If we didn't find the declaration in the current scope
            // we check the parent.
            if let Some(_) = idx.checked_sub(1) {
                idx -= 1;
                current_scope_table = &self.tables[idx];
            } else {
                break;
            }
        }
        None
    }

    // Bind a new symbol to the current scope.
    pub fn bind(&mut self, name: &str, sym: Symbol) {
        let mut tbl = &mut self.tables[self.curr_idx];
        match tbl.get(name) {
            Some(value) => {
                panic!("Name {} is already bound to symbol {}", name, sym)
            }
            None => {
                tbl.insert(name.to_string(), sym);
            }
        }
    }

    // Returns a view of the symbol tables.
    fn tables(&self) -> &Vec<HashMap<String, Symbol>> {
        &self.tables
    }

    // Return the index of the current scope.
    pub fn scope(&self) -> usize {
        self.curr_idx
    }

    // Entering a new scope pushes a new symbol table into the stack.
    pub fn enter_scope(&mut self) {
        let table = HashMap::new();
        self.tables.push(table);
        self.parent_idx = self.curr_idx;
        self.curr_idx += 1;
    }

    // Leave the current scope returning the parent.
    pub fn leave_scope(&mut self) {
        self.curr_idx = self.parent_idx;
        if self.parent_idx > 0 {
            self.parent_idx = self.parent_idx - 1;
        }
    }
}

/// `SemanticAnalyzer` implements the `ASTConsumer` trait and runs a semantic
/// analysis pass on the initial AST created by the `Parser`.
///
/// Once the semantic analysis validates the AST it creates a symbol table
/// with annoted types for existing declarations in the program.§
///
/// Type checking is done by resolving the types of the symbols or literals
/// at the leafs and propagating them to root of the expression.
pub struct SemanticAnalyzer {
    // AST to process, the AST which represents the program to compile
    // is a vector of declarations.
    // ast: Vec<ast::Stmt>,
    // Symbol table created during semantic analysis, it collects all
    // the existing symbols (variables and functions), their types
    // and their scopes.
    sym_table: SymbolTable,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            sym_table: SymbolTable::new(),
        }
    }

    /// Returns a view the symbol table.
    pub fn sym_table(&self) -> &Vec<HashMap<String, Symbol>> {
        &self.sym_table.tables()
    }

    /// Get the expected scope of a symbol given our index in the symbol
    /// table stack.
    fn scope(&self) -> Scope {
        match self.sym_table.scope() {
            0 => Scope::Global,
            _ => Scope::Local,
        }
    }

    /// Define a new binding given a declaration.
    fn define(&mut self, stmt: &ast::Stmt) {
        match stmt {
            ast::Stmt::Var(name, t, value) => {
                let scope = self.scope();
                self.sym_table.bind(name, Symbol::new(name, *t, scope));
            }
            // TODO: bind the return type or compound type
            ast::Stmt::Function(name, t, params, body) => {
                let scope = self.scope();
                self.sym_table.bind(
                    name,
                    Symbol::new(name, types::DeclType::Function, scope),
                );
            }
            _ => panic!("Expected declaration found statement : {:?}", stmt),
        }
    }

    /// Resolve an expression to check if it was properly defined.
    fn resolve(&self, expr: &ast::Expr) {
        match expr {
            // Nothing to do for literals
            ast::Expr::Literal(_) => (),
            // Check if the variable expression was properly bound.
            ast::Expr::Unary(_, rhs) => self.resolve(rhs),
            ast::Expr::Binary(_, lhs, rhs) => {
                self.resolve(lhs);
                self.resolve(rhs);
            }
            ast::Expr::Logical(_, lhs, rhs) => {
                self.resolve(lhs);
                self.resolve(rhs);
            }
            ast::Expr::Assign(name, rhs) => {
                // Check if the assignee is properly defined
                if let Some(_) = self.sym_table.resolve(name) {
                    // Assignee is properly defined try and resolve the rhs.
                    self.resolve(rhs);
                }
                // If it's unknown fail.
                panic!("Unknown variable assignment at {name}");
            }
            ast::Expr::Var(name) => match self.sym_table.resolve(name) {
                Some(_) => (),
                None => panic!("Unkown variable {name}"),
            },
            ast::Expr::Grouping(group) => {
                self.resolve(group);
            },
            ast::Expr::Call(callee, args) => {
                self.resolve(callee);
                for arg in args {
                    self.resolve(arg);
                }
            },
            _ => panic!("Trying to call a non function!"),
        }
    }

    /// Core analysis routine, builds the symbol table and collect semantic
    /// errors to display later.
    fn analyze(&mut self, stmt: &ast::Stmt) {
        match stmt {
            ast::Stmt::Var(..) => self.define(stmt),
            ast::Stmt::Function(_, _, _, ref body) => {
                self.define(stmt);
                self.sym_table.enter_scope();
                for stmt in body {
                    self.analyze(&stmt);
                }
                self.sym_table.leave_scope();
            }
            ast::Stmt::Block(stmts) => {
                self.sym_table.enter_scope();
                for stmt in stmts {
                    self.analyze(&stmt);
                }
                self.sym_table.leave_scope();
            }
            ast::Stmt::Expr(expr) => self.resolve(&expr),
            ast::Stmt::Return(expr) => self.resolve(&expr),
            ast::Stmt::If(expr, thenBranch, condBranch) => {
                self.resolve(&expr);
                self.analyze(&thenBranch);
                match condBranch {
                    None => (),
                    Some(branch) => self.analyze(&branch),
                }
            },
            ast::Stmt::While(expr, body) => {
                self.resolve(&expr);
                self.analyze(&body);
            },
            _ => todo!(),
        }
    }

    /// Run semantic analysis pass on the given AST.
    pub fn run(&mut self, ast: &Vec<ast::Stmt>) {
        for stmt in ast {
            self.analyze(stmt);
        }
    }
}

/// Implementation of the `ASTConsumer` trait for `SemanticAnalyzer`
impl ast::ASTConsumer<types::DeclType> for SemanticAnalyzer {
    /// When we visit an expression we try to infer it's type based on a few
    /// heuristics, during type checking we try and resolve the declarations
    /// referenced in the expression, if resolution fails we throw an error.
    /// 1. If the expression is an assignment or variable we resolve it from
    /// the symbol table and gets it's declaration type.
    /// 2. If the expression is a literal we use the `typeof` function to get
    /// its type.
    /// 3. If the expression we visit is a logical, unary or binary operation
    /// we resolve the types of the leaves and propagate them back.
    fn visit_expr(&self, expr: &ast::Expr) -> types::DeclType {
        match expr {
            ast::Expr::Literal(v) => v.get_type(),
            _ => todo!(),
        }
    }

    /// When we visit a statement the type checking is done on expressions
    /// encapsulated within the statement.
    /// For example visiting an `Stmt::If` will type check the conditional
    /// to ensure it's a `Boolean` expression.
    fn visit_stmt(&self, stmt: &ast::Stmt) -> types::DeclType {
        match stmt {
            ast::Stmt::Var(ident, t, value) => *t,
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::types::DeclType;

    #[test]
    fn can_build_symbol_table() {
        let source = "let i:int = 42;";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();
        let mut sema = SemanticAnalyzer::new();
        sema.run(&ast);

        for tbl in sema.sym_table() {
            for (name, sym) in tbl {
                assert_eq!(name, "i");
                assert_eq!(
                    sym,
                    &Symbol::new("i", types::DeclType::Integer, Scope::Global)
                );
            }
        }
    }

    #[test]
    #[should_panic]
    fn fail_to_build_symbol_table_when_undefined() {
        let source = "let i:int = 42;\n i = a;";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();
        let mut sema = SemanticAnalyzer::new();
        sema.run(&ast);

        for tbl in sema.sym_table() {
            for (name, sym) in tbl {
                assert_eq!(name, "i");
                assert_eq!(
                    sym,
                    &Symbol::new("i", types::DeclType::Integer, Scope::Global)
                );
            }
        }
    }

    #[test]
    fn can_build_symbol_table_with_scopes() {
        let source = r#"
        let i:int = 42;
        function die(a:int) -> void {
            {
                let a : int = 0;
                let b : int = 1;
                let c : boolean = true;
                let d : boolean = c;

                {
                    let e : int = a + b;
                }
            }

        }
        "#;
        let mut lexer = Lexer::new(source);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();
        let mut sema = SemanticAnalyzer::new();
        sema.run(&ast);

        for tbl in sema.sym_table() {
            for (name, sym) in tbl {
                println!("Name : {} Symbol : {}", name, sym);
            }
        }
    }

    #[test]
    fn can_build_symbol_table_for_all_expr() {
        let source = r#"
        let i:int = 42;
        function die(a:int) -> void {
            {
                let a : int = 0;
                let b : int = 1;
                let c : boolean = true;
                let d : boolean = c;

                {
                    let e : int = a + b;
                    let a : boolean = !c;
                    let d : boolean = c && a;
                }
            }

        }
        die(100);
        "#;
        let mut lexer = Lexer::new(source);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();
        let mut sema = SemanticAnalyzer::new();
        sema.run(&ast);

        for tbl in sema.sym_table() {
            for (name, sym) in tbl {
                println!("Name : {} Symbol : {}", name, sym);
            }
        }
    }

    #[test]
    fn symbol_table_bind_and_resolve() {
        let mut sym_table = SymbolTable::new();
        let symbols = vec![
            Symbol::new("a", types::DeclType::Integer, Scope::Global),
            Symbol::new("b", types::DeclType::String, Scope::Global),
            Symbol::new("c", types::DeclType::Boolean, Scope::Local),
            Symbol::new("d", types::DeclType::Integer, Scope::Local),
            Symbol::new("e", types::DeclType::Integer, Scope::Local),
        ];

        for sym in &symbols[..2] {
            sym_table.bind(sym.name(), sym.clone())
        }
        sym_table.enter_scope();
        for sym in &symbols[2..] {
            sym_table.bind(sym.name(), sym.clone())
        }

        for sym in &symbols {
            let symbol = sym_table.resolve(sym.name());
            assert!(!symbol.is_none());
        }
    }

    #[test]
    fn symbol_table_bind_and_resolve_with_nested_scopes() {
        let mut sym_table = SymbolTable::new();
        let symbols = vec![
            Symbol::new("a", types::DeclType::Integer, Scope::Global),
            Symbol::new("b", types::DeclType::String, Scope::Local),
            Symbol::new("c", types::DeclType::Boolean, Scope::Local),
            Symbol::new("d", types::DeclType::Integer, Scope::Local),
            Symbol::new("e", types::DeclType::Integer, Scope::Local),
        ];

        for sym in &symbols {
            sym_table.bind(sym.name(), sym.clone());
            sym_table.enter_scope();
        }

        for sym in &symbols {
            let symbol = sym_table.resolve(sym.name());
            assert!(!symbol.is_none());
        }
    }

    #[test]
    fn symbol_table_bind_and_resolve_from_inner_scope() {
        let mut sym_table = SymbolTable::new();
        let symbols = vec![
            Symbol::new("a", types::DeclType::Integer, Scope::Global),
            Symbol::new("b", types::DeclType::String, Scope::Local),
            Symbol::new("c", types::DeclType::Boolean, Scope::Local),
            Symbol::new("d", types::DeclType::Integer, Scope::Local),
            Symbol::new("e", types::DeclType::Integer, Scope::Local),
        ];

        for sym in &symbols {
            sym_table.bind(sym.name(), sym.clone());
            sym_table.enter_scope();
        }

        for sym in symbols.iter().rev() {
            let symbol = sym_table.resolve(sym.name());
            assert!(!symbol.is_none());
        }
    }
}
