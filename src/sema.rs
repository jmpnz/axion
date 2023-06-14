//! Semantic analyzer responsible for doing semantic analysis, type checking
//! and rewriting the AST to include type annotations for the next step.
use crate::ast;
use crate::token;
use crate::types;

use std::collections::HashMap;

/// `SymbolKind` defines the scope of a symbol.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    Local,
    Global,
    Param,
}

/// `Symbol` represents a symbol in the AST. Symbols include variables
/// and functions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    // Symbol name.
    name: String,
    // Symbol type.
    t: types::DeclType,
    // Optional return type for functions.
    ret: Option<types::DeclType>,
    // Symbol scope.
    scope: SymbolKind,
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Symbol({}, {}, {:?})", self.name, self.t, self.scope)
    }
}

impl Symbol {
    // Construct a new symbol for a declaration.
    pub fn new(name: &str, t: types::DeclType, scope: SymbolKind) -> Self {
        Self {
            name: name.to_string(),
            t,
            ret: None,
            scope,
        }
    }
    // Construct a new symbol for a function declaration.
    pub fn new_function(
        name: &str,
        t: types::DeclType,
        ret: types::DeclType,
        scope: SymbolKind,
    ) -> Self {
        Self {
            name: name.to_string(),
            t,
            ret: Some(ret),
            scope,
        }
    }

    // Returns the symbol name as an immutable reference.
    fn name(&self) -> &str {
        self.name.as_str()
    }

    // Returns the symbol type.
    const fn t(&self) -> types::DeclType {
        self.t
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

    // Check if a symbol exists in the symbol table.
    fn exists(&self, name: &str) -> Option<Symbol> {
        // Get a reference to the last table in the stack.
        let mut idx = self.tables.len() - 1;
        let mut current_scope_table = &self.tables[idx];
        loop {
            // Try and find the declaration in the current scope.
            for (ident, symbol) in current_scope_table {
                if ident == name {
                    return Some(symbol.clone());
                }
            }
            // If we didn't find the declaration in the current scope
            // we check the parent.
            if idx.checked_sub(1).is_some() {
                idx -= 1;
                current_scope_table = &self.tables[idx];
            } else {
                break;
            }
        }
        None
    }

    // Resolve a new symbol.
    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        // Get a reference to the current scope we're processing
        let mut idx = self.curr_idx;
        let mut current_scope_table = &self.tables[idx];
        loop {
            // Try and find the declaration in the current scope.
            for (ident, symbol) in current_scope_table {
                if ident == name {
                    return Some(symbol.clone());
                }
            }
            // If we didn't find the declaration in the current scope
            // we check the parent.
            if idx.checked_sub(1).is_some() {
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
        let tbl = &mut self.tables[self.curr_idx];
        match tbl.get(name) {
            Some(_value) => {
                panic!("Name {name} is already bound to {sym}")
            }
            None => {
                tbl.insert(name.to_string(), sym);
            }
        }
    }

    // Returns a view of the symbol tables.
    const fn tables(&self) -> &Vec<HashMap<String, Symbol>> {
        &self.tables
    }

    // Return the index of the current scope.
    pub const fn scope(&self) -> usize {
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
            self.parent_idx -= 1;
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
    // Symbol table created during semantic analysis, it collects all
    // the existing symbols (variables and functions), their types
    // and their scopes.
    pub sym_table: SymbolTable,
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            sym_table: SymbolTable::new(),
        }
    }

    /// Returns a view the symbol table.
    #[must_use]
    pub const fn sym_table(&self) -> &Vec<HashMap<String, Symbol>> {
        self.sym_table.tables()
    }

    /// Get the expected scope of a symbol given our index in the symbol
    /// table stack.
    const fn scope(&self) -> SymbolKind {
        match self.sym_table.scope() {
            0 => SymbolKind::Global,
            _ => SymbolKind::Local,
        }
    }

    /// Define a new binding given a declaration.
    fn define(&mut self, stmt: &ast::Stmt) {
        match stmt {
            ast::Stmt::Var(name, t, _value) => {
                let scope = self.scope();
                self.sym_table.bind(name, Symbol::new(name, *t, scope));
            }
            ast::Stmt::Function(name, t, _params, _body) => {
                let scope = self.scope();
                let mut sym = Symbol::new_function(
                    name,
                    types::DeclType::Function,
                    *t,
                    scope,
                );
                self.sym_table.bind(name, sym);
            }
            _ => panic!("Expected declaration found statement : {stmt:?}"),
        }
    }

    /// Lookup an existing binding by name, returns a `Symbol`
    /// if one is found otherwise none.
    fn lookup(&self, name: &str) -> Option<Symbol> {
        self.sym_table.resolve(name)
    }

    /// Resolve an expression to check if it was properly defined.
    fn resolve(&self, expr: &ast::Expr) {
        match expr {
            // Nothing to do for literals
            ast::Expr::Literal(_) | ast::Expr::Index(_, _) => (),
            // Check if the variable expression was properly bound.
            ast::Expr::Unary(_, rhs) => self.resolve(rhs),
            ast::Expr::Binary(_, lhs, rhs)
            | ast::Expr::Logical(_, lhs, rhs) => {
                self.resolve(lhs);
                self.resolve(rhs);
            }
            ast::Expr::Assign(name, rhs) => {
                // Check if the assignee is properly defined
                if self.sym_table.resolve(name).is_some() {
                    // Assignee is properly defined try and resolve the rhs.
                    self.resolve(rhs);
                }
                // If it's unknown fail.
                panic!("Unknown variable assignment at {name}");
            }
            ast::Expr::Var(name) => match self.sym_table.resolve(name) {
                Some(_) => (),
                None => panic!("Unknown variable {name}"),
            },
            ast::Expr::Grouping(group) => {
                self.resolve(group);
            }
            ast::Expr::Call(callee, args) => {
                self.resolve(callee);
                for arg in args {
                    self.resolve(arg);
                }
            }
        }
    }

    /// Core analysis routine, builds the symbol table and collect semantic
    /// errors to display later.
    fn analyze(&mut self, stmt: &ast::Stmt) {
        match stmt {
            ast::Stmt::Var(_, _, expr) => {
                self.define(stmt);
                if let Some(expr) = expr {
                    self.typecheck(expr);
                }
            }
            ast::Stmt::Function(_, _, params, ref body) => {
                self.define(stmt);
                self.sym_table.enter_scope();
                // Bind parameters to the local scope
                for param in params {
                    self.sym_table.bind(
                        &param.0,
                        Symbol::new(&param.0, param.1, SymbolKind::Param),
                    );
                }
                for stmt in body {
                    self.analyze(stmt);
                }
                self.sym_table.leave_scope();
            }
            ast::Stmt::Block(stmts) => {
                self.sym_table.enter_scope();
                for stmt in stmts {
                    self.analyze(stmt);
                }
                self.sym_table.leave_scope();
            }
            ast::Stmt::If(expr, then_branch, else_branch) => {
                self.resolve(expr);
                assert_eq!(
                    self.typecheck(expr),
                    types::AtomicType::Boolean,
                    "Expression within `If` condition should be boolean",
                );
                self.typecheck(expr);
                self.analyze(then_branch);
                else_branch
                    .as_ref()
                    .map_or((), |branch| self.analyze(branch));
            }
            ast::Stmt::While(expr, body) => {
                self.resolve(expr);
                self.analyze(body);
                assert_eq!(
                    self.typecheck(expr),
                    types::AtomicType::Boolean,
                    "Expression within `While` condition should be boolean",
                );
            }
            ast::Stmt::Expr(expr) | ast::Stmt::Return(expr) => {
                self.resolve(expr);
                self.typecheck(expr);
            }
            ast::Stmt::Break => (),
        }
    }

    /// Run semantic analysis pass on the given AST.
    pub fn run(&mut self, ast: &Vec<ast::Stmt>) {
        for stmt in ast {
            self.analyze(stmt);
        }
    }

    /// Resolve the type of an expression, the type system we implement
    /// is very simple and enforces the following rules.
    /// - Values can only be assigned to variables of the same type.
    /// - Function parameters can only accept a value of the same type.
    /// - Return statements bind to the type of the returned values, the type must
    /// match the return type of the function.
    /// - All binary operators must have the same type on the lhs and rhs.
    /// - The equality operators can be applied to any type except `Void`
    /// and `Function` and always return a boolean.
    /// - The comparison operators can only be applied to integer values
    /// and always return boolean.
    /// - The boolean operators (!, ||, &&) can only be applied to boolean
    /// values and always return boolean.
    /// - The arithmetic operators can only be applied to integer values
    /// and always return an integer..
    fn typecheck(&self, expr: &ast::Expr) -> types::AtomicType {
        match expr {
            ast::Expr::Literal(value) => match value {
                ast::LiteralValue::Int(_) => types::AtomicType::Integer,
                ast::LiteralValue::Str(_) => types::AtomicType::String,
                ast::LiteralValue::Char(_) => types::AtomicType::Char,
                ast::LiteralValue::Boolean(_) => types::AtomicType::Boolean,
            },
            ast::Expr::Unary(op, expr) => match (op, self.typecheck(expr)) {
                (token::Token::Bang, types::AtomicType::Boolean) => {
                    types::AtomicType::Boolean
                }
                (token::Token::Minus, types::AtomicType::Integer) => {
                    types::AtomicType::Integer
                }
                (_, t) => panic!("Type Error: unexpected type {t} for {op}"),
            },
            ast::Expr::Binary(op, lhs, rhs) => {
                match (op, self.typecheck(lhs), self.typecheck(rhs)) {
                    (
                        token::Token::Plus
                        | token::Token::Minus
                        | token::Token::Slash
                        | token::Token::Star,
                        types::AtomicType::Integer,
                        types::AtomicType::Integer,
                    ) => types::AtomicType::Integer,

                    (_, _, _) => panic!(
                    "Unexpected token, expected arithmetic operator got {op}"
                ),
                }
            }
            ast::Expr::Logical(op, lhs, rhs) => {
                match (op, self.typecheck(lhs), self.typecheck(rhs)) {
                    (
                        token::Token::And | token::Token::Or,
                        types::AtomicType::Boolean,
                        types::AtomicType::Boolean,
                    ) => types::AtomicType::Boolean,

                    (_, l, r) => panic!(
                        "Expected Type::Boolean got left: {l} and right: {r}"
                    ),
                }
            }
            ast::Expr::Assign(name, expr) => {
                let sym = self.lookup(name);

                sym.map_or_else(
                    || panic!("Variable {name} not found"),
                    |symbol| {
                        let Some(t) = symbol.t().atomic() else {
                         panic!("Unexpected assignment type {}",symbol.t())
                     };
                        let expr_t = self.typecheck(expr);
                        assert!(!(expr_t != t),
                 "Assignment of type {expr_t} to variable of type {t}");
                        expr_t
                    },
                )
            }
            ast::Expr::Var(name) => self.lookup(name).map_or_else(
                || panic!("Variable {name} not found"),
                |sym| {
                    let Some(t) = sym.t().atomic() else {
                             panic!("Unexpected assignment type {}",sym.t())
                         };
                    t
                },
            ),
            ast::Expr::Grouping(expr) => self.typecheck(expr),
            ast::Expr::Call(callee, args) => self.typecheck(callee),
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::lexer::Lexer;
    use crate::parser::Parser;

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
                    &Symbol::new(
                        "i",
                        types::DeclType::Integer,
                        SymbolKind::Global
                    )
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
    }

    #[test]
    #[should_panic]
    fn fail_analysis_when_operator_type_mismatch() {
        let source = "let i:int = !42;\n let a:boolean = -true;";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();
        let mut sema = SemanticAnalyzer::new();
        sema.run(&ast);
    }

    #[test]
    #[should_panic(
        expected = "Name a is already bound to Symbol(a, Type::Integer, Local)"
    )]
    fn fail_to_build_symbol_table_when_redefining_param() {
        let source = r#"
        let i:int = 42;
        function die(a:int) -> void {
            // a is redefined here.
            let a : int = 0;
            let b : int = 1;
            let c : boolean = true;
            let d : boolean = c;

            {
                let e : int = a + b;
            }
        }
        "#;
        let mut lexer = Lexer::new(source);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();
        let mut sema = SemanticAnalyzer::new();
        sema.run(&ast);
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
                    let c : boolean = c;
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

        let expected = vec![
            Symbol::new("i", types::DeclType::Integer, SymbolKind::Global),
            Symbol::new_function(
                "die",
                types::DeclType::Function,
                types::DeclType::Void,
                SymbolKind::Global,
            ),
            Symbol::new("a", types::DeclType::Integer, SymbolKind::Local),
            Symbol::new("b", types::DeclType::Integer, SymbolKind::Local),
            Symbol::new("c", types::DeclType::Boolean, SymbolKind::Local),
            Symbol::new("d", types::DeclType::Boolean, SymbolKind::Local),
            Symbol::new("e", types::DeclType::Integer, SymbolKind::Local),
        ];

        for sym in expected {
            assert_eq!(sema.sym_table.exists(sym.name()), Some(sym));
        }
    }

    #[test]
    #[should_panic(
        expected = "Expected Type::Boolean got left: Type::Boolean and right: Type::Integer"
    )]
    fn fails_when_expression_has_type_error() {
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
                    let f : boolean = !c;
                    let g : boolean = c && a;
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

        let expected = vec![
            Symbol::new("i", types::DeclType::Integer, SymbolKind::Global),
            Symbol::new_function(
                "die",
                types::DeclType::Function,
                types::DeclType::Void,
                SymbolKind::Global,
            ),
            Symbol::new("a", types::DeclType::Integer, SymbolKind::Local),
            Symbol::new("b", types::DeclType::Integer, SymbolKind::Local),
            Symbol::new("c", types::DeclType::Boolean, SymbolKind::Local),
            Symbol::new("d", types::DeclType::Boolean, SymbolKind::Local),
            Symbol::new("e", types::DeclType::Integer, SymbolKind::Local),
            Symbol::new("f", types::DeclType::Boolean, SymbolKind::Local),
            Symbol::new("g", types::DeclType::Boolean, SymbolKind::Local),
        ];

        for sym in expected {
            assert_eq!(sema.sym_table.exists(sym.name()), Some(sym));
        }
    }

    #[test]
    #[should_panic]
    fn fails_when_if_condition_is_not_boolean() {
        let source = r#"
        let i:int = 42;
        function die(a:int) -> void {
            {
                if (42) {
                    // do something
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
    }

    #[test]
    #[should_panic]
    fn fails_when_while_condition_is_not_boolean() {
        let source = r#"
        let i:int = 42;
        function die(a:int) -> void {
            {
                while ("Hello") {
                    // do something
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
    }

    #[test]
    fn symbol_table_bind_and_resolve() {
        let mut sym_table = SymbolTable::new();
        let symbols = vec![
            Symbol::new("a", types::DeclType::Integer, SymbolKind::Global),
            Symbol::new("b", types::DeclType::String, SymbolKind::Global),
            Symbol::new("c", types::DeclType::Boolean, SymbolKind::Local),
            Symbol::new("d", types::DeclType::Integer, SymbolKind::Local),
            Symbol::new("e", types::DeclType::Integer, SymbolKind::Local),
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
            assert_ne!(symbol, None);
        }
    }

    #[test]
    fn symbol_table_bind_and_resolve_with_nested_scopes() {
        let mut sym_table = SymbolTable::new();
        let symbols = vec![
            Symbol::new("a", types::DeclType::Integer, SymbolKind::Global),
            Symbol::new("b", types::DeclType::String, SymbolKind::Local),
            Symbol::new("c", types::DeclType::Boolean, SymbolKind::Local),
            Symbol::new("d", types::DeclType::Integer, SymbolKind::Local),
            Symbol::new("e", types::DeclType::Integer, SymbolKind::Local),
        ];

        for sym in &symbols {
            sym_table.bind(sym.name(), sym.clone());
            sym_table.enter_scope();
        }

        for sym in &symbols {
            let symbol = sym_table.resolve(sym.name());
            assert_ne!(symbol, None);
            assert_eq!(&symbol.unwrap(), sym);
        }
    }

    #[test]
    fn symbol_table_bind_and_resolve_from_inner_scope() {
        let mut sym_table = SymbolTable::new();
        let symbols = vec![
            Symbol::new("a", types::DeclType::Integer, SymbolKind::Global),
            Symbol::new("b", types::DeclType::String, SymbolKind::Local),
            Symbol::new("c", types::DeclType::Boolean, SymbolKind::Local),
            Symbol::new("d", types::DeclType::Integer, SymbolKind::Local),
            Symbol::new("e", types::DeclType::Integer, SymbolKind::Local),
        ];

        for sym in &symbols {
            sym_table.bind(sym.name(), sym.clone());
            sym_table.enter_scope();
        }

        for sym in symbols.iter().rev() {
            let symbol = sym_table.resolve(sym.name());
            assert_ne!(symbol, None);
            assert_eq!(&symbol.unwrap(), sym);
        }
    }
}
