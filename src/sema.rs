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
    // Symbol position in the stack when it's a local variable or function
    // parameter. This is used during codegen to allocate in the stack.
    pos: usize,
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Symbol({}, {}, {:?})", self.name, self.t, self.scope)
    }
}

impl Symbol {
    // Construct a new symbol for a declaration.
    #[must_use]
    pub fn new(name: &str, t: types::DeclType, scope: SymbolKind) -> Self {
        Self {
            name: name.to_string(),
            t,
            ret: None,
            scope,
            pos: 0,
        }
    }

    // Construct a new symbol with a position.
    #[must_use]
    pub fn new_with_pos(
        name: &str,
        t: types::DeclType,
        scope: SymbolKind,
        pos: usize,
    ) -> Self {
        Self {
            name: name.to_string(),
            t,
            ret: None,
            scope,
            pos: pos,
        }
    }

    // Assign a position to a symbol.
    fn assign(&mut self, pos: usize) {
        self.pos = pos
    }

    // Construct a new symbol for a function declaration.
    #[must_use]
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
            pos: 0,
        }
    }

    // Returns the symbol name as an immutable reference.
    fn name(&self) -> &str {
        self.name.as_str()
    }

    // Returns the symbol type.
    pub fn t(&self) -> types::DeclType {
        self.t
    }

    // Returns the return type if its a function else `None`.
    const fn ret_t(&self) -> Option<types::DeclType> {
        self.ret
    }

    // Returns the symbol scope.
    pub fn scope(&self) -> SymbolKind {
        self.scope
    }

    // Returns the symbol position in the declarative stack.
    pub fn pos(&self) -> usize {
        self.pos
    }
}

/// `SymbolTable` represents a stack of symbol tables, the global scope
/// sits at `root_idx` and is always the 0th entry. Each time we enter
/// a new scope (scopes are enclosed within `ast::Stmt::Block`) we push
/// a new symbol table into the stack and all declarations within the
/// scope end up in the most recent symbol table.
#[derive(Debug, Clone)]
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
    #[must_use]
    pub fn new() -> Self {
        Self {
            root_idx: 0,
            curr_idx: 0,
            parent_idx: 0,
            tables: vec![HashMap::new()],
        }
    }

    // Resolve a new symbol.
    #[must_use]
    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        self.find(name, self.curr_idx)
    }

    // Checks if a symbol exists in the table, used to mainly for checking
    // semantic correctness in test cases.
    #[must_use]
    pub fn exists(&self, name: &str) -> Option<Symbol> {
        self.find(name, self.tables.len() - 1)
    }

    // Find a symbol by starting from the given index, the index should be in
    // the range of symbol tables stack..
    fn find(&self, name: &str, start: usize) -> Option<Symbol> {
        // Get a reference to the current scope we're processing
        let mut idx = start;
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
    // # Panics
    // When `name` is already bound, binding fails.
    pub fn bind(&mut self, name: &str, mut sym: Symbol) {
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
    #[must_use]
    pub const fn scope(&self) -> usize {
        self.curr_idx
    }

    // Return how many symbols exist in the current scope.
    #[must_use]
    fn symbol_count(&self) -> usize {
        self.tables[self.curr_idx].len()
    }

    // Returns the expected position in the stack starting from the current
    // scope.
    #[must_use]
    fn stack_position(&self) -> usize {
        // Walk backwards until the global scope adding up the symbol count.
        let mut sym_count = 0;
        let mut idx = self.curr_idx;
        // The global scope is at index 0
        while idx > 0 {
            sym_count += self.tables[idx].len();
            idx -= 1;
        }
        sym_count
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
    #[must_use]
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

    /// Returns the symbol table.
    pub fn symtable(&self) -> SymbolTable {
        self.sym_table.clone()
    }

    /// Get the expected scope of a symbol given our index in the symbol
    /// table stack.
    #[must_use]
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
                // If this is a local variable declaration we need to find
                // the number of existing variables in the current scope
                // so we can assign it a position.
                let pos = match scope {
                    SymbolKind::Local => self.sym_table.stack_position(),
                    _ => 0,
                };
                let mut sym = Symbol::new(name, *t, scope);
                sym.assign(pos);
                self.sym_table.bind(name, sym);
            }
            ast::Stmt::Function(name, t, _params, _body) => {
                let scope = self.scope();
                let sym = Symbol::new_function(
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
    pub fn resolve(&self, expr: &ast::Expr) {
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
                } else {
                    // If it's unknown fail.
                    panic!("Unknown variable assignment at {name}");
                }
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
                // Bind parameters to the local scope, assigning a position
                // to each symbol so we can resolve their position in the
                // stack later.
                for (index, param) in params.iter().enumerate() {
                    let mut sym =
                        Symbol::new(&param.0, param.1, SymbolKind::Param);
                    sym.assign(index);
                    self.sym_table.bind(&param.0, sym);
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
                (ast::UnaryOp::Not, types::AtomicType::Boolean) => {
                    types::AtomicType::Boolean
                }
                (ast::UnaryOp::Neg, types::AtomicType::Integer) => {
                    types::AtomicType::Integer
                }
                (op, t) => {
                    panic!("Type Error: unexpected operator {op} for {t}")
                }
            },
            ast::Expr::Binary(op, lhs, rhs) => {
                match (op, self.typecheck(lhs), self.typecheck(rhs)) {
                    (
                       ast::BinOp::Add
                        | ast::BinOp::Sub
                        | ast::BinOp::Mul
                        | ast::BinOp::Div,
                        types::AtomicType::Integer,
                        types::AtomicType::Integer,
                    ) => types::AtomicType::Integer,
                    (
                        ast::BinOp::Gt
                        | ast::BinOp::Gte
                        | ast::BinOp::Lt
                        | ast::BinOp::Lte
                        | ast::BinOp::Neq
                        | ast::BinOp::Equ,
                        types::AtomicType::Integer,
                        types::AtomicType::Integer,
                    ) => types::AtomicType::Boolean,

                    (_, _, _) => panic!(
                    "Unexpected token, expected arithmetic or comparison operator got {op}"
                ),
                }
            }
            ast::Expr::Logical(op, lhs, rhs) => {
                match (op, self.typecheck(lhs), self.typecheck(rhs)) {
                    (
                        ast::LogicalOp::And | ast::LogicalOp::Or,
                        types::AtomicType::Boolean,
                        types::AtomicType::Boolean,
                    ) => types::AtomicType::Boolean,

                    (op, l, r) => panic!(
                        "Expected Type::Boolean for {op} got left: {l} and right: {r}"
                    ),
                }
            }
            ast::Expr::Assign(name, expr) => {
                self.lookup(name).map_or_else(
                    || panic!("Variable {name} not found"),
                    |symbol| {
                        let Some(t) = symbol.t().atomic() else {
                        // TODO if t is Type::Function, resolve return type.
                         panic!("Unexpected assignment type {}",symbol.t())
                     };
                        let expr_t = self.typecheck(expr);
                        assert!(!(expr_t != t),
                 "Assignment of type {expr_t} to variable of type {t}");
                        expr_t
                    },
                )
            }
            ast::Expr::Var(name) => {
                if let Some(sym) = self.lookup(name) {
                    match sym.t() {
                        types::DeclType::Function => {
                            sym.ret_t().unwrap().atomic().unwrap()
                        }
                        _ => sym.t.atomic().unwrap(),
                    }
                } else {
                    panic!("Symbol {name} not found");
                }
            }
            ast::Expr::Grouping(expr) => self.typecheck(expr),
            ast::Expr::Call(callee, _args) => self.typecheck(callee),
            ast::Expr::Index(..) => todo!(),
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
                    let e : boolean = c;
                    let f : int = a + b;
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
            Symbol::new_with_pos(
                "a",
                types::DeclType::Integer,
                SymbolKind::Local,
                1,
            ),
            Symbol::new_with_pos(
                "b",
                types::DeclType::Integer,
                SymbolKind::Local,
                2,
            ),
            Symbol::new_with_pos(
                "c",
                types::DeclType::Boolean,
                SymbolKind::Local,
                3,
            ),
            Symbol::new_with_pos(
                "d",
                types::DeclType::Boolean,
                SymbolKind::Local,
                4,
            ),
            Symbol::new_with_pos(
                "e",
                types::DeclType::Boolean,
                SymbolKind::Local,
                5,
            ),
            Symbol::new_with_pos(
                "f",
                types::DeclType::Integer,
                SymbolKind::Local,
                6,
            ),
        ];

        for sym in expected {
            assert_eq!(sema.sym_table.exists(sym.name()), Some(sym));
        }
    }

    #[test]
    #[should_panic(
        expected = "Expected Type::Boolean for && got left: Type::Boolean and right: Type::Integer"
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
            assert_eq!(sema.sym_table.resolve(sym.name()), Some(sym));
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
    fn can_analyze_canonical_example() {
        let source = r#"
        let i:int = 42;
        function die(a:int) -> int {
           let accum:int = 0;
           if (a <= 42) {
                return 0;
           } else {
                a = a + 1;
            }
           return a;
        }
        function main() -> void {
            for(let i:int = 0;i < 10000;i = i + 1) {
                die(i);
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
