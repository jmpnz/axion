//! Code generation pipeline for axion programs.
use crate::ast;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::sema;
use crate::sema::{SemanticAnalyzer, Symbol};
use crate::types;

/// `ScratchSpace` is used to represent an x86 scratch registers and their
/// state during codegen.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct ScratchSpace {
    index: usize,
    in_use: bool,
}

/// `ScratchTable` is used to keep track of in-use scratch registers.
struct ScratchTable {
    registers: Vec<ScratchSpace>,
}

impl ScratchTable {
    /// Create a new scratch table.
    pub fn new() -> Self {
        Self {
            registers: vec![
                ScratchSpace::new(0, "rbx"),
                ScratchSpace::new(1, "r10"),
                ScratchSpace::new(2, "r11"),
                ScratchSpace::new(3, "r12"),
                ScratchSpace::new(4, "r13"),
                ScratchSpace::new(5, "r14"),
                ScratchSpace::new(6, "r15"),
            ],
        }
    }

    /// Allocates a scratch register by finding the first unused register and
    /// returning its name.
    fn allocate(&mut self) -> Option<ScratchSpace> {
        for reg in self.registers.iter_mut() {
            if !reg.in_use {
                reg.in_use = true;
                return Some(*reg);
            }
        }
        None
    }

    /// Free a scratch register.
    fn free(&mut self, index: usize) {
        self.registers[index].in_use = false;
    }
}

impl ScratchSpace {
    pub fn new(index: usize, name: &str) -> ScratchSpace {
        Self {
            index: index,
            in_use: false,
        }
    }

    /// Return the index of the scratch space.
    fn index(&self) -> usize {
        self.index
    }

    /// Return the register name given its index.
    fn name(&self) -> &str {
        match self.index {
            0 => "rbx",
            1 => "r10",
            2 => "r11",
            3 => "r12",
            4 => "r13",
            5 => "r14",
            6 => "r15",
            _ => panic!("Unknown register index : {0}", self.index),
        }
    }

    /// Return the abi name for the argument's register index.
    fn arg_name(index: usize) -> &'static str {
        match index {
            0 => "rdi",
            1 => "rsi",
            2 => "rdx",
            3 => "rcx",
            4 => "r8",
            5 => "r9",
            _ => panic!("Unknown register index : {0}", index),
        }
    }
}

/// `CodeGenerator` module runs a codegen pass on the AST emitting assembly.
/// Currently we only support x86-64, the targeted platform is Linux with
/// the System V ABI calling convention.
struct CodeGenerator {
    // Assembly stream where generated assembly is written.
    stream: String,
    // Scratch registers table, the scratch registers in the System V ABI are:
    // rbx, r10, r11, r12, r13, r14, r15.
    scratch: ScratchTable,
    // Symbol table.
    symtable: sema::SymbolTable,
    // Counter used to generate new labels.
    label_counter: u64,
}

impl CodeGenerator {
    #[must_use]
    pub fn new(symtable: sema::SymbolTable) -> Self {
        Self {
            stream: String::new(),
            scratch: ScratchTable::new(),
            label_counter: 0,
            symtable: symtable,
        }
    }

    /// Core code generation routine, uses the symbol table and AST to compile
    /// the program AST to assembly.
    pub fn codegen(&mut self, symtab: &sema::SymbolTable, ast: &[ast::Stmt]) {
        for stmt in ast {
            self.compile_statement(stmt);
        }
    }

    /// Compile an expression.
    fn compile_expression(&mut self, expr: &ast::Expr) -> Option<ScratchSpace> {
        match expr {
            ast::Expr::Literal(value) => match value {
                ast::LiteralValue::Int(v) => {
                    let maybe_reg = self.scratch.allocate();
                    match maybe_reg {
                        Some(reg) => {
                            self.emit(&format!("movq ${}, {}", v, reg.name()));
                            Some(reg)
                        }
                        None => panic!("unavailable scratch space"),
                    }
                }
                ast::LiteralValue::Char(v) => {
                    let maybe_reg = self.scratch.allocate();
                    match maybe_reg {
                        Some(reg) => {
                            self.emit(&format!("movq ${}, {}", v, reg.name()));
                            Some(reg)
                        }
                        None => panic!("unavailable scratch space"),
                    }
                }
                ast::LiteralValue::Boolean(v) => {
                    let maybe_reg = self.scratch.allocate();
                    match maybe_reg {
                        Some(reg) => {
                            self.emit(&format!("movq ${}, {}", v, reg.name()));
                            Some(reg)
                        }
                        None => panic!("unavailable scratch space"),
                    }
                }
                ast::LiteralValue::Str(s) => {
                    let label = self.create_label_name();
                    self.emit(&format!(".data"));
                    self.emit(&format!("{}:", label));
                    self.emit(&format!(".string \"{}\"", s));
                    self.emit(&format!(".text"));
                    None
                }
                _ => todo!(),
            },
            ast::Expr::Binary(op, lhs, rhs) => {
                let r0 = self.compile_expression(lhs).unwrap();
                let r1 = self.compile_expression(rhs).unwrap();
                match op {
                    ast::BinOp::Add => {
                        self.emit(&format!(
                            "addq {}, {}",
                            r1.name(),
                            r0.name()
                        ));
                    }
                    ast::BinOp::Sub => {
                        self.emit(&format!(
                            "subq {}, {}",
                            r1.name(),
                            r0.name()
                        ));
                    }
                    ast::BinOp::Div => {
                        self.emit(&format!("movq $0, %%rdx"));
                        self.emit(&format!("movq {}, %%rax", r0.name()));
                        self.emit(&format!("cqto"));
                        self.emit(&format!("idivq {}", r1.name()));
                        self.scratch.free(r1.index());
                        self.scratch.free(r0.index());
                        match self.scratch.allocate() {
                            Some(reg) => {
                                self.emit(&format!(
                                    "movq %%rax, {}",
                                    reg.name()
                                ));
                                return Some(reg);
                            }
                            None => panic!("unavailable scratch space"),
                        }
                    }
                    ast::BinOp::Mul => {
                        self.emit(&format!("movq {}, %%rax", r0.name()));
                        self.emit(&format!("imulq {}", r1.name()));
                        self.scratch.free(r1.index());
                        self.scratch.free(r0.index());
                        match self.scratch.allocate() {
                            Some(reg) => {
                                self.emit(&format!(
                                    "movq %%rax, {}",
                                    reg.name()
                                ));
                                return Some(reg);
                            }
                            None => panic!("unavailable scratch space"),
                        }
                    }
                    _ => {
                        self.emit(&format!("cmp {}, {}", r1.name(), r0.name()));
                        self.scratch.free(r1.index());
                        self.scratch.free(r0.index());

                        let reg = self.scratch.allocate().unwrap();
                        let label = self.create_label();

                        self.emit(&format!("movq $1, {}", reg.name()));
                        match op {
                            ast::BinOp::Gt => {
                                self.emit(&format!("jg {}", label));
                            }
                            ast::BinOp::Gte => {
                                self.emit(&format!("jge {}", label));
                            }
                            ast::BinOp::Lt => {
                                self.emit(&format!("jl {}", label));
                            }
                            ast::BinOp::Lte => {
                                self.emit(&format!("jle {}", label));
                            }
                            ast::BinOp::Equ => {
                                self.emit(&format!("je {}", label));
                            }
                            ast::BinOp::Neq => {
                                self.emit(&format!("jne {}", label));
                            }
                            _ => panic!("unexpected operatiion"),
                        }
                        self.emit(&format!("movq $0, {}", reg.name()));
                        self.emit(&format!("{}:", label));
                        return Some(reg);
                    }

                    _ => todo!(),
                }
                None
            }
            ast::Expr::Logical(op, lhs, rhs) => {
                let r0 = self.compile_expression(lhs).unwrap();
                let r1 = self.compile_expression(rhs).unwrap();
                match op {
                    ast::LogicalOp::And => {
                        self.emit(&format!(
                            "andq {}, {}",
                            r0.name(),
                            r1.name()
                        ));
                        Some(r1)
                    }
                    ast::LogicalOp::Or => {
                        self.emit(&format!("orq {}, {}", r0.name(), r1.name()));
                        Some(r1)
                    }
                }
            }
            ast::Expr::Unary(op, rhs) => {
                let r = self.compile_expression(rhs).unwrap();
                match op {
                    ast::UnaryOp::Neg => {
                        self.emit(&format!("neg {}", r.name()));
                        Some(r)
                    }
                    ast::UnaryOp::Not => {
                        let label = self.create_label();
                        self.emit(&format!("cmp $0, {}", r.name()));
                        self.emit(&format!("movq $1, {}", r.name()));
                        self.emit(&format!("je {}", label));
                        self.emit(&format!("movq $0, {}", r.name()));
                        self.emit(&format!("{}:", label));
                        Some(r)
                    }
                }
            }
            ast::Expr::Grouping(expr) => self.compile_expression(expr),
            ast::Expr::Assign(name, expr) => {
                let r = self.compile_expression(expr).unwrap();
                let sym = self.create_symbol(name);
                self.emit(&format!("movq {}, {}", r.name(), sym));
                Some(r)
            }
            ast::Expr::Var(name) => {
                let sym = self.resolve_symbol(name);
                let reg = self.scratch.allocate().unwrap();
                if sym.t() != types::DeclType::String
                    || sym.scope() != sema::SymbolKind::Global
                {
                    let s = self.create_symbol(name);
                    self.emit(&format!("movq {}, {}", s, reg.name()));
                    return Some(reg);
                }
                None
            }
            ast::Expr::Call(callee, args) => {
                self.emit(&format!("pushq %%r10"));
                self.emit(&format!("pushq %%r11"));
                // Prepare arguments
                for (index, arg) in args.iter().enumerate() {
                    let reg = self.compile_expression(arg).unwrap();
                    self.emit(&format!(
                        "movq {}, {}",
                        reg.name(),
                        ScratchSpace::arg_name(index)
                    ));
                    self.scratch.free(reg.index());
                }
                let func_name = match callee.as_ref() {
                    ast::Expr::Var(name) => name.clone(),
                    _ => panic!("unexpected call expression"),
                };
                self.emit(&format!("call {}", func_name));
                self.emit(&format!("popq %%r11"));
                self.emit(&format!("popq %%r10"));
                let reg = self.scratch.allocate().unwrap();
                self.emit(&format!("movq %%rax, {}", reg.name()));
                Some(reg)
            }
            _ => todo!(),
        }
    }

    /// Compile a statement.
    fn compile_statement(&mut self, stmt: &ast::Stmt) {
        match stmt {
            ast::Stmt::Expr(expr) => {
                let reg = self.compile_expression(&expr).unwrap();
                self.scratch.free(reg.index());
            }
            ast::Stmt::Return(expr) => {
                let reg = self.compile_expression(&expr).unwrap();
                self.emit(&format!("movq {}, %%rax", reg.name()));
                self.emit(&format!("popq %%rbp"));
                self.emit(&format!("ret"));
                self.scratch.free(reg.index());
            }
            ast::Stmt::Var(name, t, initializer) => {
                let sym = self.resolve_symbol(name);
                match (sym.scope(), sym.t()) {
                    (
                        sema::SymbolKind::Global,
                        types::DeclType::Integer
                        | types::DeclType::Char
                        | types::DeclType::Boolean,
                    ) => {
                        self.emit(&format!(".data"));
                        self.emit(&format!(".global {}", name));
                        match initializer {
                            Some(expr) => match expr {
                                ast::Expr::Literal(value) => match value {
                                    ast::LiteralValue::Int(v) => {
                                        self.emit(&format!(
                                            "{}: .quad {}",
                                            name, v
                                        ));
                                    }
                                    ast::LiteralValue::Str(v) => {
                                        self.emit(&format!(
                                            "{}: .string {}",
                                            name, v
                                        ));
                                    }
                                    ast::LiteralValue::Char(v) => {
                                        self.emit(&format!(
                                            "{}: .quad {}",
                                            name, v
                                        ));
                                    }
                                    ast::LiteralValue::Boolean(v) => {
                                        self.emit(&format!(
                                            "{}: .quad {}",
                                            name, v
                                        ));
                                    }
                                },
                                _ => {
                                    self.compile_expression(expr);
                                }
                            },
                            None => self.emit(&format!("{}: .quad 0", name)),
                        }
                    }
                    (_, _) => todo!(),
                }
            }
            ast::Stmt::Function(name, t, params, stmts) => {
                self.emit(&format!(".text"));
                self.emit(&format!(".global {}", name));
                self.emit(&format!("{}:", name));
                // prologue
                self.emit(&format!("pushq %%rbp"));
                self.emit(&format!("movq %%rsp, %%rbp"));

                let mut args = Vec::new();
                for (index, _) in params.iter().enumerate() {
                    self.emit(&format!(
                        "pushq {}",
                        ScratchSpace::arg_name(index)
                    ));
                    args.push(ScratchSpace::arg_name(index));
                }
                // allocate space on the stack of local variables.
                let mut num_args = 0;
                for stmt in stmts {
                    match stmt {
                        ast::Stmt::Var(..) => num_args += 1,
                        _ => continue,
                    }
                }
                self.emit(&format!("subq ${}, %%rsp", num_args * 8));
                // save callee-saved registers
                self.emit(&format!("pushq %%rbx"));
                self.emit(&format!("pushq %%r12"));
                self.emit(&format!("pushq %%r13"));
                self.emit(&format!("pushq %%r14"));
                self.emit(&format!("pushq %%r15"));

                for stmt in stmts {
                    self.compile_statement(&stmt);
                }

                for arg in args.iter().rev() {
                    self.emit(&format!("popq {}", arg));
                }

                self.emit(&format!("popq %%r15"));
                self.emit(&format!("popq %%r14"));
                self.emit(&format!("popq %%r13"));
                self.emit(&format!("popq %%r12"));
                self.emit(&format!("popq %%rbx"));

                self.emit(&format!("movq %%rbp, %%rsp"));
                self.emit(&format!("popq %%rbp"));
                self.emit(&format!("ret"));
            }
            _ => todo!(),
        }
    }

    /// Emit assembly instructions to the stream.
    fn emit(&mut self, inst: &str) {
        use std::fmt::Write;
        writeln!(self.stream, "{}", inst).unwrap();
    }

    /// Create a new code label.
    fn create_label(&mut self) -> String {
        self.label_counter += 1;
        format!(".L{}", self.label_counter)
    }

    /// Create a new global string label.
    fn create_label_name(&mut self) -> String {
        self.label_counter += 1;
        format!("_S{}", self.label_counter)
    }

    /// Resolve a symbol from the symbtable.
    fn resolve_symbol(&self, name: &str) -> Symbol {
        match self.symtable.resolve(name) {
            Some(sym) => sym,
            None => panic!("symbol {} not found", name),
        }
    }

    /// Resolve a symbol from the symbol table creating its equivalent codegen.
    fn create_symbol(&self, name: &str) -> String {
        match self.symtable.resolve(name) {
            Some(sym) => match sym.scope() {
                sema::SymbolKind::Global => name.to_string(),
                sema::SymbolKind::Local | sema::SymbolKind::Param => {
                    format!("-%{}(%%rbp)", sym.pos() * 8)
                }
            },
            None => panic!("symbol {} not found", name),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_allocate_scratch_and_create_labels() {
        let mut scratch = ScratchTable::new();
        let r1 = scratch.allocate();
        assert!(r1.is_some());
        assert_eq!(r1.unwrap().name(), "rbx");
        let r2 = scratch.allocate();
        assert!(r2.is_some());
        assert_eq!(r2.unwrap().name(), "r10");
        scratch.free(1);
        let r10 = scratch.allocate();
        assert!(r10.is_some());
        assert_eq!(r10.unwrap().name(), "r10");
    }

    #[test]
    fn can_codegen_an_expression() {
        let expr = ast::Expr::Literal(ast::LiteralValue::Str(
            "Hello World".to_string(),
        ));
        let sem_analyzer = sema::SemanticAnalyzer::new();
        let mut codegen = CodeGenerator::new(sem_analyzer.symtable());
        codegen.compile_expression(&expr);
        println!("{}", codegen.stream);
    }

    #[test]
    fn can_codegen_a_binary_expression() {
        let expr = ast::Expr::Binary(
            ast::BinOp::Add,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(1))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(1))),
        );
        let sem_analyzer = sema::SemanticAnalyzer::new();
        let mut codegen = CodeGenerator::new(sem_analyzer.symtable());
        codegen.compile_expression(&expr);
        println!("{}", codegen.stream);
    }

    #[test]
    fn can_codegen_basic_block() {
        let source = r#"
            let a : int = 5;
            let b : int = 2;
            let c : int = a + b;
            function test() -> int {
                let a : int = 3;
            }
        "#;
        let mut lexer = Lexer::new(source);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();
        let mut sema = SemanticAnalyzer::new();
        sema.run(&ast);
        let mut codegen = CodeGenerator::new(sema.symtable());
        codegen.codegen(&sema.symtable(), &ast);
        println!("{}", codegen.stream);
    }
}
