//! Code generation pipeline for axion programs.
use crate::ast;
use crate::sema;

/// Supported backends for code generation (currently supports x86 only).
enum CodeGenBackend {
    X86,
}

/// `ScratchSpace` is a table of scratch registers used to keep track of in use
/// and free registers during code generation.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct ScratchSpace {
    index: usize,
    in_use: bool,
}

impl ScratchSpace {
    pub fn new(index: usize, name: &str) -> ScratchSpace {
        Self {
            index: index,
            in_use: false,
        }
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
}

/// `CodeGenerator` module runs a codegen pass on the AST emitting assembly.
/// Currently we only support x86-64, the targeted platform is Linux with
/// the System V ABI calling convention.
struct CodeGenerator {
    // Assembly stream where generated assembly is written.
    stream: String,
    // Scratch registers table, the scratch registers in the System V ABI are:
    // rbx, r10, r11, r12, r13, r14, r15.
    registers: Vec<ScratchSpace>,
    // Counter used to generate new labels.
    label_counter: u64,
}

impl CodeGenerator {
    #[must_use]
    pub fn new() -> Self {
        Self {
            stream: String::new(),
            registers: vec![
                ScratchSpace::new(0, "rbx"),
                ScratchSpace::new(1, "r10"),
                ScratchSpace::new(2, "r11"),
                ScratchSpace::new(3, "r12"),
                ScratchSpace::new(4, "r13"),
                ScratchSpace::new(5, "r14"),
                ScratchSpace::new(6, "r15"),
            ],
            label_counter: 0,
        }
    }

    /// Core code generation routine, uses the symbol table and AST to compile
    /// the program AST to assembly.
    pub fn codegen(&self, symtab: &sema::SymbolTable, ast: &[ast::Stmt]) {}

    /// Compile an expression.
    fn compile_expression(&mut self, expr: &ast::Expr) {
        match expr {
            ast::Expr::Literal(value) => match value {
                ast::LiteralValue::Int(v) => {
                    let maybe_reg = self.allocate_scratch();
                    match maybe_reg  {
                        Some(reg) => {
                            self.emit(&format!("movq ${}, {}", v, "rbx"));
                        },
                        None => panic!("unavailable scratch space"),
                    }
                },
                ast::LiteralValue::Str(s) => {
                    let label = self.create_label_name();
                    self.emit(&format!(".data"));
                    self.emit(&format!("{}:", label));
                    self.emit(&format!(".string \"{}\"", s));
                    self.emit(&format!(".text"));
                }
                _ => todo!(),
            },
            ast::Expr::Binary(op, lhs, rhs) => {
                self.compile_expression(lhs);
                self.compile_expression(rhs);
                match op {
                    ast::BinOp::Add => {
                        self.emit(&format!("addq"));
                    },
                    _ => todo!(),
                }

            }
            _ => todo!(),
        }
    }

    /// Compile a statement.
    fn compile_statement(&self, stmt: &ast::Stmt) {}

    /// Allocates a scratch register by finding the first unused register and
    /// returning its name.
    fn allocate_scratch(&mut self) -> Option<&str> {
        self.registers
            .iter_mut()
            .find(|reg| !reg.in_use)
            .map(|reg| {
                reg.in_use = true;
                reg.name().clone()
            })
    }

    /// Emit assembly instructions to the stream.
    fn emit(&mut self, inst: &str) {
        use std::fmt::Write;
        writeln!(self.stream, "{}", inst).unwrap();
    }

    /// Free a scratch register.
    fn free_scratch(&mut self, index: usize) {
        self.registers[index].in_use = false;
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_allocate_scratch_and_create_labels() {
        let mut codegen = CodeGenerator::new();
        let r1 = codegen.allocate_scratch();
        assert!(r1.is_some());
        assert_eq!(r1.unwrap(), "rbx");
        let r2 = codegen.allocate_scratch();
        assert!(r2.is_some());
        assert_eq!(r2.unwrap(), "r10");
        codegen.free_scratch(1);
        let r10 = codegen.allocate_scratch();
        assert!(r10.is_some());
        assert_eq!(r10.unwrap(), "r10");

        for i in 1..10 {
            let label = codegen.create_label();
            assert_eq!(format!(".L{}", i), label);
        }
    }

    #[test]
    fn can_codegen_an_expression() {
        let expr = ast::Expr::Literal(ast::LiteralValue::Str(
            "Hello World".to_string(),
        ));
        let mut codegen = CodeGenerator::new();
        codegen.compile_expression(&expr);
        println!("{}", codegen.stream);
    }

    #[test]
    fn can_codegen_a_binary_expression() {
        let expr = ast::Expr::Binary(
            ast::BinOp::Add,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(
                1,
            ))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(
                1,
            ))),
        );
        let mut codegen = CodeGenerator::new();
        codegen.compile_expression(&expr);
        println!("{}", codegen.stream);
    }
}
