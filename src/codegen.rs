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
    stream: Vec<String>,
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
            stream: Vec::new(),
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
    fn compile_expression(&self, expr: &ast::Expr) {}

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
                reg.name()
            })
    }

    /// Free a scratch register.
    fn free_scratch(&mut self, index: usize) {
        self.registers[index].in_use = false;
    }

    /// Create a new label.
    fn create_label(&mut self) -> String {
        self.label_counter += 1;
        format!(".L{}", self.label_counter)
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
}
