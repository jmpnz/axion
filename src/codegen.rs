//! Code generation pipeline for axion programs.
use crate::ast;
use crate::sema;

/// Supported backends for code generation (currently supports x86 only).
enum CodeGenBackend {
    X86,
}

/// ScratchSpace is a table of scratch registers used to keep track of in use
/// and free registers during code generation.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct ScratchSpace {
    index: u64,
    in_use: bool,
}

impl ScratchSpace {
    pub fn new(index: u64, name: &str) -> ScratchSpace {
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
        }
    }

    /// Core code generation routine, uses the symbol table and AST to compile
    /// the program AST to assembly.
    pub fn codegen(&self, symtab: &sema::SymbolTable, ast: &Vec<ast::Stmt>) {}

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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allocate_scratch_space() {
        let mut codegen = CodeGenerator::new();
        let r1 = codegen.allocate_scratch();
        assert!(r1.is_some());
        assert_eq!(r1.unwrap(), "rbx");
    }
}
