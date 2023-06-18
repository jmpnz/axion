//! Code generation pipeline for axion programs.
use crate::ast;
use crate::sema;

/// Supported backends for code generation (currently supports x86 only).
enum CodeGenBackend {
    X86,
}

/// ScratchSpace is a table of scratch registers used to keep track of in use
/// and free registers during code generation.
struct ScratchSpace(u64, &'static str, bool);

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
                ScratchSpace(0, "rbx", false),
                ScratchSpace(1, "r10", false),
                ScratchSpace(2, "r11", false),
                ScratchSpace(3, "r12", false),
                ScratchSpace(4, "r13", false),
                ScratchSpace(5, "r14", false),
                ScratchSpace(6, "r15", false),
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

    /// Allocates a scratch register.
    fn allocate_scratch(&self) {}
}
