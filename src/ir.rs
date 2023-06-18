//! Modular IR construction and codegen for the Bril intermediate language
//! used to represent axion programs.
//!
//! The following is an example of a program encoded in Bril
//! @main {
//!     v0: int = const 1;
//!     v1: int = const 2;
//!     v2: int = add v0 v1;
//!     v3: int = mul v2 v2;
//!     return;
//! }
//!
//! We can use the AST directly to generate assembly, but that would mean that
//! for each backend (instruction set) we have to support we will need to write
//! new code.
//! The AST structure is too rich and difficult to optimize, an IR on the other
//! hand is closer to assembly. .
//!
//! The first pass on the AST is done to create basic blocks (sequence of no
//! branching instructions). Instructions in Bril are semantically very close
//! to assembly but have a richer structure.
//!
//! There are three types of instructions :
//! - Constant ops : Which represent immediate values (constants)
//! - Value ops : Which represent expressions (assignment, arithemtic ops..)
//! - Effect ops: Which have side effects (conditional statements, loops..)
use crate::ast;
use crate::types;

/// `IRModule` is used to store instructions and metadata related to a program
/// module. Modules are a way to represents the intermediate representation of
/// a program. Each module stores a list of global variables and functions and
/// their instructions.
/// Every module must have at least one function "main" where initial execution
/// starts.
///
/// Modules depend on the symbol table and run a single codegen pass on an ast
/// to generate the initial intermediate representation.
///
/// Optimization passes are oblivious to the an `IRModule` and are implemented
/// as a trait that can be passed to the `optimize` function.
pub struct IRModule {
    globals: Vec<String>,
}

/// The IR uses a three address code format with at most three arguments, each
/// argument is an existing symbol reference (variable).
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct SymbolRef(usize);

/// Labels are used to name basic blocks.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label(String);

/// Imeediate represents immediate values.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Immediate {
    Int(i64),
    Chr(char),
    Str(String),
    Bool(bool),
}

/// Instructions in the IR.
#[derive(Debug, Clone, PartialEq, Eq)]
enum IROp {
    // Arithmetic ops
    Add(SymbolRef, SymbolRef),
    Sub(SymbolRef, SymbolRef),
    Mul(SymbolRef, SymbolRef),
    Div(SymbolRef, SymbolRef),
    // Comparison ops
    Gt(SymbolRef, SymbolRef),
    Gte(SymbolRef, SymbolRef),
    Lt(SymbolRef, SymbolRef),
    Lte(SymbolRef, SymbolRef),
    Equ(SymbolRef, SymbolRef),
    Neq(SymbolRef, SymbolRef),
    // Logic ops
    Not(SymbolRef),
    And(SymbolRef, SymbolRef),
    Or(SymbolRef, SymbolRef),
    // Control flow
    Jmp(Label /*Label*/),
    Br(
        SymbolRef, /*Label*/
        Label,     /*Label*/
        Label,     /*Label*/
    ),
    Call(SymbolRef, Vec<SymbolRef> /*Arguments*/),
    Ret(Option<SymbolRef> /*Return value*/),
}

/// `IROp` types encode the nature of instructions allowing us to reason
/// about the overall program structure.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum IROpKind {
    Const,
    Value,
    Effect,
}
