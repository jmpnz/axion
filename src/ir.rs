//! Modular IR construction and codegen for the Bril intermediate language
//! used to represent axion programs.
//! The following is an example of a program encoded in Bril
//! @main {
//!     v0: int = const 1;
//!     v1: int = const 2;
//!     v2: int = add v0 v1;
//!     v3: int = mul v2 v2;
//!     return;
//! }

/// `IRModule` is used to store instructions and metadata related to a program
/// module. Modules are a way to represents the intermediate representation of
/// a program. Each module stores a list of global variables and functions and
/// their instructions.
///
/// Modules depend on the symbol table and run a single codegen pass on an ast
/// to generate the initial intermediate representation.
///
/// Optimization passes are oblivious to the an `IRModule` and are implemented
/// as a trait that can be passed to the `optimize` function.
pub struct IRModule {
}

/// `BrilOp` encodes the bril core language operations.
pub enum BrilOp {
    Add,
    Mul,
    Sub,
    Div,
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
    Not,
    And,
    Or,
    Jump,
    Branch,
    Call,
    Return,
}
