//! Modular IR construction and codegen for the Bril intermediate language
//! used to represent axion programs.

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
    // Text stream where instructions are written.
    // instructions:
    //
}
