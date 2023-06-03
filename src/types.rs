//! Types module define the supported primitive types and type declarations.
//! The supported types are (int, string, char, boolean, array).

/// Before we can associate types to expressions we need to associate types
/// to declarations. The `DeclType` enum is used to associate types with
/// declarations.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DeclType {
    // Integer type is an atomic type to represent integers.
    Integer,
    // String type is an atomic type to represent immutable strings.
    String,
    // Char type is an atomic type to represent single byte characters.
    Char,
    // Boolean type is an atomic type to represent booleans.
    Boolean,
    // Array type is a compound type to represent static arrays.
    Array,
    // Function type is a type to represent functions.
    Function,
}

impl std::fmt::Display for DeclType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Integer => write!(f, "Type::Integer"),
            Self::String => write!(f, "Type::String"),
            Self::Char => write!(f, "Type::Char"),
            Self::Boolean => write!(f, "Type::Boolean"),
            Self::Array => write!(f, "Type::Array"),
            Self::Function => write!(f, "Type::Function"),
        }
    }
}
