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
    // Void type is used for functions that don't return a value.
    Void,
}

impl DeclType {
    /// Returns the `AtomicType` of the declaration if the declaration
    /// has an atomic type, else `None`.
    pub const fn atomic(&self) -> Option<AtomicType> {
        match self {
            Self::Integer => Some(AtomicType::Integer),
            Self::String => Some(AtomicType::String),
            Self::Char => Some(AtomicType::Char),
            Self::Boolean => Some(AtomicType::Boolean),
            _ => None,
        }
    }
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
            Self::Void => write!(f, "Type::Void"),
        }
    }
}

/// `AtomicType` is used to represent the atomic types in the langauge
/// all type algebra is implemented on atomic types and all expressions
/// resolve to an `AtomicType`.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AtomicType {
    Integer,
    String,
    Char,
    Boolean,
}

impl std::fmt::Display for AtomicType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Integer => write!(f, "Type::Integer"),
            Self::String => write!(f, "Type::String"),
            Self::Char => write!(f, "Type::Char"),
            Self::Boolean => write!(f, "Type::Boolean"),
        }
    }
}
