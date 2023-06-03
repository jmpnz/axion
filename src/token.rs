//! Top definition of B-minor tokens.

/// Literals in B-minor can be integers, booleans, characters or strings.
#[derive(Debug)]
pub enum Literal {
    Int,
    Bool,
    Char,
    Str,
}

/// Token is used to represent individual tokens in B-minor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    // Single character tokens.
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Caret,
    Percent,
    Bang,
    Equal,
    Or,
    And,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,
    EqualEqual,
    BangEqual,
    Minus,
    Decrement,
    Plus,
    Increment,
    Star,
    Slash,
    Comma,
    Semicolon,
    Colon,
    // Literal types.
    IntegerLiteral(i64),
    CharacterLiteral(char),
    StringLiteral(String),
    // Identifiers.
    Identifier(String),
    // Keywords.
    Integer,
    String,
    Boolean,
    Char,
    Array,
    If,
    Else,
    For,
    While,
    Void,
    Function,
    Return,
    True,
    False,
    Print,
    // End of file.
    Eof,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::LBracket => write!(f, "["),
            Self::RBracket => write!(f, "]"),
            Self::Caret => write!(f, "^"),
            Self::Percent => write!(f, "%"),
            Self::Bang => write!(f, "!"),
            Self::Equal => write!(f, "="),
            Self::Or => write!(f, "OR"),
            Self::And => write!(f, "AND"),
            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            Self::Lesser => write!(f, "<"),
            Self::LesserEqual => write!(f, "<="),
            Self::EqualEqual => write!(f, "=="),
            Self::BangEqual => write!(f, "!="),
            Self::Minus => write!(f, "-"),
            Self::Decrement => write!(f, "--"),
            Self::Plus => write!(f, "+"),
            Self::Increment => write!(f, "++"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Comma => write!(f, ","),
            Self::Semicolon => write!(f, ";"),
            Self::Colon => write!(f, ":"),
            Self::IntegerLiteral(v) => write!(f, "INTEGER({v})"),
            Self::CharacterLiteral(v) => write!(f, "CHARACTER({v})"),
            Self::StringLiteral(v) => write!(f, "STRING({v})"),
            Self::Identifier(s) => write!(f, "IDENT({s})"),
            Self::Integer => write!(f, "INTEGER"),
            Self::String => write!(f, "STRING"),
            Self::Boolean => write!(f, "BOOLEAN"),
            Self::Char => write!(f, "CHAR"),
            Self::Array => write!(f, "ARRAY"),
            Self::If => write!(f, "IF"),
            Self::Else => write!(f, "ELSE"),
            Self::For => write!(f, "FOR"),
            Self::While => write!(f, "WHILE"),
            Self::Void => write!(f, "VOID"),
            Self::Function => write!(f, "FUNCTION"),
            Self::Return => write!(f, "RETURN"),
            Self::True => write!(f, "TRUE"),
            Self::False => write!(f, "FALSE"),
            Self::Print => write!(f, "PRINT"),
            Self::Eof => write!(f, "EOF"),
            _ => write!(f, "TOKEN"),
        }
    }
}

/// Check if the given identifier is a B-minor keyword, if it's a keyword
/// return the respective token otherwise return `None`.
pub fn is_keyword(ident: &str) -> Option<Token> {
    match ident {
        "array" => Some(Token::Array),
        "boolean" => Some(Token::Boolean),
        "char" => Some(Token::Char),
        "else" => Some(Token::Else),
        "false" => Some(Token::False),
        "for" => Some(Token::For),
        "function" => Some(Token::Function),
        "if" => Some(Token::If),
        "integer" => Some(Token::Integer),
        "print" => Some(Token::Print),
        "return" => Some(Token::Return),
        "string" => Some(Token::String),
        "true" => Some(Token::True),
        "void" => Some(Token::Void),
        "while" => Some(Token::While),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokens_display() {
        let tok = Token::Eof;
        assert_eq!(tok.to_string(), "EOF");
    }

    #[test]
    fn is_keyword_checks() {
        assert_eq!(is_keyword("array"), Some(Token::Array));
        assert_eq!(is_keyword("boolean"), Some(Token::Boolean));
        assert_eq!(is_keyword("char"), Some(Token::Char));
        assert_eq!(is_keyword("else"), Some(Token::Else));
        assert_eq!(is_keyword("false"), Some(Token::False));
        assert_eq!(is_keyword("for"), Some(Token::For));
        assert_eq!(is_keyword("function"), Some(Token::Function));
        assert_eq!(is_keyword("if"), Some(Token::If));
        assert_eq!(is_keyword("integer"), Some(Token::Integer));
        assert_eq!(is_keyword("print"), Some(Token::Print));
        assert_eq!(is_keyword("return"), Some(Token::Return));
        assert_eq!(is_keyword("string"), Some(Token::String));
        assert_eq!(is_keyword("true"), Some(Token::True));
        assert_eq!(is_keyword("void"), Some(Token::Void));
        assert_eq!(is_keyword("while"), Some(Token::While));

        assert_eq!(is_keyword("var"), None);
        assert_eq!(is_keyword("square"), None);
        assert_eq!(is_keyword("x"), None);
        assert_eq!(is_keyword("i"), None);
    }
}
