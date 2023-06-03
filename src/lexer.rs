//! Simple lexical engine to process axion source code into tokens.
//! The engine works by iterating over individual characters and updating
//! it's internal state machine.
//! On failure the `lex` method returns a lexical error.
use crate::token::{is_keyword, Token};
use std::error::Error;
use std::fmt;

/// Lexical engine used to process B-minor source code into tokens.
/// `Lexer` is a very simple scanner style lexical engine that processes
/// individual lexemes at the character level.
pub struct Lexer {
    /// Source code to process, split at individual chars.
    source: Vec<char>,
    /// Position at the beginning of the next lexeme.
    start: usize,
    /// Current position of the lexer in the source.
    pos: usize,
    /// Current line in the source code, used for error reporting.
    line: usize,
    /// Processed tokens.
    tokens: Vec<Token>,
}

impl Lexer {
    /// Creates a new lexer instance from a given `source` string.
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            start: 0,
            pos: 0,
            line: 0,
            tokens: vec![],
        }
    }

    /// Lex the passed source code and returns a list of tokens.
    pub fn lex(&mut self) -> Result<Vec<Token>, LexError> {
        while let Some(ch) = self.next() {
            // Grab the first lexeme as we will need to used it to scan
            // multi-character tokens such as identifiers, numbers and strings
            self.start = self.pos - 1;
            match ch {
                '&' if self.eat('&') => self.tokens.push(Token::And),
                '|' if self.eat('|') => self.tokens.push(Token::Or),
                '!' if self.eat('=') => self.tokens.push(Token::BangEqual),
                '=' if self.eat('=') => self.tokens.push(Token::EqualEqual),
                '>' if self.eat('=') => self.tokens.push(Token::GreaterEqual),
                '<' if self.eat('=') => self.tokens.push(Token::LesserEqual),
                '-' if self.eat('>') => self.tokens.push(Token::Arrow),
                '/' if self.eat('/') || self.eat('*') => self.eat_comment(),
                '(' => self.tokens.push(Token::LParen),
                ')' => self.tokens.push(Token::RParen),
                '{' => self.tokens.push(Token::LBrace),
                '}' => self.tokens.push(Token::RBrace),
                '[' => self.tokens.push(Token::LBracket),
                ']' => self.tokens.push(Token::RBracket),
                ';' => self.tokens.push(Token::Semicolon),
                ':' => self.tokens.push(Token::Colon),
                ',' => self.tokens.push(Token::Comma),
                '<' => self.tokens.push(Token::Lesser),
                '>' => self.tokens.push(Token::Greater),
                '=' => self.tokens.push(Token::Equal),
                '!' => self.tokens.push(Token::Bang),
                '*' => self.tokens.push(Token::Star),
                '/' => self.tokens.push(Token::Slash),
                '+' => self.tokens.push(Token::Plus),
                '-' => self.tokens.push(Token::Minus),
                '"' => self.string(),
                '\'' => self.char(),
                '0'..='9' => self.integer(),
                '_' | 'a'..='z' | 'A'..='Z' => self.identifier(),
                // Do nothing on whitespace.
                ' ' | '\r' | '\t' => (),
                // Increment line number on newlines.
                '\n' => self.line += 1,
                _ => {
                    return Err(LexError::new(
                        self.line,
                        format!("Unrecognized token {ch}"),
                    ))
                }
            }
        }
        self.tokens.push(Token::Eof);
        Ok(self.tokens.clone())
    }

    // Return next char and increment cursor position.
    fn next(&mut self) -> Option<char> {
        // Refactor to iterator style
        if !self.eof() {
            self.pos += 1;
            return Some(self.source[self.pos - 1]);
        }
        None
    }

    // Match current character, advancing the cursor if we match `expected`.
    fn eat(&mut self, expected: char) -> bool {
        // TODO: This shouldn't be here, we should instead return an error
        // in the match arm when we don't recognize the next token.
        if self.eof() || self.source[self.pos] != expected {
            return false;
        }
        self.pos += 1;
        true
    }

    // Eat comments.
    fn eat_comment(&mut self) {
        while self.peek() != '\n' && !self.eof() {
            // Omit return value since we don't process comments
            self.next();
        }
    }

    // Peek next character without advancing the cursor
    fn peek(&self) -> char {
        if self.eof() {
            return '\0';
        }
        self.source[self.pos]
    }

    // Scan integer literal.
    fn integer(&mut self) {
        while self.peek().is_ascii_digit() {
            self.next();
        }

        let s = self.source[self.start..self.pos].iter().collect::<String>();
        let value = s.as_str().parse::<i64>().unwrap();
        self.tokens.push(Token::IntegerLiteral(value));
    }

    // Scan string literals enclosed in double quotes.
    fn string(&mut self) {
        while self.peek() != '"' && !self.eof() {
            // Handle multiline strings
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.next();
        }
        // Consume closing quote
        self.next();
        // Trim surrounding quotes and build the string literal.
        let s = self.source[self.start + 1..self.pos - 1]
            .iter()
            .collect::<String>();
        self.tokens.push(Token::StringLiteral(s));
    }
    // Scan literal characters enclosed in single quotes.
    fn char(&mut self) {
        while self.peek() != '\'' && !self.eof() {
            self.next();
        }
        // Consume closing quote.
        self.next();
        // Trim surrounding quotes and build the char literal.
        let c = self.source[self.start + 1];
        self.tokens.push(Token::CharacterLiteral(c));
    }

    // Scan identifiers.
    fn identifier(&mut self) {
        while self.peek().is_ascii_alphanumeric() {
            self.next();
        }

        let s = self.source[self.start..self.pos].iter().collect::<String>();
        if let Some(keyword) = is_keyword(&s) {
            self.tokens.push(keyword);
        } else {
            self.tokens.push(Token::Identifier(s));
        }
    }

    // Check if we reached the end of the source.
    fn eof(&self) -> bool {
        self.pos >= self.source.len()
    }
}

/// Lex error type is used to report scanning errors to the user.
#[derive(Debug, Clone)]
pub struct LexError {
    details: String,
    line: usize,
}

impl LexError {
    // To build a `LexError` we pass the line in the source where the error
    // occured and an error message with more details.
    // An idiomatic pattern in Rust is to pass `&str` instead of `String`
    // in this case `LexError` is an owner so we can move the message here.
    // It's also helpful since we use `format!` to build the error message
    // and it already returns a `String` making it unnecessary to call `as_str`
    // and then clone the underlying.
    const fn new(line: usize, details: String) -> Self {
        Self { details, line }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} at line {}.", self.details, self.line)
    }
}

impl Error for LexError {
    fn description(&self) -> &str {
        &self.details
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Macro to generate test cases.
    macro_rules! test_lexer {
        ($name:ident, $source:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let source = $source;
                let mut lexer = Lexer::new(source);
                let tokens = lexer.lex().unwrap();
                assert_eq!(&tokens, $expected);
            }
        };
    }

    #[test]
    fn lex_empty_source_returns_eof() {
        let source = "";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next(), None);
        assert!(lexer.eof());
    }
    #[test]
    fn lex_can_handle_unkown_tokens() {
        let source = "$@hello";
        let mut lexer = Lexer::new(source);
        assert!(lexer.lex().is_err());
    }

    test_lexer!(
        can_lex_single_char_tokens,
        "{|| && }  [==](=)!+-*/:;, /* //",
        &vec![
            Token::LBrace,
            Token::Or,
            Token::And,
            Token::RBrace,
            Token::LBracket,
            Token::EqualEqual,
            Token::RBracket,
            Token::LParen,
            Token::Equal,
            Token::RParen,
            Token::Bang,
            Token::Plus,
            Token::Minus,
            Token::Star,
            Token::Slash,
            Token::Colon,
            Token::Semicolon,
            Token::Comma,
            Token::Eof,
        ]
    );

    test_lexer!(
        can_handle_mixture_of_literals,
        r#" 42; "Apollo"; -98732"#,
        &vec![
            Token::IntegerLiteral(42),
            Token::Semicolon,
            Token::StringLiteral("Apollo".to_string()),
            Token::Semicolon,
            Token::Minus,
            Token::IntegerLiteral(98732),
            Token::Eof,
        ]
    );

    test_lexer!(
        can_handle_keywords_and_identifiers,
        "const answer : int = 42;",
        &vec![
            Token::Const,
            Token::Identifier("answer".to_string()),
            Token::Colon,
            Token::Int,
            Token::Equal,
            Token::IntegerLiteral(42),
            Token::Semicolon,
            Token::Eof,
        ]
    );

    test_lexer!(
        can_handle_keywords,
        "return 42;",
        &vec![
            Token::Return,
            Token::IntegerLiteral(42),
            Token::Semicolon,
            Token::Eof,
        ]
    );

    test_lexer!(
        lex_function,
        r#"
const answer : int = 42;

function sumArray(arr: array[int], size: int) -> int {}"#,
        &vec![
            Token::Const,
            Token::Identifier("answer".to_string()),
            Token::Colon,
            Token::Int,
            Token::Equal,
            Token::IntegerLiteral(42),
            Token::Semicolon,
            Token::Function,
            Token::Identifier("sumArray".to_string()),
            Token::LParen,
            Token::Identifier("arr".to_string()),
            Token::Colon,
            Token::Array,
            Token::LBracket,
            Token::Int,
            Token::RBracket,
            Token::Comma,
            Token::Identifier("size".to_string()),
            Token::Colon,
            Token::Int,
            Token::RParen,
            Token::Arrow,
            Token::Int,
            Token::LBrace,
            Token::RBrace,
            Token::Eof,
        ]
    );

    test_lexer!(
        can_handle_conditionals,
        r#"
        if ( a > 42) {
            return true;
        } else {
            return false;
        }
        "#,
        &vec![
            Token::If,
            Token::LParen,
            Token::Identifier("a".to_string()),
            Token::Greater,
            Token::IntegerLiteral(42),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Eof,
        ]
    );

    test_lexer!(
        can_handle_while_conditionals,
        r#"
        let i : int = 10;
        let sum : int = 0;
        while (i >= 0) {
            sum = sum + 10;
            if (i == 5) {
                break;
            }
        }"#,
        &vec![
            Token::Let,
            Token::Identifier("i".to_string()),
            Token::Colon,
            Token::Int,
            Token::Equal,
            Token::IntegerLiteral(10),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("sum".to_string()),
            Token::Colon,
            Token::Int,
            Token::Equal,
            Token::IntegerLiteral(0),
            Token::Semicolon,
            Token::While,
            Token::LParen,
            Token::Identifier("i".to_string()),
            Token::GreaterEqual,
            Token::IntegerLiteral(0),
            Token::RParen,
            Token::LBrace,
            Token::Identifier("sum".to_string()),
            Token::Equal,
            Token::Identifier("sum".to_string()),
            Token::Plus,
            Token::IntegerLiteral(10),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Identifier("i".to_string()),
            Token::EqualEqual,
            Token::IntegerLiteral(5),
            Token::RParen,
            Token::LBrace,
            Token::Break,
            Token::Semicolon,
            Token::RBrace,
            Token::RBrace,
            Token::Eof,
        ]
    );

    test_lexer!(
        can_handle_for_loops,
        r#"
        let sum : int = 0;
        for (i = 0;i < 10; i = i+1) {
        }
        "#,
        &vec![
            Token::Let,
            Token::Identifier("sum".to_string()),
            Token::Colon,
            Token::Int,
            Token::Equal,
            Token::IntegerLiteral(0),
            Token::Semicolon,
            Token::For,
            Token::LParen,
            Token::Identifier("i".to_string()),
            Token::Equal,
            Token::IntegerLiteral(0),
            Token::Semicolon,
            Token::Identifier("i".to_string()),
            Token::Lesser,
            Token::IntegerLiteral(10),
            Token::Semicolon,
            Token::Identifier("i".to_string()),
            Token::Equal,
            Token::Identifier("i".to_string()),
            Token::Plus,
            Token::IntegerLiteral(1),
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Eof,
        ]
    );

    test_lexer!(
        can_handle_array_literals,
        "let arr : array[int] = [1,2,3,4,5];",
        &vec![
            Token::Let,
            Token::Identifier("arr".to_string()),
            Token::Colon,
            Token::Array,
            Token::LBracket,
            Token::Int,
            Token::RBracket,
            Token::Equal,
            Token::LBracket,
            Token::IntegerLiteral(1),
            Token::Comma,
            Token::IntegerLiteral(2),
            Token::Comma,
            Token::IntegerLiteral(3),
            Token::Comma,
            Token::IntegerLiteral(4),
            Token::Comma,
            Token::IntegerLiteral(5),
            Token::RBracket,
            Token::Semicolon,
            Token::Eof,
        ]
    );
}
