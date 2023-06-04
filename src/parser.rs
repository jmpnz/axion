//! Recursive Descent parser for axion.
use crate::ast;
use crate::token::Token;
use crate::types::DeclType;

/// `Parser` implements a recursive descent parser.
pub struct Parser {
    // Tokens to process.
    tokens: Vec<Token>,
    // Cursor position in the token stream.
    pos: usize,
}

impl Parser {
    /// Creates a new `Parser` instance ownership of the token stream
    /// produced by the `Lexer` is passed to the `Parser`.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens,
            pos: 0,
        }
    }

    /// Parse a declaration.
    fn declaration(&mut self) -> ast::Stmt {
        if self.next(&Token::Let) {
            return self.var_declaration();
        }
        self.statement()
    }

    /// Parse a variable declaration.
    fn var_declaration(&mut self) -> ast::Stmt {
        let ident = self.advance();
        if let Token::Identifier(name) = ident {
            let mut initializer = None;
            self.eat(&Token::Colon);
            let t = match self.advance() {
                Token::Int => DeclType::Integer,
                Token::String => DeclType::String,
                Token::Char => DeclType::Char,
                Token::Boolean => DeclType::Boolean,
                _ => panic!("Unknown declaration type"),
            };
            if self.next(&Token::Equal) {
                let expr = self.expression();
                initializer = Some(Box::new(expr));
            }
            self.eat(&Token::Semicolon);
            return ast::Stmt::Var(name, t, initializer);
        }
        panic!("Expected identifier got")
    }

    /// Parse a statement.
    fn statement(&mut self) -> ast::Stmt {
        if self.next(&Token::Return) {
            let expr = self.expression();
            self.eat(&Token::Semicolon);
            return ast::Stmt::Return(Box::new(expr));
        }
        self.expression_statement()
    }

    /// Parse an expression statement.
    fn expression_statement(&mut self) -> ast::Stmt {
        let expr = self.expression();
        self.eat(&Token::Semicolon);
        ast::Stmt::Expr(Box::new(expr))
    }

    /// Parse an expression.
    fn expression(&mut self) -> ast::Expr {
        self.equality()
    }

    /// Parse an equality expression.
    fn equality(&mut self) -> ast::Expr {
        let mut expr = self.comparison();

        while self.next(&Token::BangEqual) || self.next(&Token::EqualEqual) {
            let operator = self.previous();
            let right = self.comparison();
            expr = ast::Expr::Binary(operator, Box::new(expr), Box::new(right));
        }
        expr
    }

    /// Parse a comparison expression.
    fn comparison(&mut self) -> ast::Expr {
        let mut expr = self.term();

        while self.next(&Token::Greater)
            || self.next(&Token::GreaterEqual)
            || self.next(&Token::Lesser)
            || self.next(&Token::LesserEqual)
        {
            let operator = self.previous();
            let right = self.term();
            expr = ast::Expr::Binary(operator, Box::new(expr), Box::new(right));
        }
        expr
    }

    /// Parse a term expression.
    fn term(&mut self) -> ast::Expr {
        let mut expr = self.factor();

        while self.next(&Token::Minus) || self.next(&Token::Plus) {
            let operator = self.previous();
            let right = self.factor();
            expr = ast::Expr::Binary(operator, Box::new(expr), Box::new(right));
        }
        expr
    }

    /// Parse a factor expression.
    fn factor(&mut self) -> ast::Expr {
        let mut expr = self.unary();

        while self.next(&Token::Slash) || self.next(&Token::Star) {
            let operator = self.previous();
            let right = self.unary();
            expr = ast::Expr::Binary(operator, Box::new(expr), Box::new(right));
        }
        expr
    }

    /// Parse a unary expression.
    fn unary(&mut self) -> ast::Expr {
        if self.next(&Token::Bang) || self.next(&Token::Minus) {
            let operator = self.previous();
            let right = self.unary();
            let expr = ast::Expr::Unary(operator, Box::new(right));
            return expr;
        }
        self.primary()
    }

    /// Parse a primary expression.
    fn primary(&mut self) -> ast::Expr {
        let expr = match self.peek() {
            Token::False => {
                ast::Expr::Literal(ast::LiteralValue::Boolean(false))
            }
            Token::True => ast::Expr::Literal(ast::LiteralValue::Boolean(true)),
            Token::IntegerLiteral(v) => {
                ast::Expr::Literal(ast::LiteralValue::Int(v))
            }
            Token::StringLiteral(v) => {
                ast::Expr::Literal(ast::LiteralValue::Str(v))
            }
            Token::CharacterLiteral(v) => {
                ast::Expr::Literal(ast::LiteralValue::Char(v))
            }
            Token::Identifier(ident) => ast::Expr::Var(ident),
            Token::LParen => {
                self.advance();
                let grouping = self.expression();
                ast::Expr::Grouping(Box::new(grouping))
            }
            _ => panic!("Unexpected token"),
        };
        self.advance();
        expr
    }

    /// Eat the next token if it matches `expected`.
    fn eat(&mut self, expected: &Token) -> Token {
        if self.check(expected) {
            return self.advance();
        }
        panic!("Unexpected token : {}", expected)
    }

    /// Next matches the current token against `expected` advancing the cursor
    /// if there is a match and returning `true`, otherwise returns `false`.
    fn next(&mut self, expected: &Token) -> bool {
        if self.check(expected) {
            self.advance();
            return true;
        }
        return false;
    }

    /// Advance the parser to the next token.
    fn advance(&mut self) -> Token {
        if !self.eof() {
            self.pos += 1;
        }
        self.previous()
    }

    /// Check if the current token is the `expected` token.
    fn check(&self, expected: &Token) -> bool {
        if self.eof() {
            return false;
        }
        &self.peek() == expected
    }

    /// Check if we reached end of file.
    fn eof(&self) -> bool {
        self.peek() == Token::Eof
    }

    /// Peek and return a copy of the current token.
    /// Most copies are largely inexpensive the only
    /// copies that might raise a concern
    fn peek(&self) -> Token {
        self.tokens[self.pos].clone()
    }

    /// Return a copy of the last consumed token.
    fn previous(&self) -> Token {
        self.tokens[self.pos - 1].clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::types::DeclType;

    // Macro to generate test cases for parsing.
    macro_rules! test_parser {
        ($name:ident, $source:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let source = $source;
                let mut lexer = Lexer::new(source);
                let tokens = lexer.lex().unwrap();
                let mut parser = Parser::new(tokens);
                let ast = parser.expression();
                assert_eq!(ast, $expected);
            }
        };
    }

    test_parser!(
        equality_expr,
        "5 == 5",
        ast::Expr::Binary(
            Token::EqualEqual,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
        )
    );

    test_parser!(
        inequality_expr,
        "5 != 4",
        ast::Expr::Binary(
            Token::BangEqual,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(4))),
        )
    );
    test_parser!(
        comparison_gte_expr,
        "5 >= 4",
        ast::Expr::Binary(
            Token::GreaterEqual,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(4))),
        )
    );
    test_parser!(
        comparison_lte_expr,
        "5 <= 4",
        ast::Expr::Binary(
            Token::LesserEqual,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(4))),
        )
    );
    test_parser!(
        unary_expr,
        "!true",
        ast::Expr::Unary(
            Token::Bang,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Boolean(true))),
        )
    );
    test_parser!(
        add_expr,
        "5 + 5",
        ast::Expr::Binary(
            Token::Plus,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
        )
    );
    test_parser!(
        sub_expr,
        "5 - 5",
        ast::Expr::Binary(
            Token::Minus,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
        )
    );
    test_parser!(
        mul_expr,
        "5 * 5",
        ast::Expr::Binary(
            Token::Star,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
        )
    );
    test_parser!(
        div_expr,
        "5 / 5",
        ast::Expr::Binary(
            Token::Slash,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
        )
    );
    test_parser!(
        grouping_expr,
        "(5 / 5) * (3/4 + 1)",
        ast::Expr::Binary(
            Token::Star,
            Box::new(ast::Expr::Grouping(Box::new(ast::Expr::Binary(
                Token::Slash,
                Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
                Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            )))),
            Box::new(ast::Expr::Grouping(Box::new(ast::Expr::Binary(
                Token::Plus,
                Box::new(ast::Expr::Binary(
                    Token::Slash,
                    Box::new(ast::Expr::Literal(ast::LiteralValue::Int(3))),
                    Box::new(ast::Expr::Literal(ast::LiteralValue::Int(4))),
                )),
                Box::new(ast::Expr::Literal(ast::LiteralValue::Int(1)))
            ),)))
        )
    );
    test_parser!(
        precedence_expr,
        "5 / 5 * 3/4 + 1",
        ast::Expr::Binary(
            Token::Plus,
            Box::new(ast::Expr::Binary(
                Token::Slash,
                Box::new(ast::Expr::Binary(
                    Token::Star,
                    Box::new(ast::Expr::Binary(
                        Token::Slash,
                        Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
                        Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
                    )),
                    Box::new(ast::Expr::Literal(ast::LiteralValue::Int(3)))
                ),),
                Box::new(ast::Expr::Literal(ast::LiteralValue::Int(4))),
            )),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(1)))
        )
    );
    #[test]
    fn parse_var_decl() {
        let source = "let a : int = 42;";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.declaration();
        let expected = ast::Stmt::Var(
            "a".to_string(),
            DeclType::Integer,
            Some(Box::new(ast::Expr::Literal(ast::LiteralValue::Int(42)))),
        );
        assert_eq!(ast, expected);
        println!("Declaration : {:?}", ast);
    }
}
