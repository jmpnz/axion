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
        Self { tokens, pos: 0 }
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
                initializer = Some(expr);
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
            return ast::Stmt::Return(expr);
        }
        if self.next(&Token::Break) {
            self.eat(&Token::Semicolon);
            return ast::Stmt::Break;
        }
        if self.next(&Token::LBrace) {
            return self.block();
        }
        if self.next(&Token::If) {
            return self.if_stmt();
        }
        if self.next(&Token::While) {
            return self.while_stmt();
        }
        if self.next(&Token::For) {
            return self.for_stmt();
        }
        self.expression_statement()
    }

    /// Parse a block statement.
    fn block(&mut self) -> ast::Stmt {
        let mut stmts: Vec<ast::Stmt> = Vec::new();

        while !self.check(&Token::RBrace) && !self.eof() {
            let stmt = self.declaration();
            stmts.push(stmt);
        }
        self.eat(&Token::RBrace);
        ast::Stmt::Block(stmts)
    }

    /// Parse an if statement.
    fn if_stmt(&mut self) -> ast::Stmt {
        self.eat(&Token::LParen);
        let condition = self.expression();
        self.eat(&Token::RParen);

        let then_branch = Box::new(self.statement());
        let mut else_branch = None;
        if self.next(&Token::Else) {
            else_branch = Some(Box::new(self.statement()));
        }
        ast::Stmt::If(condition, then_branch, else_branch)
    }

    /// Parse a while statement.
    fn while_stmt(&mut self) -> ast::Stmt {
        self.eat(&Token::LParen);
        let condition = self.expression();
        self.eat(&Token::RParen);
        let body = Box::new(self.statement());
        ast::Stmt::While(condition, body)
    }

    /// Parse a for statement.
    fn for_stmt(&mut self) -> ast::Stmt {
        self.eat(&Token::LParen);
        let initializer = match self.advance() {
            Token::Semicolon => None,
            Token::Let => Some(self.var_declaration()),
            _ => Some(self.expression_statement()),
        };
        let loop_condition = if !self.check(&Token::Semicolon) {
            Some(self.expression())
        } else {
            None
        };
        self.eat(&Token::Semicolon);
        let increment = if !self.check(&Token::RParen) {
            Some(self.expression())
        } else {
            None
        };
        self.eat(&Token::RParen);
        let mut body = self.statement();
        match increment {
            Some(inc) => match body {
                ast::Stmt::Block(ref mut stmts) => {
                    stmts.push(ast::Stmt::Expr(inc))
                }
                _ => (),
            },
            None => (),
        };
        let condition = match loop_condition {
            Some(expr) => expr,
            None => ast::Expr::Literal(ast::LiteralValue::Boolean(true)),
        };
        body = ast::Stmt::While(condition, Box::new(body));

        let new_body = match initializer {
            Some(stmt) => ast::Stmt::Block(vec![stmt, body]),
            None => body,
        };
        new_body
    }
    /// Parse an expression statement.
    fn expression_statement(&mut self) -> ast::Stmt {
        let expr = self.expression();
        self.eat(&Token::Semicolon);
        ast::Stmt::Expr(expr)
    }

    /// Parse an expression.
    fn expression(&mut self) -> ast::Expr {
        self.assignment()
    }

    /// Parse an assignment expression.
    fn assignment(&mut self) -> ast::Expr {
        let mut expr = self.or();

        if self.next(&Token::Equal) {
            let equals = self.previous();
            let value = self.assignment();

            match expr {
                ast::Expr::Var(name) => {
                    return ast::Expr::Assign(name, Box::new(value))
                }
                _ => panic!("Invalid assignment target"),
            }
        }

        expr
    }

    /// Parse an or expression.
    fn or(&mut self) -> ast::Expr {
        let mut expr = self.and();

        if self.next(&Token::Or) {
            let operator = self.previous();
            let right = self.and();
            expr =
                ast::Expr::Logical(operator, Box::new(expr), Box::new(right));
        }
        expr
    }

    /// Parse an and expression.
    fn and(&mut self) -> ast::Expr {
        let mut expr = self.equality();

        if self.next(&Token::And) {
            let operator = self.previous();
            let right = self.equality();
            expr =
                ast::Expr::Logical(operator, Box::new(expr), Box::new(right));
        }
        expr
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
            _ => panic!("Unexpected token : {}", self.peek()),
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
        false
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

    // Macro to generate test cases for parsing expressions.
    macro_rules! test_expression_parser {
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

    // Macro to generate test cases for parsing statements.
    macro_rules! test_statement_parser {
        ($name:ident, $source:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let source = $source;
                let mut lexer = Lexer::new(source);
                let tokens = lexer.lex().unwrap();
                let mut parser = Parser::new(tokens);
                let ast = parser.declaration();
                assert_eq!(ast, $expected);
            }
        };
    }

    test_expression_parser!(
        equality_expr,
        "5 == 5",
        ast::Expr::Binary(
            Token::EqualEqual,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
        )
    );

    test_expression_parser!(
        inequality_expr,
        "5 != 4",
        ast::Expr::Binary(
            Token::BangEqual,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(4))),
        )
    );
    test_expression_parser!(
        comparison_gte_expr,
        "5 >= 4",
        ast::Expr::Binary(
            Token::GreaterEqual,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(4))),
        )
    );
    test_expression_parser!(
        comparison_lte_expr,
        "5 <= 4",
        ast::Expr::Binary(
            Token::LesserEqual,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(4))),
        )
    );
    test_expression_parser!(
        unary_expr,
        "!true",
        ast::Expr::Unary(
            Token::Bang,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Boolean(true))),
        )
    );
    test_expression_parser!(
        add_expr,
        "5 + 5",
        ast::Expr::Binary(
            Token::Plus,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
        )
    );
    test_expression_parser!(
        sub_expr,
        "5 - 5",
        ast::Expr::Binary(
            Token::Minus,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
        )
    );
    test_expression_parser!(
        mul_expr,
        "5 * 5",
        ast::Expr::Binary(
            Token::Star,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
        )
    );
    test_expression_parser!(
        div_expr,
        "5 / 5",
        ast::Expr::Binary(
            Token::Slash,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
        )
    );
    test_expression_parser!(
        or_expr,
        "true || (5 == 5)",
        ast::Expr::Logical(
            Token::Or,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Boolean(true))),
            Box::new(ast::Expr::Grouping(Box::new(ast::Expr::Binary(
                Token::EqualEqual,
                Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
                Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            )))),
        )
    );
    test_expression_parser!(
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
    test_expression_parser!(
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
    test_expression_parser!(
        assignment_expr,
        "a = 42;",
        ast::Expr::Assign(
            "a".to_string(),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(42))),
        )
    );

    test_statement_parser!(
        variable_declaration,
        "{ let a : int = 42;\n let b : boolean = true; }",
        ast::Stmt::Block(vec![
            ast::Stmt::Var(
                "a".to_string(),
                DeclType::Integer,
                Some(ast::Expr::Literal(ast::LiteralValue::Int(42))),
            ),
            ast::Stmt::Var(
                "b".to_string(),
                DeclType::Boolean,
                Some(ast::Expr::Literal(ast::LiteralValue::Boolean(true,))),
            )
        ])
    );

    test_statement_parser!(
        if_statement_with_else_branch,
        "if ( a == 42 ) { a = a + 1;} else { a = a -1; }",
        ast::Stmt::If(
            ast::Expr::Binary(
                Token::EqualEqual,
                Box::new(ast::Expr::Var("a".to_string())),
                Box::new(ast::Expr::Literal(ast::LiteralValue::Int(42))),
            ),
            Box::new(ast::Stmt::Block(vec![ast::Stmt::Expr(
                ast::Expr::Assign(
                    "a".to_string(),
                    Box::new(ast::Expr::Binary(
                        Token::Plus,
                        Box::new(ast::Expr::Var("a".to_string())),
                        Box::new(ast::Expr::Literal(ast::LiteralValue::Int(1))),
                    )),
                ),
            )])),
            Some(Box::new(ast::Stmt::Block(vec![ast::Stmt::Expr(
                ast::Expr::Assign(
                    "a".to_string(),
                    Box::new(ast::Expr::Binary(
                        Token::Minus,
                        Box::new(ast::Expr::Var("a".to_string())),
                        Box::new(ast::Expr::Literal(ast::LiteralValue::Int(1))),
                    )),
                ),
            )]))),
        )
    );

    test_statement_parser!(
        while_statement,
        "while (true) { i = i + 1; }",
        ast::Stmt::While(
            ast::Expr::Literal(ast::LiteralValue::Boolean(true)),
            Box::new(ast::Stmt::Block(vec![ast::Stmt::Expr(
                ast::Expr::Assign(
                    "i".to_string(),
                    Box::new(ast::Expr::Binary(
                        Token::Plus,
                        Box::new(ast::Expr::Var("i".to_string())),
                        Box::new(ast::Expr::Literal(ast::LiteralValue::Int(1))),
                    )),
                ),
            ),])),
        )
    );

    test_statement_parser!(
        for_statement,
        "for (let i : int = 0;i < 10; i = i + 1) { break; }",
        ast::Stmt::Block(vec![
            ast::Stmt::Var(
                "i".to_string(),
                DeclType::Integer,
                Some(ast::Expr::Literal(ast::LiteralValue::Int(0))),
            ),
            ast::Stmt::While(
                ast::Expr::Binary(
                    Token::Lesser,
                    Box::new(ast::Expr::Var("i".to_string())),
                    Box::new(ast::Expr::Literal(ast::LiteralValue::Int(10))),
                ),
                Box::new(ast::Stmt::Block(vec![
                    ast::Stmt::Break,
                    ast::Stmt::Expr(ast::Expr::Assign(
                        "i".to_string(),
                        Box::new(ast::Expr::Binary(
                            Token::Plus,
                            Box::new(ast::Expr::Var("i".to_string())),
                            Box::new(ast::Expr::Literal(
                                ast::LiteralValue::Int(1)
                            )),
                        )),
                    ),),
                ]),)
            )
        ])
    );
}
