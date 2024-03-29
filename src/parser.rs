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
    #[must_use]
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse(&mut self) -> Vec<ast::Stmt> {
        let mut program = Vec::new();
        while !self.eof() {
            let decl = self.declaration();
            program.push(decl);
        }
        program
    }

    /// Parse a declaration.
    fn declaration(&mut self) -> ast::Stmt {
        if self.next(&Token::Let) {
            return self.var_declaration();
        }
        if self.next(&Token::Function) {
            return self.function_declaration();
        }
        self.statement()
    }

    /// Parse a function declaration.
    fn function_declaration(&mut self) -> ast::Stmt {
        let tok = self.advance();
        let name = Self::destruct_ident(&tok)
            .unwrap_or_else(|| panic!("Expected identifier got {tok}"));

        self.eat(&Token::LParen);
        let mut params: Vec<ast::Parameter> = Vec::new();
        while !self.check(&Token::RParen) {
            let Token::Identifier(name) = self.advance() else {
                panic!("Expected identifier")
            };
            self.eat(&Token::Colon);

            let t = match self.advance() {
                Token::Int => DeclType::Integer,
                Token::String => DeclType::String,
                Token::Char => DeclType::Char,
                Token::Boolean => DeclType::Boolean,
                _ => panic!("Unknown declaration type {}", self.previous()),
            };
            let param = ast::Parameter(name, t);
            params.push(param);
            if !self.next(&Token::Comma) {
                break;
            }
        }
        self.eat(&Token::RParen);
        self.eat(&Token::Arrow);
        let ret_type = match self.advance() {
            Token::Int => DeclType::Integer,
            Token::String => DeclType::String,
            Token::Char => DeclType::Char,
            Token::Boolean => DeclType::Boolean,
            Token::Void => DeclType::Void,
            _ => panic!("Unknown declaration type {}", self.previous()),
        };

        self.eat(&Token::LBrace);
        let block = self.block();
        if let ast::Stmt::Block(stmts) = block {
            ast::Stmt::Function(name, ret_type, params, stmts)
        } else {
            panic!("Expected block after function declarations");
        }
    }

    // Destruct an identifier returning a the `String` if there's one
    // or `None`.
    fn destruct_ident(ident: &Token) -> Option<String> {
        if let &Token::Identifier(name) = &ident {
            return Some(name.clone());
        }
        None
    }

    /// Parse a variable declaration.
    fn var_declaration(&mut self) -> ast::Stmt {
        let ident = self.advance();
        if let Token::Identifier(name) = ident {
            self.eat(&Token::Colon);
            let t = match self.advance() {
                Token::Int => DeclType::Integer,
                Token::String => DeclType::String,
                Token::Char => DeclType::Char,
                Token::Boolean => DeclType::Boolean,
                _ => panic!("Unknown declaration type"),
            };
            // Initialize to default value when none is assigned
            let initializer = if self.next(&Token::Equal) {
                Some(self.expression())
            } else {
                Some(ast::Expr::Literal(t.default_value()))
            };
            self.eat(&Token::Semicolon);
            return ast::Stmt::Var(name, t, initializer);
        }
        panic!("Expected identifier got")
    }

    /// Parse a statement.
    fn statement(&mut self) -> ast::Stmt {
        match self.peek() {
            Token::Return => {
                self.advance();
                let expr = self.expression();
                self.eat(&Token::Semicolon);
                ast::Stmt::Return(expr)
            }
            Token::Break => {
                self.advance();
                self.eat(&Token::Semicolon);
                ast::Stmt::Break
            }
            Token::LBrace => {
                self.advance();
                self.block()
            }
            Token::If => {
                self.advance();
                self.if_stmt()
            }
            Token::While => {
                self.advance();
                self.while_stmt()
            }
            Token::For => {
                self.advance();
                self.for_stmt()
            }
            _ => self.expression_statement(),
        }
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

        let else_branch = if self.next(&Token::Else) {
            Some(Box::new(self.statement()))
        } else {
            None
        };
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
        let loop_condition = if self.check(&Token::Semicolon) {
            None
        } else {
            Some(self.expression())
        };
        self.eat(&Token::Semicolon);
        let increment = if self.check(&Token::RParen) {
            None
        } else {
            Some(self.expression())
        };
        self.eat(&Token::RParen);
        let mut body = self.statement();

        if let Some(inc) = increment {
            if let ast::Stmt::Block(ref mut stmts) = body {
                stmts.push(ast::Stmt::Expr(inc));
            }
        };
        let condition = loop_condition.map_or_else(
            || ast::Expr::Literal(ast::LiteralValue::Boolean(true)),
            |expr| expr,
        );
        body = ast::Stmt::While(condition, Box::new(body));

        match initializer {
            Some(stmt) => ast::Stmt::Block(vec![stmt, body]),
            None => body,
        }
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
        let expr = self.or();

        if self.next(&Token::Equal) {
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
            let operator = match self.previous() {
                Token::And => ast::LogicalOp::And,
                Token::Or => ast::LogicalOp::Or,
                _ => panic!("Unexpected operator"),
            };
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
            let operator = match self.previous() {
                Token::And => ast::LogicalOp::And,
                Token::Or => ast::LogicalOp::Or,
                _ => panic!("Unexpected operator"),
            };
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
            let operator = match self.previous() {
                Token::BangEqual => ast::BinOp::Neq,
                Token::EqualEqual => ast::BinOp::Equ,
                _ => panic!("Unexpected operator"),
            };
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
            let operator = match self.previous() {
                Token::Greater => ast::BinOp::Gt,
                Token::GreaterEqual => ast::BinOp::Gte,
                Token::Lesser => ast::BinOp::Lt,
                Token::LesserEqual => ast::BinOp::Lte,
                _ => panic!("Unexpected operator"),
            };
            let right = self.term();
            expr = ast::Expr::Binary(operator, Box::new(expr), Box::new(right));
        }
        expr
    }

    /// Parse a term expression.
    fn term(&mut self) -> ast::Expr {
        let mut expr = self.factor();

        while self.next(&Token::Minus) || self.next(&Token::Plus) {
            let operator = match self.previous() {
                Token::Plus => ast::BinOp::Add,
                Token::Minus => ast::BinOp::Sub,
                _ => panic!("Unexpected operator"),
            };
            let right = self.factor();
            expr = ast::Expr::Binary(operator, Box::new(expr), Box::new(right));
        }
        expr
    }

    /// Parse a factor expression.
    fn factor(&mut self) -> ast::Expr {
        let mut expr = self.unary();

        while self.next(&Token::Slash) || self.next(&Token::Star) {
            let operator = match self.previous() {
                Token::Star => ast::BinOp::Mul,
                Token::Slash => ast::BinOp::Div,
                _ => panic!("Unexpected operator"),
            };
            let right = self.unary();
            expr = ast::Expr::Binary(operator, Box::new(expr), Box::new(right));
        }
        expr
    }

    /// Parse a unary expression.
    fn unary(&mut self) -> ast::Expr {
        if self.next(&Token::Bang) || self.next(&Token::Minus) {
            let operator = match self.previous() {
                Token::Bang => ast::UnaryOp::Not,
                Token::Minus => ast::UnaryOp::Neg,
                _ => panic!("Unexpected operator"),
            };
            let right = self.unary();
            let expr = ast::Expr::Unary(operator, Box::new(right));
            return expr;
        }
        self.call()
    }

    /// Parse a function call.
    fn call(&mut self) -> ast::Expr {
        let mut expr = self.primary();

        if self.next(&Token::LParen) {
            let mut args: Vec<ast::Expr> = Vec::new();
            if !self.next(&Token::RParen) {
                loop {
                    let arg = self.expression();
                    args.push(arg);
                    if !self.next(&Token::Comma) {
                        break;
                    }
                }
            }
            self.eat(&Token::RParen);
            expr = ast::Expr::Call(Box::new(expr), args);
        }
        expr
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
        panic!(
            "eat: expected token : {} but found : {}",
            expected,
            self.peek()
        )
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
            ast::BinOp::Equ,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
        )
    );

    test_expression_parser!(
        inequality_expr,
        "5 != 4",
        ast::Expr::Binary(
            ast::BinOp::Neq,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(4))),
        )
    );
    test_expression_parser!(
        comparison_gte_expr,
        "5 >= 4",
        ast::Expr::Binary(
            ast::BinOp::Gte,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(4))),
        )
    );
    test_expression_parser!(
        comparison_lte_expr,
        "5 <= 4",
        ast::Expr::Binary(
            ast::BinOp::Lte,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(4))),
        )
    );
    test_expression_parser!(
        unary_expr,
        "!true",
        ast::Expr::Unary(
            ast::UnaryOp::Not,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Boolean(true))),
        )
    );
    test_expression_parser!(
        add_expr,
        "5 + 5",
        ast::Expr::Binary(
            ast::BinOp::Add,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
        )
    );
    test_expression_parser!(
        sub_expr,
        "5 - 5",
        ast::Expr::Binary(
            ast::BinOp::Sub,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
        )
    );
    test_expression_parser!(
        mul_expr,
        "5 * 5",
        ast::Expr::Binary(
            ast::BinOp::Mul,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
        )
    );
    test_expression_parser!(
        div_expr,
        "5 / 5",
        ast::Expr::Binary(
            ast::BinOp::Div,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
        )
    );
    test_expression_parser!(
        or_expr,
        "true || (5 == 5)",
        ast::Expr::Logical(
            ast::LogicalOp::Or,
            Box::new(ast::Expr::Literal(ast::LiteralValue::Boolean(true))),
            Box::new(ast::Expr::Grouping(Box::new(ast::Expr::Binary(
                ast::BinOp::Equ,
                Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
                Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            )))),
        )
    );
    test_expression_parser!(
        grouping_expr,
        "(5 / 5) * (3/4 + 1)",
        ast::Expr::Binary(
            ast::BinOp::Mul,
            Box::new(ast::Expr::Grouping(Box::new(ast::Expr::Binary(
                ast::BinOp::Div,
                Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
                Box::new(ast::Expr::Literal(ast::LiteralValue::Int(5))),
            )))),
            Box::new(ast::Expr::Grouping(Box::new(ast::Expr::Binary(
                ast::BinOp::Add,
                Box::new(ast::Expr::Binary(
                    ast::BinOp::Div,
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
            ast::BinOp::Add,
            Box::new(ast::Expr::Binary(
                ast::BinOp::Div,
                Box::new(ast::Expr::Binary(
                    ast::BinOp::Mul,
                    Box::new(ast::Expr::Binary(
                        ast::BinOp::Div,
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

    test_expression_parser!(
        call_expr,
        "identity(3,3);",
        ast::Expr::Call(
            Box::new(ast::Expr::Var("identity".to_string())),
            vec![
                ast::Expr::Literal(ast::LiteralValue::Int(3)),
                ast::Expr::Literal(ast::LiteralValue::Int(3)),
            ],
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
                ast::BinOp::Equ,
                Box::new(ast::Expr::Var("a".to_string())),
                Box::new(ast::Expr::Literal(ast::LiteralValue::Int(42))),
            ),
            Box::new(ast::Stmt::Block(vec![ast::Stmt::Expr(
                ast::Expr::Assign(
                    "a".to_string(),
                    Box::new(ast::Expr::Binary(
                        ast::BinOp::Add,
                        Box::new(ast::Expr::Var("a".to_string())),
                        Box::new(ast::Expr::Literal(ast::LiteralValue::Int(1))),
                    )),
                ),
            )])),
            Some(Box::new(ast::Stmt::Block(vec![ast::Stmt::Expr(
                ast::Expr::Assign(
                    "a".to_string(),
                    Box::new(ast::Expr::Binary(
                        ast::BinOp::Sub,
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
                        ast::BinOp::Add,
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
                    ast::BinOp::Lt,
                    Box::new(ast::Expr::Var("i".to_string())),
                    Box::new(ast::Expr::Literal(ast::LiteralValue::Int(10))),
                ),
                Box::new(ast::Stmt::Block(vec![
                    ast::Stmt::Break,
                    ast::Stmt::Expr(ast::Expr::Assign(
                        "i".to_string(),
                        Box::new(ast::Expr::Binary(
                            ast::BinOp::Add,
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

    test_statement_parser!(
        function_declaration,
        r#"
        function sum100(i : int) -> int {
            return i;
        }
        "#,
        ast::Stmt::Function(
            "sum100".to_string(),
            DeclType::Integer,
            vec![ast::Parameter("i".to_string(), DeclType::Integer)],
            vec![ast::Stmt::Return(ast::Expr::Var("i".to_string())),],
        )
    );

    test_statement_parser!(
        function_declaration_void_return_type,
        r#"
        function sum100(i : int) -> void {
        }
        "#,
        ast::Stmt::Function(
            "sum100".to_string(),
            DeclType::Void,
            vec![ast::Parameter("i".to_string(), DeclType::Integer)],
            vec![],
        )
    );

    test_statement_parser!(
        function_declaration_no_args,
        r#"
        function sum100() -> void {
        }
        "#,
        ast::Stmt::Function(
            "sum100".to_string(),
            DeclType::Void,
            vec![],
            vec![],
        )
    );
}
