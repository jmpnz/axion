use axion::lexer::Lexer;
use axion::parser::Parser;

fn compile_source(source: &str) {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.lex().unwrap();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    println!("AST : {:?}", ast);
}

fn main() {
    let source = "let a:int = 42;";
    compile_source(source);
}
