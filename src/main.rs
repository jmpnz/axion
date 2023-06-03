use axion::add;
use axion::lexer::Lexer;

fn main() {
    let source = "a:integer = 42;";
    let mut lexer = Lexer::new(source);
    let tokens = lexer.lex().unwrap();
    println!("{:?}", tokens);
    println!("{}", add(55, 55));
}
