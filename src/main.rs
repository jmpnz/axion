use axion::lexer::Lexer;
use axion::parser::Parser;
use axion::sema::SemanticAnalyzer;

fn compile_source(source: &str) {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.lex().unwrap();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    let mut sema = SemanticAnalyzer::new();
    sema.run(&ast);
    println!("AST : {ast:?}");
}

fn main() {
    let source = r#"
    let i:int = 42;
    function die(a:int) -> int {
       if (a <= 42) {
            return 0;
       } else {
            a = a + 1;
        }
       return a;
    }
    function main(x:int) -> void {
        for(let i:int = 0;i < 10000;i = i + 1) {
            die(i);
        }
    }
    "#;
    compile_source(source);
}
