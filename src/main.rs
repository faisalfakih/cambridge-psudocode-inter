mod Lexer;
mod Parser;
mod errortype;
use std::fs;

use crate::Parser::parser;

fn main() {
    // read from main.cps
    let contents = fs::read_to_string("tests/main.cps")
        .expect("Something went wrong reading the file");

    let lexer = Lexer::lexer::Lexer::new(contents.clone());
    match lexer.clone().tokenize() {
        Ok(tokens) => {
            for token in tokens {
                println!("{:?}", token);
            }
        }
        Err(e) => {
            eprintln!("{}", e);
        }
    }

    let mut parser = parser::Parser::new(lexer.tokenize().unwrap(), contents.to_owned()).clone();
    let res = parser.parse_top_expr();
    match res {
        Ok(ast) => {
            ast.print_prefix();
        }
        Err(e) => {
            eprintln!("{}", e);
        }
    }
}
