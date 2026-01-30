mod Lexer;
mod Parser;
mod Inter;
mod errortype;
use std::fs;

use crate::Parser::parser;

fn main() {
    // read from main.cps
    let contents = fs::read_to_string("tests/main.cps")
        .expect("Something went wrong reading the file");

    let lexer = Lexer::lexer::Lexer::new(contents.clone());
    let t = lexer.clone().tokenize();
    let tokens;
    match t {
        Ok(ref tok) => {
            tokens = tok.clone();
            // for tok in t {
            //     println!("{:?}", tok);
            // }
        }
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    }

    let mut parser = parser::Parser::new(tokens, contents.to_owned()).clone();
    let res = parser.parse_statements();
    match res.clone() {
        Ok(_) => {
            // for a in ast.iter() {
            //     a.print_prefix();
            // }
            let mut interpreter = Inter::interpreter::Interpreter::new();
            match interpreter.interpret(res.clone().unwrap()) {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("{}", e);
                }
            };
        }
        Err(e) => {
            eprintln!("{}", e);
        }
    }

}
