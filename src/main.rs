mod Lexer;
mod errortype;
use std::fs;

fn main() {
    // read from main.cps
    let contents = fs::read_to_string("tests/main.cps")
        .expect("Something went wrong reading the file");

    let mut lexer = Lexer::lexer::Lexer::new(contents);
    match lexer.tokenize() {
        Ok(tokens) => {
            for token in tokens {
                println!("{:?}", token);
            }
        }
        Err(e) => {
            eprintln!("{}", e);
        }
    }
}
