
mod tokens;
mod lex;

use std::io::{stdin, Read};
use tokens::*;
use lex::*;

fn main() {
    let mut s = String::new();
    stdin().read_to_string(&mut s).unwrap();

    let mut lexer = lex::Lexer::new(s.as_str().chars());
    while let Some(tok) = lexer.next() {
        println!("{}", tok);
    }

    if let Some(err) = lexer.get_error() {
        println!("lex error! {}", err);
    }
}
