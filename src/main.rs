
mod tokens;
mod lex;
mod ast;
mod ast_display;
mod syn;
mod gen;
mod args;

use std::io::{stdin, Read};
use clap::Parser;

use crate::gen::CodeGen;

fn main() {
    let args = args::Args::parse();

    // read input (stdin)
    let mut s = String::new();
    stdin().read_to_string(&mut s).unwrap();
    if s.chars().last().unwrap() != '\n' {
        s.push('\n');
    }

    match args.output_mode {
        args::OutputMode::Lex => {
            // print output from lexer

            let mut lexer = lex::Lexer::new(s.as_str().chars());
            while let Some(tok) = lexer.next() {
                println!("{}", tok);
            }
            if let Some(err) = lexer.get_error() {
                panic!("lex error! {}", err);
            }
        },
        args::OutputMode::Ast | args::OutputMode::Gen => {
            let mut lexer = lex::Lexer::new(s.as_str().chars()).peekable();
            let mut parser = syn::Parser::new(&mut lexer);

            let program_ast = match parser.parse_program() {
                Ok(ast) => ast,
                Err(err) => panic!("lex or parse error! {:?}", err),
            };

            if let args::OutputMode::Ast = args.output_mode {
                // print output from parser
                println!("{}", ast_display::indent(format!("{}", program_ast), 4, false));
            } else {
                // generate LLVM IR
                let mut context = gen::GenContext::new();

                match program_ast.gen(&mut context, None).and_then(|_| context.get_string()) {
                    Ok(llvm_ir) => println!("{}", llvm_ir),
                    Err(err) => panic!("gen error! {:?}", err),
                }
            }
        },
    }
}
