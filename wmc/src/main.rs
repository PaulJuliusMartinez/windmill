#[macro_use]
extern crate lazy_static;

use std::{env, fs};

mod lexer;
mod parser;

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        println!("Please provide a filename.");
        return;
    }

    let src = fs::read_to_string(&args[1]);
    if let Err(err) = src {
        println!("Could not read from file: {}.", err);
        return;
    }

    let src = src.unwrap();
    let (tokens, lex_errors) = lexer::lex(&src, &args[1]);
    if !lex_errors.is_empty() {
        for err in lex_errors.iter().rev() {
            err.eprint();
        }
        eprintln!("Compilation failed due to lexical errors.");
        return;
    }

    let non_ws_tokens: Vec<lexer::LexedToken> = tokens
        .into_iter()
        .filter(|t| t.token.is_meaningful_for_parsing())
        .collect();

    // dbg!(&non_ws_tokens);

    println!("{:#?}", parser::parse(non_ws_tokens.as_slice()));
}
