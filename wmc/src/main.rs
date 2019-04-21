#[macro_use]
extern crate lazy_static;

use std::{env, fs};

mod lexer;

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
    lexer::lex(&src, &args[1]);
}
