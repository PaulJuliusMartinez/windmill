use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let program_str = fs::read_to_string(filename).expect("Can't find file.");
    println!("{}", program_str);
}
