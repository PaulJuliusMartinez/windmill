use std::env;
use std::fs;
use std::iter;
use std::str;
use std::vec;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    println!("{}", filename);
    let mut tokens = KaleidoscopeTokenizer::from_filename(filename);
    while let Some(token) = tokens.next() {
        println!("{:?}", token);
    }
}

/*********
 * LEXER *
 *********/

struct KaleidoscopeTokenizer {
    chars: iter::Peekable<vec::IntoIter<char>>,
}

impl KaleidoscopeTokenizer {
    fn from_filename(filename: &str) -> KaleidoscopeTokenizer {
        let program_str = fs::read_to_string(filename).expect("Can't find file.");
        KaleidoscopeTokenizer {
            chars: program_str
                .chars()
                .collect::<Vec<char>>()
                .into_iter()
                .peekable(),
        }
    }
}

impl Iterator for KaleidoscopeTokenizer {
    type Item = KToken;

    fn next(&mut self) -> Option<KToken> {
        let mut first_non_whitespace_ch: char;

        loop {
            let next_ch = self.chars.next();
            if next_ch.is_none() {
                return None;
            }
            first_non_whitespace_ch = next_ch.unwrap();
            if !(first_non_whitespace_ch.is_whitespace()) {
                break;
            }
        }

        if first_non_whitespace_ch.is_alphabetic() {
            let mut identifier = String::new();
            identifier.push(first_non_whitespace_ch);

            while let Some(&ch) = self.chars.peek() {
                if !ch.is_alphanumeric() {
                    break;
                }
                identifier.push(ch);
                // Actually consume next character.
                self.chars.next();
            }

            match identifier.as_ref() {
                "def" => return Some(KToken::Def),
                "extern" => return Some(KToken::Extern),
                _ => return Some(KToken::Identifier(identifier)),
            }
        } else if is_ascii_digit_or_period(first_non_whitespace_ch) {
            let mut num = String::new();
            let mut seen_period = first_non_whitespace_ch == '.';
            num.push(first_non_whitespace_ch);

            while let Some(&ch) = self.chars.peek() {
                if !is_ascii_digit_or_period(ch) || (seen_period && ch == '.') {
                    break;
                }

                seen_period = ch == '.';
                num.push(ch);

                // Make sure to actually consume next character.
                self.chars.next();
            }

            match num.parse::<f64>() {
                Ok(n) => return Some(KToken::Number(n)),
                _ => return Some(KToken::Err),
            }
        } else if first_non_whitespace_ch == '#' {
            // Comment; read until end of line of eof.
            while let Some(ch) = self.chars.next() {
                if ch == '\n' {
                    return self.next();
                }
            }

            return None;
        } else {
            return Some(KToken::Unknown(first_non_whitespace_ch));
        }
    }
}

fn is_ascii_digit_or_period(ch: char) -> bool {
    ch.is_ascii_digit() || ch == '.'
}

#[derive(Debug)]
enum KToken {
    Def,
    Extern,
    Identifier(String),
    Number(f64),
    Unknown(char),
    Err,
}
