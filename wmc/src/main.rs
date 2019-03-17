use std::env;
use std::fs;
use std::iter;
use std::str;
use std::vec;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let tokens = KTokenizer::from_filename(filename);
    println!("{:#?}", parse(tokens));
}

/*********
 * LEXER *
 *********/

struct KTokenizer {
    chars: iter::Peekable<vec::IntoIter<char>>,
    peeked: Option<Option<KToken>>,
}

impl KTokenizer {
    fn from_filename(filename: &str) -> KTokenizer {
        let program_str = fs::read_to_string(filename).expect("Can't find file.");
        KTokenizer {
            chars: program_str
                .chars()
                .collect::<Vec<char>>()
                .into_iter()
                .peekable(),
            peeked: None,
        }
    }
}

impl KTokenizer {
    fn next(&mut self) -> Option<KToken> {
        println!("Called next");
        if self.peeked.is_some() {
            let stolen = self.peeked.take();
            println!("Next token (peeked value): {:?}", stolen);
            return stolen.unwrap();
        }

        let next = self.next_impl();
        println!("Next token: {:?}", next);
        return next;
    }

    fn peek(&mut self) -> Option<&KToken> {
        println!("Called peek");
        if self.peeked.is_none() {
            self.peeked = Some(self.next_impl());
        }

        match self.peeked {
            Some(Some(ref value)) => {
                println!("Peeked: {:?}", value);
                Some(value)
            }
            Some(None) => {
                println!("Peeked: None");
                None
            }
            _ => unreachable!(),
        }
    }

    fn next_impl(&mut self) -> Option<KToken> {
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

/**********
 * PARSER *
 **********/

/*
 * top :: definition | external | expression | ';'
 *
 * definition :: 'def' prototype expression
 *
 * external :: 'extern' prototype
 *
 * prototype :: id '(' id* ')'
 *
 * expression :: primary bin_op_rhs
 *
 * primary :: id_expr | num_expr | paren_expr
 *
 * id_expr :: id | id '(' expression* ')'
 * num_expr ::  Number
 * paren_expr :: '(' expression ')'
 *
 * bin_op_rhs :: [ bin_op primary ]*
 *
 * bin_op :: '+' | '-' | '*' | '<'
 */

#[derive(Debug)]
enum BinaryOperator {
    Plus,
    Minus,
    Times,
    LessThan,
}

#[derive(Debug)]
enum Expr {
    Literal(f64),
    Variable(String),
    BinaryOp {
        op: BinaryOperator,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    FnCall {
        function: String,
        args: Vec<Expr>,
    },
}

#[derive(Debug)]
struct Prototype {
    name: String,
    args: Vec<String>,
}

#[derive(Debug)]
enum Top {
    Definition(Prototype, Expr),
    ExternDefinition(Prototype),
    Expr(Expr),
}

fn parse(mut tokens: KTokenizer) -> Result<Vec<Top>, String> {
    let mut prog: Vec<Top> = vec![];

    while let Some(token) = tokens.peek() {
        let top = match token {
            KToken::Def => parse_definition(&mut tokens),
            KToken::Extern => parse_extern_definition(&mut tokens),
            _ => Ok(Top::Expr(parse_expression(&mut tokens)?)),
        };

        prog.push(top?);
    }

    Ok(prog)
}

fn parse_definition(tokens: &mut KTokenizer) -> Result<Top, String> {
    println!("parsing definition");
    let _def = tokens.next();
    let prototype = parse_prototype(tokens)?;
    let expr = parse_expression(tokens)?;
    Ok(Top::Definition(prototype, expr))
}

fn parse_extern_definition(tokens: &mut KTokenizer) -> Result<Top, String> {
    println!("parsing extern definition");
    let _extern = tokens.next();
    let prototype = parse_prototype(tokens)?;
    Ok(Top::ExternDefinition(prototype))
}

fn parse_prototype(tokens: &mut KTokenizer) -> Result<Prototype, String> {
    println!("parsing prototype");
    let name = match tokens.next() {
        Some(KToken::Identifier(fn_name)) => fn_name,
        t => return Err(format!("Unexpected token: {:?}", t)),
    };

    expect_ch(tokens, '(')?;
    let mut args = vec![];
    loop {
        match tokens.next() {
            Some(KToken::Identifier(arg)) => {
                args.push(arg);
            }
            Some(KToken::Unknown(')')) => {
                break;
            }
            t => return Err(format!("Expected identifier or ')' but got: {:?}", t)),
        }
    }

    Ok(Prototype { name, args })
}

fn parse_expression(tokens: &mut KTokenizer) -> Result<Expr, String> {
    println!("parsing expression");
    let expr = parse_primary_expression(tokens)?;
    parse_bin_op_rhs(tokens, 0, expr)
}

fn parse_primary_expression(tokens: &mut KTokenizer) -> Result<Expr, String> {
    println!("parsing primary expression");
    match tokens.next() {
        Some(KToken::Number(n)) => {
            return Ok(Expr::Literal(n));
        }
        Some(KToken::Unknown('(')) => {
            let expr = parse_expression(tokens)?;
            expect_ch(tokens, ')')?;
            return Ok(expr);
        }
        Some(KToken::Identifier(id)) => {
            if let Some(KToken::Unknown('(')) = tokens.peek() {
                let _left_parens = tokens.next();
                let mut args = vec![];

                loop {
                    if let Some(KToken::Unknown(')')) = tokens.peek() {
                        let _right_parens = tokens.next();
                        break;
                    } else {
                        let arg = parse_expression(tokens)?;
                        args.push(arg);
                    }
                }

                return Ok(Expr::FnCall { function: id, args });
            } else {
                return Ok(Expr::Variable(id));
            }
        }
        t => {
            return Err(format!(
                "Unexpected token while parsing expression: {:?}",
                t
            ));
        }
    }
}

fn parse_bin_op_rhs(
    tokens: &mut KTokenizer,
    prev_precedence: i32,
    mut lhs: Expr,
) -> Result<Expr, String> {
    println!("parsing binary op rhs");
    loop {
        let precedence = get_precedence(tokens.peek());
        if precedence < prev_precedence {
            return Ok(lhs);
        }

        let bin_op = token_to_bin_op(tokens.next().unwrap());
        let mut rhs = parse_primary_expression(tokens)?;

        let next_precedence = get_precedence(tokens.peek());
        if precedence < next_precedence {
            rhs = parse_bin_op_rhs(tokens, precedence + 1, rhs)?;
        }

        lhs = Expr::BinaryOp {
            op: bin_op,
            left: Box::new(lhs),
            right: Box::new(rhs),
        };
    }
}

fn expect_ch(tokens: &mut KTokenizer, expected: char) -> Result<(), String> {
    match tokens.next() {
        Some(KToken::Unknown(ch)) if ch == expected => Ok(()),
        t => Err(format!("Expected {} but got: {:?}", expected, t)),
    }
}

fn get_precedence(token: Option<&KToken>) -> i32 {
    match token {
        Some(KToken::Unknown('<')) => 10,
        Some(KToken::Unknown('+')) => 20,
        Some(KToken::Unknown('-')) => 30,
        Some(KToken::Unknown('*')) => 40,
        _ => -1,
    }
}

fn token_to_bin_op(token: KToken) -> BinaryOperator {
    match token {
        KToken::Unknown('<') => BinaryOperator::LessThan,
        KToken::Unknown('+') => BinaryOperator::Plus,
        KToken::Unknown('-') => BinaryOperator::Minus,
        KToken::Unknown('*') => BinaryOperator::Times,
        _ => panic!("Expected next token to be binary operator."),
    }
}
