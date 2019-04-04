use std::collections::HashMap;
use std::env;
use std::fs;
use std::iter;
use std::str;
use std::vec;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassManager;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum};
use inkwell::FloatPredicate;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let tokens = KTokenizer::from_filename(filename);
    let parse_tree = parse(tokens).unwrap();

    let llvm = Context::create();
    let builder = llvm.create_builder();
    let module = llvm.create_module("kaleidoscope");
    let variables = HashMap::new();

    /* JIT Optimization */

    let jit_module = llvm.create_module("kaleidoscope-jit");
    let fpm = PassManager::create_for_function(&module);

    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.initialize();

    let mut code_gen_context = CodeGenContext {
        llvm,
        builder,
        module,
        variables,
        fpm,
    };

    for top_value in parse_tree.iter() {
        let generated_ir = top_value.code_gen(&mut code_gen_context);

        if generated_ir.is_function_value() {
            println!(
                "{}",
                generated_ir
                    .as_function_value()
                    .print_to_string()
                    .to_string()
            );
        } else if generated_ir.is_float_value() {
            println!(
                "{}",
                generated_ir.as_float_value().print_to_string().to_string()
            );
        } else {
            println!(
                "Didn't get function value or float value: {:#?}",
                generated_ir
            );
        }
    }
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
        if self.peeked.is_some() {
            let stolen = self.peeked.take();
            return stolen.unwrap();
        }

        let next = self.next_impl();
        return next;
    }

    fn peek(&mut self) -> Option<&KToken> {
        if self.peeked.is_none() {
            self.peeked = Some(self.next_impl());
        }

        match self.peeked {
            Some(Some(ref value)) => Some(value),
            Some(None) => None,
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

#[derive(Debug, Eq, PartialEq)]
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
    let _def = tokens.next();
    let prototype = parse_prototype(tokens)?;
    let expr = parse_expression(tokens)?;
    Ok(Top::Definition(prototype, expr))
}

fn parse_extern_definition(tokens: &mut KTokenizer) -> Result<Top, String> {
    let _extern = tokens.next();
    let prototype = parse_prototype(tokens)?;
    Ok(Top::ExternDefinition(prototype))
}

fn parse_prototype(tokens: &mut KTokenizer) -> Result<Prototype, String> {
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
    let expr = parse_primary_expression(tokens)?;
    parse_bin_op_rhs(tokens, 0, expr)
}

fn parse_primary_expression(tokens: &mut KTokenizer) -> Result<Expr, String> {
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

/************
 * CODE GEN *
 ************/

struct CodeGenContext {
    llvm: Context,
    builder: Builder,
    module: Module,
    variables: HashMap<String, BasicValueEnum>,
    fpm: PassManager,
}

trait CodeGen {
    fn code_gen(&self, cgc: &CodeGenContext) -> BasicValueEnum;
}

impl CodeGen for Expr {
    fn code_gen(&self, cgc: &CodeGenContext) -> BasicValueEnum {
        match self {
            Expr::Literal(n) => cgc.llvm.f64_type().const_float(*n).as_basic_value_enum(),
            Expr::Variable(var_name) => match cgc.variables.get(var_name) {
                Some(val) => *val,
                None => panic!("Couldn't find variable!"),
            },
            Expr::BinaryOp { op, left, right } => {
                let lhs = left.code_gen(cgc).into_float_value();
                let rhs = right.code_gen(cgc).into_float_value();

                // Do LessThan separately otherwise branches of match have different return types.
                if *op == BinaryOperator::LessThan {
                    let bool_bit =
                        cgc.builder
                            .build_float_compare(FloatPredicate::OLT, lhs, rhs, "cmpemp");
                    return cgc
                        .builder
                        .build_unsigned_int_to_float(bool_bit, cgc.llvm.f64_type(), "booltmp")
                        .as_basic_value_enum();
                }

                let result = match op {
                    BinaryOperator::Plus => cgc.builder.build_float_add(lhs, rhs, "addtemp"),
                    BinaryOperator::Minus => cgc.builder.build_float_sub(lhs, rhs, "subtemp"),
                    BinaryOperator::Times => cgc.builder.build_float_mul(lhs, rhs, "multemp"),
                    _ => unreachable!(),
                };

                result.as_basic_value_enum()
            }
            Expr::FnCall { function, args } => match cgc.module.get_function(function) {
                Some(llvm_fn) => {
                    let fn_args: Vec<BasicValueEnum> =
                        args.iter().map(|arg| arg.code_gen(cgc)).collect();
                    // Not totally sure this is kosher.
                    return cgc
                        .builder
                        .build_call(llvm_fn, &fn_args, "calltmp")
                        .try_as_basic_value()
                        .left()
                        .unwrap();
                }
                None => panic!("Couldn't find function!"),
            },
        }
    }
}

impl Prototype {
    fn code_gen(&self, cgc: &CodeGenContext) -> AnyValueEnum {
        let num_args = self.args.len();
        let f64_type = cgc.llvm.f64_type();
        let arg_types: Vec<BasicTypeEnum> = std::iter::repeat(f64_type.as_basic_type_enum())
            .take(num_args)
            .collect();
        let fn_type = f64_type.fn_type(&arg_types, false);
        let function_value = cgc
            .module
            .add_function(&self.name, fn_type, Some(Linkage::External));

        // Define argument names
        for i in 0..num_args {
            function_value
                .get_nth_param(i as u32)
                .unwrap()
                .as_float_value()
                .set_name(&self.args[i]);
        }

        // If I use .as_any_value_enum it mysteriously turns into a PointerValue.
        AnyValueEnum::FunctionValue(function_value)
    }
}

impl Top {
    fn code_gen(&self, cgc: &mut CodeGenContext) -> AnyValueEnum {
        match self {
            Top::ExternDefinition(proto) => proto.code_gen(cgc),
            Top::Definition(proto, expr) => {
                if cgc.module.get_function(&proto.name).is_some() {
                    panic!("Cannot redeclare function. {}", proto.name);
                }

                let function_value = proto.code_gen(cgc).into_function_value();
                let bb = cgc.llvm.append_basic_block(&function_value, "entry");

                cgc.builder.position_at_end(&bb);

                // Initialize variables
                cgc.variables.clear();
                let len = proto.args.len();
                for i in 0..len {
                    cgc.variables.insert(
                        proto.args[i].clone(),
                        function_value.get_nth_param(i as u32).unwrap(),
                    );
                }

                let return_value = expr.code_gen(cgc).into_float_value();
                cgc.builder.build_return(Some(&return_value));

                let print = true;
                function_value.verify(print);

                cgc.fpm.run_on_function(&function_value);

                AnyValueEnum::FunctionValue(function_value)
            }
            Top::Expr(expr) => expr.code_gen(cgc).as_any_value_enum(),
        }
    }
}
