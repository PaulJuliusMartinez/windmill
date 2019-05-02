mod ast;

use crate::lexer::token::Token;
use crate::lexer::LexedToken;
use ast::*;

pub fn parse<'a>(tokens: &'a [LexedToken]) -> Vec<Item<'a>> {
    let mut parser = Parser {
        tokens,
        curr_token_index: 0,
    };
    parser.parse_items()
}

struct Parser<'a> {
    tokens: &'a [LexedToken],
    curr_token_index: usize,
}

impl<'a> Parser<'a> {
    fn peek_token(&self) -> Option<&'a Token> {
        match self.tokens.get(self.curr_token_index) {
            Some(lexed_token) => Some(&lexed_token.token),
            None => None,
        }
    }

    fn consume_token(&mut self) -> Option<&'a Token> {
        let token = self.peek_token();
        self.increment();
        token
    }

    fn increment(&mut self) {
        if self.curr_token_index < self.tokens.len() {
            self.curr_token_index += 1
        }
    }

    fn parse_items(&mut self) -> Vec<Item<'a>> {
        let mut items = Vec::new();

        loop {
            match self.peek_token() {
                Some(Token::KwFn) => items.push(self.parse_function_def()),
                Some(Token::KwStruct) => items.push(self.parse_struct_def()),
                Some(Token::KwVariant) => items.push(self.parse_variant_def()),
                Some(Token::KwInterface) => items.push(self.parse_interface_def()),
                None => break,
                t => {
                    eprintln!(
                        "Expected 'fn', 'struct', 'variant' or 'interface'; got {:?}.",
                        t
                    );
                    self.increment();
                }
            }
        }

        items
    }

    fn parse_function_def(&mut self) -> Item<'a> {
        // Consume "fn"
        self.increment();
        let name = self.consume_identifier().unwrap();
        let generics = self.parse_generics();
        self.consume_syntax(Token::LeftParen);
        let arguments = self.parse_function_args();
        self.consume_syntax(Token::RightParen);
        let return_type = self.parse_function_return_type();
        self.consume_syntax(Token::LeftCurly);
        self.consume_syntax(Token::RightCurly);

        Item::FunctionDef(FunctionDef {
            name,
            generics,
            arguments,
            return_type,
        })
    }

    fn parse_generics(&mut self) -> Vec<&'a str> {
        let mut generics = Vec::new();

        match self.peek_token() {
            Some(Token::LessThan) => { /* We have generics! */ }
            _ => return generics,
        }

        self.increment();

        while let Some(generic_arg) = self.peek_identifier() {
            generics.push(generic_arg);
            self.increment();

            match self.consume_token() {
                Some(Token::Comma) => { /* Keep parsing more generics */ }
                Some(Token::GreaterThan) => {
                    return generics;
                }
                t => {
                    eprintln!("Expected ',' or '>' when passing fn generics; got {:?}", t);
                    return generics;
                }
            }
        }

        eprintln!("Unexpected EOF while parsing fn generics.");

        return generics;
    }

    fn parse_function_args(&mut self) -> Vec<(&'a str, TypeLiteral<'a>)> {
        let mut function_args = Vec::new();

        while let Some(arg_name) = self.consume_identifier() {
            self.consume_syntax(Token::Colon);
            let arg_type = self.parse_type_literal();
            function_args.push((arg_name, arg_type));

            match self.peek_token() {
                Some(Token::Comma) => {
                    self.increment();
                }
                Some(Token::RightParen) => break,
                t => {
                    eprintln!("Expected ',' or ')' when passing fn args; got {:?}", t);
                    return function_args;
                }
            }
        }

        function_args
    }

    fn parse_type_literal(&mut self) -> TypeLiteral<'a> {
        TypeLiteral::PlainType(self.consume_identifier().unwrap())
    }

    fn parse_function_return_type(&mut self) -> Option<TypeLiteral<'a>> {
        None
    }

    fn parse_struct_def(&mut self) -> Item<'a> {
        unimplemented!()
    }

    fn parse_variant_def(&mut self) -> Item<'a> {
        unimplemented!()
    }

    fn parse_interface_def(&mut self) -> Item<'a> {
        unimplemented!()
    }

    fn consume_identifier(&mut self) -> Option<&'a str> {
        let ident = self.peek_identifier();
        if ident.is_some() {
            self.increment();
        }
        ident
    }

    fn peek_identifier(&mut self) -> Option<&'a str> {
        let ident = match self.peek_token() {
            Some(Token::Identifier(identifier)) => Some(identifier.as_str()),
            _ => {
                eprintln!("Expected identifier.");
                None
            }
        };

        ident
    }

    fn consume_syntax(&mut self, token: Token) {
        match self.peek_token() {
            Some(t) if *t == token => { /* nothing */ }
            Some(t) => {
                eprintln!("Expected {:?} but got {:?}", token, t);
            }
            None => {
                eprintln!("Expected {:?} but reached EOF", token);
            }
        }
        self.increment();
    }
}
