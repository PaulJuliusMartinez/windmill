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

            // Allow trailing commas; e.g., fn foo<A, B,>
            if self.is_next_token_eq(Token::GreaterThan) {
                // Eat The
                self.increment();
                return generics;
            }
        }

        eprintln!("Unexpected non-identifier while parsing fn generics.");

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

            // Allow trailing commas; e.g., fn foo(a: A, b: B,)
            if self.is_next_token_eq(Token::RightParen) {
                return function_args;
            }
        }

        function_args
    }

    fn parse_type_literal(&mut self) -> TypeLiteral<'a> {
        let mut type_literal = match self.peek_token() {
            Some(Token::Identifier(identifier)) => {
                // Eat identifier
                self.increment();

                let type_ident = identifier.as_str();
                if self.is_next_token_eq(Token::LessThan) {
                    let parameterized_types = self.parse_parameterized_types();
                    TypeLiteral::ParameterizedType {
                        main_type: type_ident,
                        parameterized_types,
                    }
                } else {
                    TypeLiteral::PlainType(type_ident)
                }
            }
            Some(Token::LeftParen) => self.parse_paren_type(),
            t => {
                eprintln!("Expected identifier or '(' to start parsing type literal, but got {:?} instead", t);
                TypeLiteral::PlainType("ERROR")
            }
        };

        // Hmmm, array types are tricky. We'll just eagerly consume [] as long as we can.
        let mut is_array_type = false;
        while self.is_next_token_eq(Token::LeftSquare) {
            self.increment();
            self.consume_syntax(Token::RightSquare);
            type_literal = TypeLiteral::ArrayType(Box::new(type_literal));
            is_array_type = true;
        }

        type_literal
    }

    fn parse_paren_type(&mut self) -> TypeLiteral<'a> {
        // Consume '('
        self.consume_syntax(Token::LeftParen);

        // Turn "()" into a tuple type with no elements.
        if self.is_next_token_eq(Token::RightParen) {
            self.increment();
            return TypeLiteral::TupleType(Vec::new());
        }

        let first_type = self.parse_type_literal();

        // This is just a parenthesized type; return it directly.
        if self.is_next_token_eq(Token::RightParen) {
            self.increment();
            return first_type;
        }

        // We must get a comma next.
        self.consume_syntax(Token::Comma);

        let mut tuple_types = vec![first_type];

        // Parse the rest of the elements of the tuple.
        loop {
            if self.is_next_token_eq(Token::RightParen) {
                self.increment();
                break;
            }
            tuple_types.push(self.parse_type_literal());

            match self.peek_token() {
                Some(Token::Comma) => {
                    self.increment();
                }
                Some(Token::RightParen) => {
                    self.increment();
                    break;
                }
                t => {
                    eprintln!(
                        "Expected ',' or ')' while parsing parethesized type; got {:?}",
                        t
                    );
                    break;
                }
            }
        }

        // TODO (paul): Check for function return type here and maybe return TypeLiteral::FnType.
        TypeLiteral::TupleType(tuple_types)
    }

    fn parse_parameterized_types(&mut self) -> Vec<TypeLiteral<'a>> {
        self.consume_syntax(Token::LessThan);
        let mut parameter_types = Vec::new();

        loop {
            parameter_types.push(self.parse_type_literal());

            match self.peek_token() {
                Some(Token::GreaterThan) => {
                    self.increment();
                    break;
                }
                Some(Token::Comma) => {
                    self.increment();
                    // Handle trailing comma.
                    if self.is_next_token_eq(Token::GreaterThan) {
                        self.increment();
                        break;
                    }
                }
                t => {
                    eprintln!(
                        "Expected ',' or '>' while parsing parethesized type; got {:?}",
                        t
                    );
                    break;
                }
            }
        }

        parameter_types
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

    /*
     * These are all little helpers for peeking/consuming certain types of tokens.
     */

    fn consume_identifier(&mut self) -> Option<&'a str> {
        let ident = self.peek_identifier();
        if ident.is_some() {
            self.increment();
        }
        ident
    }

    fn peek_identifier(&self) -> Option<&'a str> {
        let ident = match self.peek_token() {
            Some(Token::Identifier(identifier)) => Some(identifier.as_str()),
            _ => None,
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

    fn is_next_token_eq(&self, token: Token) -> bool {
        match self.peek_token() {
            Some(t) if *t == token => true,
            _ => false,
        }
    }
}
