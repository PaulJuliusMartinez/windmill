use std::str::Chars;

mod token;
mod validation;

use token::{NumberLiteral, Token, KEYWORDS};
use validation::{LexError, LexErrorKind, LexErrorLocation, LexResult};

#[derive(Debug)]
pub struct LexedToken {
    token: Token,
    line_no: u32,
    col_no: u32,
}

struct LexerState<'a, 'b> {
    chars: RewindableCharIterator<'a>,
    line_no: u32,
    col_no: u32,
    tokens: Vec<LexedToken>,
    errors: Vec<LexError<'b>>,
}

struct RewindableCharIterator<'a> {
    chars: Chars<'a>,
    last_ch: Option<Option<char>>,
    peeked_ch: Option<Option<char>>,
    rewound: bool,
}

impl<'a> RewindableCharIterator<'a> {
    fn new(chars: Chars<'a>) -> RewindableCharIterator<'a> {
        RewindableCharIterator {
            chars,
            last_ch: None,
            peeked_ch: None,
            rewound: false,
        }
    }

    fn next(&mut self) -> Option<char> {
        if self.rewound {
            self.rewound = false;
            self.last_ch.unwrap()
        } else {
            self.last_ch = self.peeked_ch.take().or_else(|| Some(self.chars.next()));
            self.last_ch.unwrap()
        }
    }

    fn peek(&mut self) -> Option<char> {
        if self.rewound {
            self.last_ch.unwrap()
        } else {
            match self.peeked_ch {
                Some(peeked) => peeked,
                None => {
                    self.peeked_ch = Some(self.chars.next());
                    self.peeked_ch.unwrap()
                }
            }
        }
    }

    fn rewind(&mut self) {
        if self.rewound {
            panic!("Can only rewind once.");
        }
        self.rewound = true
    }
}

pub fn lex<'a, 'b>(src: &'a str, filename: &'b str) -> (Vec<LexedToken>, Vec<LexError<'b>>) {
    let mut state = LexerState {
        chars: RewindableCharIterator::new(src.chars()),
        line_no: 1,
        col_no: 1,
        tokens: Vec::new(),
        errors: Vec::new(),
    };

    state.lex(filename);
    validation::check_for_lexer_errors(&state.tokens);
    println!("{:?}", state.errors);

    return (state.tokens, state.errors);
}

macro_rules! lex_digits_fn {
    ($fn_name:ident: $( $range:pat ),+) => {
        fn $fn_name(&mut self, value: &mut String) {
            loop {
                let next_ch = match self.chars.peek() {
                    $(
                        Some(ch @ $range) => ch,
                    )*
                    _ => break,
                };
                value.push(next_ch);
                self.consume_next_ch();
            }
        }
    }
}

impl<'a, 'b> LexerState<'a, 'b> {
    fn lex(&mut self, filename: &'b str) {
        while let Some(ch) = self.chars.next() {
            let start_line_no = self.line_no;
            let start_col_no = self.col_no;
            self.increment_pos(ch);

            let token = match ch {
                // Simple tokens:
                '#' => Ok(Token::Pound),
                // '$' => Ok(Token::Dollar),
                ',' => Ok(Token::Comma),
                '.' => Ok(Token::Dot),
                ':' => Ok(Token::Colon),
                ';' => Ok(Token::Semicolon),
                '?' => Ok(Token::Question),
                '[' => Ok(Token::LeftSquare),
                ']' => Ok(Token::RightSquare),
                '(' => Ok(Token::LeftParen),
                ')' => Ok(Token::RightParen),
                '{' => Ok(Token::LeftCurly),
                '}' => Ok(Token::RightCurly),
                // '@' => Ok(Token::At),

                // Two character combinations.
                '!' => Ok(self.maybe_lex_ch_eq_token(Token::ExclamationPoint, Token::NotEqual)),
                '%' => Ok(self.maybe_lex_ch_eq_token(Token::Percent, Token::PercentEqual)),
                '*' => Ok(self.maybe_lex_ch_eq_token(Token::Asterisk, Token::TimesEqual)),
                '+' => Ok(self.maybe_lex_ch_eq_token(Token::Plus, Token::PlusEqual)),
                '-' => Ok(self.maybe_lex_ch_eq_token(Token::Hyphen, Token::MinusEqual)),
                '^' => Ok(self.maybe_lex_ch_eq_token(Token::Caret, Token::CaretEqual)),
                '=' => Ok(self.maybe_lex_ch_eq_token(Token::Equal, Token::DoubleEqual)),

                // Two/three character combinations with eq.
                '&' => Ok(self.lex_two_or_three_ch_eq_combination(
                    '&',
                    Token::Ampersand,
                    Token::DoubleAmpersand,
                    Token::AmpersandEqual,
                    Token::DoubleAmpersandEqual,
                )),
                '|' => Ok(self.lex_two_or_three_ch_eq_combination(
                    '|',
                    Token::VerticalBar,
                    Token::DoubleVerticalBar,
                    Token::VerticalBarEqual,
                    Token::DoubleVerticalBarEqual,
                )),
                '<' => Ok(self.lex_two_or_three_ch_eq_combination(
                    '<',
                    Token::LessThan,
                    Token::DoubleLessThan,
                    Token::LessThanEqual,
                    Token::DoubleLessThanEqual,
                )),
                '>' => Ok(self.lex_two_or_three_ch_eq_combination(
                    '>',
                    Token::GreaterThan,
                    Token::DoubleGreaterThan,
                    Token::GreaterThanEqual,
                    Token::DoubleGreaterThanEqual,
                )),

                // Whitespace
                ws @ ' ' | ws @ '\t' | ws @ '\n' | ws @ '\r' => Ok(self.lex_whitespace(ws)),

                // "/", "/=", or comments
                '/' => self.lex_after_slash(),

                // Lex identifiers and keywords.
                letter @ '_' | letter @ 'a'...'z' | letter @ 'A'...'Z' => {
                    Ok(self.lex_identifier_or_keyword(letter))
                }

                number @ '0'...'9' => self.lex_number(number),

                '"' => self.lex_quoted_string('"'),
                '\'' => self.lex_quoted_string('\''),

                _ => Err(LexErrorKind::UnknownChar(ch)),
            };

            match token {
                Ok(lexed_token) => self.tokens.push(LexedToken {
                    token: lexed_token,
                    line_no: start_line_no,
                    col_no: start_col_no,
                }),
                Err(kind) => self.errors.push(LexError {
                    location: LexErrorLocation {
                        filename,
                        line_no: start_line_no,
                        col_no: start_col_no,
                    },
                    kind,
                }),
            }
        }
    }

    fn increment_pos(&mut self, ch: char) {
        if ch == '\n' {
            self.line_no += 1;
            self.col_no = 1;
        } else {
            self.col_no += 1;
        }
    }

    fn consume_next_ch(&mut self) {
        if let Some(ch) = self.chars.next() {
            self.increment_pos(ch);
        } else {
            panic!("Called consume when no more tokesn");
        }
    }

    fn maybe_lex_ch_eq_token(&mut self, one_ch_token: Token, two_ch_token: Token) -> Token {
        match self.chars.peek() {
            Some('=') => {
                // Consume =
                self.consume_next_ch();
                two_ch_token
            }
            _ => one_ch_token,
        }
    }

    fn lex_two_or_three_ch_eq_combination(
        &mut self,
        ch: char,
        single_ch_token: Token,
        double_ch_token: Token,
        ch_eq_token: Token,
        double_ch_eq_token: Token,
    ) -> Token {
        let peeked = self.chars.peek();
        if peeked.is_none() {
            return single_ch_token;
        }
        let next_ch = peeked.unwrap();

        if next_ch == ch {
            // Consume next_ch
            self.consume_next_ch();
            self.maybe_lex_ch_eq_token(double_ch_token, double_ch_eq_token)
        } else if next_ch == '=' {
            // Consume =
            self.consume_next_ch();
            ch_eq_token
        } else {
            single_ch_token
        }
    }

    fn lex_whitespace(&mut self, first_ch: char) -> Token {
        let mut whitespace = String::new();
        whitespace.push(first_ch);

        while let Some(ch) = self.chars.peek() {
            match ch {
                ' ' | '\t' | '\n' | '\r' => (),
                _ => break,
            };

            // Consume next_ch
            self.consume_next_ch();
            whitespace.push(ch);
        }

        Token::Whitespace(whitespace)
    }

    fn lex_after_slash(&mut self) -> LexResult {
        match self.chars.peek() {
            Some('=') => Ok(Token::DivideEqual),
            Some('/') => Ok(self.lex_line_comment()),
            Some('*') => self.lex_block_comment(),
            _ => Ok(Token::Slash),
        }
    }

    fn lex_line_comment(&mut self) -> Token {
        let mut comment = String::from("/");

        while let Some(ch) = self.chars.next() {
            self.increment_pos(ch);
            comment.push(ch);
            if ch == '\n' {
                break;
            }
        }

        Token::LineComment(comment)
    }

    fn lex_block_comment(&mut self) -> LexResult {
        let mut comment = String::from("/");
        let mut maybe_starting_block = true;
        let mut maybe_closing_block = false;
        let mut depth = 0;

        while let Some(ch) = self.chars.next() {
            comment.push(ch);
            self.increment_pos(ch);

            if ch == '*' {
                if maybe_starting_block {
                    maybe_starting_block = false;
                    depth += 1;
                } else {
                    maybe_closing_block = true;
                }
            } else if ch == '/' {
                if maybe_closing_block {
                    maybe_closing_block = false;
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                } else {
                    maybe_starting_block = true;
                }
            } else {
                maybe_starting_block = false;
                maybe_closing_block = false;
            }
        }

        if depth == 0 {
            Ok(Token::BlockComment(comment))
        } else {
            Err(LexErrorKind::UnterminatedBlockComment(comment, depth))
        }
    }

    fn lex_identifier_or_keyword(&mut self, first_ch: char) -> Token {
        let mut identifier = String::new();
        identifier.push(first_ch);

        self.lex_identifier_into_string(&mut identifier);

        if let Some(keyword_token) = KEYWORDS.get(identifier.as_str()) {
            keyword_token.clone()
        } else {
            Token::Identifier(identifier)
        }
    }

    fn lex_identifier_into_string(&mut self, ident: &mut String) {
        while let Some(ch) = self.chars.peek() {
            match ch {
                '_' | 'a'...'z' | 'A'...'Z' | '0'...'9' => {
                    // Consume ch
                    self.consume_next_ch();
                    ident.push(ch);
                }
                _ => return,
            }
        }
    }

    fn lex_number(&mut self, digit: char) -> LexResult {
        let mut style = NumberLiteral::Decimal;
        let mut value = String::new();
        let mut suffix = None;
        value.push(digit);

        let mut digit_lexer_fn: fn(&mut Self, &mut String) = LexerState::lex_digits;

        // First check for prefix
        if digit == '0' {
            match self.chars.peek() {
                Some('b') => {
                    style = NumberLiteral::Binary;
                    digit_lexer_fn = LexerState::lex_digits;
                }
                Some('o') => {
                    style = NumberLiteral::Octal;
                    digit_lexer_fn = LexerState::lex_digits;
                }
                Some('x') => {
                    style = NumberLiteral::Hexadecimal;
                    digit_lexer_fn = LexerState::lex_hexadecimal_digits;
                }
                _ => (),
            }

            if style != NumberLiteral::Decimal {
                self.consume_next_ch();
                value = String::new();
            }
        }

        digit_lexer_fn(self, &mut value);

        // Check for decimal.
        if let Some('.') = self.chars.peek() {
            self.chars.next();
            // We want to lex "4.abs" as Number Dot Identifier, not a floating point literal with
            // a suffix of "abs".
            if let Some('a'...'z') | Some('A'...'Z') | Some('_') = self.chars.peek() {
                self.chars.rewind();
                return validation::validate_number_literal(style, value, suffix);
            }
            self.increment_pos('.');
            value.push('.');

            digit_lexer_fn(self, &mut value);
        }

        // Check for exponent.
        if let Some('e') | Some('E') = self.chars.peek() {
            self.lex_exponent(&mut value);
        }

        // Check for suffix.
        let mut suffix_str = String::new();
        self.lex_identifier_into_string(&mut suffix_str);
        if !suffix_str.is_empty() {
            suffix = Some(suffix_str);
        }

        validation::validate_number_literal(style, value, suffix)
    }

    lex_digits_fn!(lex_hexadecimal_digits: '0'...'9', 'a'...'f', 'A'...'F', '_');
    lex_digits_fn!(lex_digits: '0'...'9', '_');

    fn lex_exponent(&mut self, value: &mut String) {
        value.push(self.chars.next().unwrap());

        if let Some(sign @ '+') | Some(sign @ '-') = self.chars.peek() {
            value.push(sign);
            self.consume_next_ch();
        }

        self.lex_digits(value);
    }

    fn lex_quoted_string(&mut self, quote_ch: char) -> LexResult {
        if quote_ch != '\'' && quote_ch != '"' {
            panic!("Must pass either ' or \" to lex_quoted_string");
        }

        let mut content = String::new();
        let mut escaping = false;
        while let Some(ch) = self.chars.next() {
            self.increment_pos(ch);

            match ch {
                '"' if quote_ch == '"' => {
                    if escaping {
                        escaping = false;
                    } else {
                        return validation::validate_double_quote_literal(content);
                    }
                }
                '\'' if quote_ch == '\'' => {
                    if escaping {
                        escaping = false;
                    } else {
                        return validation::validate_single_quote_literal(content);
                    }
                }
                '\\' => {
                    escaping = !escaping;
                }
                '\n' => {
                    return Err(LexErrorKind::UnterminatedQuote {
                        content,
                        via_eof: false,
                        is_double: quote_ch == '"',
                    });
                }
                _ => {
                    escaping = false;
                }
            }
            // Push character after we're sure it's not the end of the quote.
            content.push(ch);
        }

        Err(LexErrorKind::UnterminatedQuote {
            content,
            via_eof: true,
            is_double: quote_ch == '"',
        })
    }
}
