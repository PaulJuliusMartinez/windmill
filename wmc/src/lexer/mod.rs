use std::collections::HashMap;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone)]
pub enum NumberLiteralPrefix {
    Binary,
    Octal,
    Hexadecimal,
}

#[derive(Debug, Clone)]
pub enum Token {
    // Keywords
    KwIf,
    KwElse,
    KwWhile,
    KwBreak,
    KwContinue,
    KwFor,
    KwReturn,

    KwTrue,
    KwFalse,

    KwFn,
    KwLet,
    KwSet,
    KwSelf,
    KwSelfType,
    KwStruct,

    // More complex keywords
    Whitespace {
        ws: String,
        has_newline: bool,
    },
    LineComment(String),
    BlockComment(String),
    Identifier(String),
    DoubleQuoteLiteral(String),
    SingleQuoteLiteral(String),
    NumberLiteral {
        prefix: Option<NumberLiteralPrefix>,
        value: String,
        exponent: Option<String>,
        suffix: Option<String>,
    },
    Unknown(char),

    // There are all error conditions for strings.
    UnterminatedBlockComment(String, u32),
    // Quotes are either terminated by end of file or new lines.
    UnterminatedDoubleQuoteLiteral {
        content: String,
        via_eof: bool,
    },
    UnterminatedSingleQuoteLiteral {
        content: String,
        via_eof: bool,
    },

    // Multiple character tokens
    DoubleLessThanEqual,
    DoubleGreaterThanEqual,
    DoubleAmpersandEqual,
    DoubleVerticalBarEqual,

    DoubleAmpersand,
    DoubleVerticalBar,
    DoubleLessThan,
    DoubleGreaterThan,
    PlusEqual,
    MinusEqual,
    TimesEqual,
    DivideEqual,
    PercentEqual,
    CaretEqual,
    AmpersandEqual,
    VerticalBarEqual,
    DoubleEqual,
    NotEqual,
    LessThanEqual,
    GreaterThanEqual,

    // Single character tokens
    ExclamationPoint,
    Pound,
    // Dollar,
    Percent,
    Ampersand,
    LeftParen,
    RightParen,
    Asterisk,
    Plus,
    Comma,
    Hyphen,
    Dot,
    Slash,
    Colon,
    Semicolon,
    LessThan,
    Equal,
    GreaterThan,
    Question,
    // At,
    LeftSquare,
    RightSquare,
    Caret,
    LeftCurly,
    VerticalBar,
    RightCurly,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token> = {
        let mut m = HashMap::new();
        m.insert("if", Token::KwIf);
        m.insert("else", Token::KwElse);
        m.insert("while", Token::KwWhile);
        m.insert("break", Token::KwBreak);
        m.insert("continue", Token::KwContinue);
        m.insert("for", Token::KwFor);
        m.insert("return", Token::KwReturn);
        m.insert("true", Token::KwTrue);
        m.insert("false", Token::KwFalse);
        m.insert("fn", Token::KwFn);
        m.insert("let", Token::KwLet);
        m.insert("set", Token::KwSet);
        m.insert("self", Token::KwSelf);
        m.insert("selfType", Token::KwSelfType);
        m.insert("struct", Token::KwStruct);
        m
    };
}

#[derive(Debug)]
pub struct LexedToken {
    token: Token,
    line_no: i32,
    col_no: i32,
}

struct LexerState<'a> {
    chars: RewindableCharIterator<'a>,
    line_no: i32,
    col_no: i32,
    tokens: Vec<LexedToken>,
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

pub fn lex(src: &str) -> Vec<LexedToken> {
    let mut state = LexerState {
        chars: RewindableCharIterator::new(src.chars()),
        line_no: 1,
        col_no: 1,
        tokens: Vec::new(),
    };

    println!("Lexing");
    state.lex();
    println!("Lexed");
    println!("{:#?}", state.tokens);

    return state.tokens;
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

impl<'a> LexerState<'a> {
    fn lex(&mut self) {
        while let Some(ch) = self.chars.next() {
            let start_line_no = self.line_no;
            let start_col_no = self.col_no;
            self.increment_pos(ch);

            let token = match ch {
                // Simple tokens:
                '#' => Token::Pound,
                // '$' => Token::Dollar,
                ',' => Token::Comma,
                '.' => Token::Dot,
                ':' => Token::Colon,
                ';' => Token::Semicolon,
                '?' => Token::Question,
                '[' => Token::LeftSquare,
                ']' => Token::RightSquare,
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '{' => Token::LeftCurly,
                '}' => Token::RightCurly,
                // '@' => Token::At,

                // Two character combinations.
                '!' => self.maybe_lex_ch_eq_token(Token::ExclamationPoint, Token::NotEqual),
                '%' => self.maybe_lex_ch_eq_token(Token::Percent, Token::PercentEqual),
                '*' => self.maybe_lex_ch_eq_token(Token::Asterisk, Token::TimesEqual),
                '+' => self.maybe_lex_ch_eq_token(Token::Plus, Token::PlusEqual),
                '-' => self.maybe_lex_ch_eq_token(Token::Hyphen, Token::MinusEqual),
                '^' => self.maybe_lex_ch_eq_token(Token::Caret, Token::CaretEqual),
                '=' => self.maybe_lex_ch_eq_token(Token::Equal, Token::DoubleEqual),

                // Two/three character combinations with eq.
                '&' => self.lex_two_or_three_ch_eq_combination(
                    '&',
                    Token::Ampersand,
                    Token::DoubleAmpersand,
                    Token::AmpersandEqual,
                    Token::DoubleAmpersandEqual,
                ),
                '|' => self.lex_two_or_three_ch_eq_combination(
                    '|',
                    Token::VerticalBar,
                    Token::DoubleVerticalBar,
                    Token::VerticalBarEqual,
                    Token::DoubleVerticalBarEqual,
                ),
                '<' => self.lex_two_or_three_ch_eq_combination(
                    '<',
                    Token::LessThan,
                    Token::DoubleLessThan,
                    Token::LessThanEqual,
                    Token::DoubleLessThanEqual,
                ),
                '>' => self.lex_two_or_three_ch_eq_combination(
                    '>',
                    Token::GreaterThan,
                    Token::DoubleGreaterThan,
                    Token::GreaterThanEqual,
                    Token::DoubleGreaterThanEqual,
                ),

                // Whitespace
                ws @ ' ' | ws @ '\t' | ws @ '\n' | ws @ '\r' => self.lex_whitespace(ws),

                // "/", "/=", or comments
                '/' => self.lex_after_slash(),

                // Lex identifiers and keywords.
                letter @ '_' | letter @ 'a'...'z' | letter @ 'A'...'Z' => {
                    self.lex_identifier_or_keyword(letter)
                }

                number @ '0'...'9' => self.lex_number(number),

                '"' => self.lex_quoted_string('"'),
                '\'' => self.lex_quoted_string('\''),

                _ => Token::Unknown(ch),
            };

            self.tokens.push(LexedToken {
                token,
                line_no: start_line_no,
                col_no: start_col_no,
            })
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
        let mut has_newline = first_ch == '\n';
        whitespace.push(first_ch);

        while let Some(ch) = self.chars.peek() {
            match ch {
                ' ' | '\t' | '\n' | '\r' => (),
                _ => break,
            };

            // Consume next_ch
            self.consume_next_ch();
            has_newline = has_newline || ch == '\n';
            whitespace.push(ch);
        }

        Token::Whitespace {
            ws: whitespace,
            has_newline,
        }
    }

    fn lex_after_slash(&mut self) -> Token {
        match self.chars.peek() {
            Some('=') => Token::DivideEqual,
            Some('/') => self.lex_line_comment(),
            Some('*') => self.lex_block_comment(),
            _ => Token::Slash,
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

    fn lex_block_comment(&mut self) -> Token {
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
            Token::BlockComment(comment)
        } else {
            Token::UnterminatedBlockComment(comment, depth)
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

    fn lex_number(&mut self, digit: char) -> Token {
        let mut prefix = None;
        let mut value = String::new();
        let mut exponent = None;
        let mut suffix = None;
        value.push(digit);

        let mut digit_lexer_fn: fn(&mut Self, &mut String) = LexerState::lex_digits;

        // First check for prefix
        if digit == '0' {
            match self.chars.peek() {
                Some('b') => {
                    prefix = Some(NumberLiteralPrefix::Binary);
                    digit_lexer_fn = LexerState::lex_binary_digits;
                }
                Some('o') => {
                    prefix = Some(NumberLiteralPrefix::Octal);
                    digit_lexer_fn = LexerState::lex_octal_digits;
                }
                Some('x') => {
                    prefix = Some(NumberLiteralPrefix::Hexadecimal);
                    digit_lexer_fn = LexerState::lex_hexadecimal_digits;
                }
                _ => (),
            }

            if prefix.is_some() {
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
                return Token::NumberLiteral {
                    prefix,
                    value,
                    exponent,
                    suffix,
                };
            }
            self.increment_pos('.');
            value.push('.');

            digit_lexer_fn(self, &mut value);
        }

        // Check for exponent.
        if let Some('e') | Some('E') = self.chars.peek() {
            exponent = Some(self.lex_exponent());
        }

        // Check for suffix.
        let mut suffix_str = String::new();
        self.lex_identifier_into_string(&mut suffix_str);
        if !suffix_str.is_empty() {
            suffix = Some(suffix_str);
        }

        return Token::NumberLiteral {
            prefix,
            value,
            exponent,
            suffix,
        };
    }

    lex_digits_fn!(lex_binary_digits: '0'...'1', '_');
    lex_digits_fn!(lex_octal_digits: '0'...'7', '_');
    lex_digits_fn!(lex_hexadecimal_digits: '0'...'9', 'a'...'f', 'A'...'F', '_');
    lex_digits_fn!(lex_digits: '0'...'9', '_');

    fn lex_exponent(&mut self) -> String {
        let mut exponent = String::new();
        exponent.push(self.chars.next().unwrap());

        if let Some(sign @ '+') | Some(sign @ '-') = self.chars.peek() {
            exponent.push(sign);
            self.consume_next_ch();
        }

        self.lex_digits(&mut exponent);

        exponent
    }

    fn lex_quoted_string(&mut self, quote_ch: char) -> Token {
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
                        return Token::DoubleQuoteLiteral(content);
                    }
                }
                '\'' if quote_ch == '\'' => {
                    if escaping {
                        escaping = false;
                    } else {
                        return Token::SingleQuoteLiteral(content);
                    }
                }
                '\\' => {
                    escaping = !escaping;
                }
                '\n' => {
                    if quote_ch == '"' {
                        return Token::UnterminatedDoubleQuoteLiteral {
                            content,
                            via_eof: false,
                        };
                    } else {
                        return Token::UnterminatedSingleQuoteLiteral {
                            content,
                            via_eof: false,
                        };
                    }
                }
                _ => {
                    escaping = false;
                }
            }
            // Push character after we're sure it's not the end of the quote.
            content.push(ch);
        }

        if quote_ch == '"' {
            return Token::UnterminatedDoubleQuoteLiteral {
                content,
                via_eof: true,
            };
        } else {
            return Token::UnterminatedSingleQuoteLiteral {
                content,
                via_eof: true,
            };
        }
    }
}
