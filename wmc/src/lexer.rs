use std::collections::HashMap;
use std::iter::Peekable;
use std::str::Chars;

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
    Whitespace,
    LineComment(String),
    BlockComment(String),
    UnterminatedBlockComment(String, u32),
    Identifier(String),
    DoubleQuoteLiteral(String),
    SingleQuoteLiteral(String),
    NumberLiteral(String),
    Unknown(char),

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
    chars: Peekable<Chars<'a>>,
    line_no: i32,
    col_no: i32,
    tokens: Vec<LexedToken>,
}

pub fn lex(src: &str) -> Vec<LexedToken> {
    let mut state = LexerState {
        chars: src.chars().peekable(),
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

                // "/", "/=", or comments
                '/' => self.lex_after_slash(),

                // Lex identifiers and keywords.
                letter @ '_' | letter @ 'a'...'z' | letter @ 'A'...'Z' => {
                    self.lex_identifier_or_keyword(letter)
                }

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

    fn maybe_lex_ch_eq_token(&mut self, one_ch_token: Token, two_ch_token: Token) -> Token {
        let peeked = self.chars.peek();
        if peeked.is_none() {
            return one_ch_token;
        }
        let ch = *peeked.unwrap();

        if ch == '=' {
            // Consume =
            self.chars.next();
            self.increment_pos(ch);
            two_ch_token
        } else {
            one_ch_token
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
        let next_ch = *peeked.unwrap();

        if next_ch == ch {
            // Consume next_ch
            self.chars.next();
            self.increment_pos(ch);
            self.maybe_lex_ch_eq_token(double_ch_token, double_ch_eq_token)
        } else if next_ch == '=' {
            // Consume =
            self.chars.next();
            self.increment_pos(ch);
            ch_eq_token
        } else {
            single_ch_token
        }
    }

    fn lex_after_slash(&mut self) -> Token {
        match self.chars.peek() {
            Some(&'=') => Token::DivideEqual,
            Some(&'/') => self.lex_line_comment(),
            Some(&'*') => self.lex_block_comment(),
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

        loop {
            match self.chars.peek() {
                Some(&ch @ '_')
                | Some(&ch @ 'a'...'z')
                | Some(&ch @ 'A'...'Z')
                | Some(&ch @ '0'...'9') => {
                    // Consume ch
                    self.chars.next();
                    self.increment_pos(ch);
                    identifier.push(ch);
                }
                _ => {
                    if let Some(keyword_token) = KEYWORDS.get(identifier.as_str()) {
                        return keyword_token.clone();
                    } else {
                        return Token::Identifier(identifier);
                    }
                }
            }
        }
    }
}
