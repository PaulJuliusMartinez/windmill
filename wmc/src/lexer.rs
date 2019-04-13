use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
pub enum Token {
    Whitespace,
    Comment(String),
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
    // Pound,
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

                // '#' => Token::Pound,
                // '$' => Token::Dollar,
                ',' => Token::Comma,
                '.' => Token::Dot,
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
                '/' => self.maybe_lex_ch_eq_token(Token::Slash, Token::DivideEqual),
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
}
