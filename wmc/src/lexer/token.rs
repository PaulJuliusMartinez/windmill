use std::collections::HashMap;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NumberLiteral {
    Binary,
    Octal,
    Hexadecimal,
    Decimal,
}

impl NumberLiteral {
    pub fn radix(&self) -> u32 {
        match self {
            NumberLiteral::Binary => 2,
            NumberLiteral::Octal => 8,
            NumberLiteral::Decimal => 10,
            NumberLiteral::Hexadecimal => 16,
        }
    }
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

    // More complex tokens
    Whitespace(String),
    LineComment(String),
    BlockComment(String),
    Identifier(String),
    DoubleQuoteLiteral(String),
    SingleQuoteLiteral(char),
    NumberLiteral {
        style: NumberLiteral,
        value: String,
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
    pub static ref KEYWORDS: HashMap<&'static str, Token> = {
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
