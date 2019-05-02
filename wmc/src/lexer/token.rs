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

    pub fn prefix(&self) -> &'static str {
        match self {
            NumberLiteral::Binary => "0b",
            NumberLiteral::Octal => "0o",
            NumberLiteral::Decimal => "",
            NumberLiteral::Hexadecimal => "0x",
        }
    }

    pub fn to_s(&self) -> &'static str {
        match self {
            NumberLiteral::Binary => "binary",
            NumberLiteral::Octal => "octal",
            NumberLiteral::Decimal => "decimal",
            NumberLiteral::Hexadecimal => "hexadecimal",
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
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
    KwInterface,
    KwStruct,
    KwVariant,

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

impl Token {
    pub fn is_meaningful_for_parsing(&self) -> bool {
        match self {
            Token::Whitespace(_) | Token::LineComment(_) | Token::BlockComment(_) => false,
            _ => true,
        }
    }
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
        m.insert("Self", Token::KwSelfType);
        m.insert("interface", Token::KwInterface);
        m.insert("struct", Token::KwStruct);
        m.insert("variant", Token::KwVariant);
        m
    };
}
