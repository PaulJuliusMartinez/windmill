use std::char;
use std::str::Chars;

use crate::lexer::{NumberLiteral, Token};

#[derive(Debug)]
pub struct LexError<'a> {
    pub location: LexErrorLocation<'a>,
    pub kind: LexErrorKind,
}

#[derive(Debug)]
pub struct LexErrorLocation<'a> {
    pub filename: &'a str,
    pub line_no: u32,
    pub col_no: u32,
}

#[derive(Debug)]
pub enum LexErrorKind {
    // Generic
    UnknownChar(char),
    // Strings
    QuoteUnknownEscape {
        escape_char: char,
        index: u32,
        is_double: bool,
    },
    QuoteMalformedUnicodeEscape {
        unexpected_char: Option<char>,
        index: u32,
        is_double: bool,
    },
    QuoteInvalidEscapedUnicodeCodepoint {
        codepoint: String,
        index: u32,
        is_double: bool,
    },
    UnterminatedQuote {
        content: String,
        via_eof: bool,
        is_double: bool,
    },
    SingleQuoteNotSingleCharacter(String),
    // Comments
    UnterminatedBlockComment(String, u32),
    // Numbers
    NoNumbers(NumberLiteral, String),
    InvalidDigitForLiteral(NumberLiteral, char),
    InvalidNumericSuffix(NumberLiteral, String, String),
    NonDecimalFloatingPoint(NumberLiteral, String),
    IntegerSuffixForFloatingPoint(String, String),
}

pub type LexResult = Result<Token, LexErrorKind>;

pub fn validate_double_quote_literal(content: String) -> LexResult {
    match validate_quoted_content(&content, true) {
        Ok(actual_content) => Ok(Token::DoubleQuoteLiteral(actual_content)),
        Err(err) => Err(err),
    }
}

pub fn validate_single_quote_literal(content: String) -> LexResult {
    match validate_quoted_content(&content, false) {
        Ok(actual_content) => {
            if actual_content.chars().count() != 1 {
                Err(LexErrorKind::SingleQuoteNotSingleCharacter(content))
            } else {
                Ok(Token::SingleQuoteLiteral(
                    actual_content.chars().next().unwrap(),
                ))
            }
        }
        Err(err) => Err(err),
    }
}

fn validate_quoted_content(content: &String, is_double: bool) -> Result<String, LexErrorKind> {
    let mut actual_content = String::new();
    let mut chars = content.chars();
    let mut index = 0;
    while let Some(ch) = chars.next() {
        match ch {
            '\\' => {
                match validate_escape(&mut chars, is_double, index) {
                    Ok((ch, chars_used)) => {
                        index += chars_used;
                        actual_content.push(ch);
                    }
                    Err(err) => return Err(err),
                };
            }
            normal_ch => {
                index += 1;
                actual_content.push(normal_ch);
            }
        }
    }

    Ok(actual_content)
}

fn validate_escape(
    chars: &mut Chars,
    is_double: bool,
    chars_used: u32,
) -> Result<(char, u32), LexErrorKind> {
    while let Some(ch) = chars.next() {
        match ch {
            '\'' => return Ok(('\'', 2)),
            '\"' => return Ok(('\"', 2)),
            '\\' => return Ok(('\\', 2)),
            'n' => return Ok(('\n', 2)),
            'r' => return Ok(('\r', 2)),
            't' => return Ok(('\t', 2)),
            '0' => return Ok(('\0', 2)),
            'u' => break,
            other_ch => {
                return Err(LexErrorKind::QuoteUnknownEscape {
                    escape_char: other_ch,
                    index: chars_used + 1,
                    is_double,
                });
            }
        }
    }

    match chars.next() {
        Some('{') => (),
        unexpected_char => {
            return Err(LexErrorKind::QuoteMalformedUnicodeEscape {
                unexpected_char,
                index: chars_used,
                is_double,
            });
        }
    }

    let mut escape = String::new();
    let mut more_chars_used = 3; // \u{

    loop {
        match chars.next() {
            Some(ch @ '0'...'9') | Some(ch @ 'a'...'z') | Some(ch @ 'A'...'Z') | Some(ch @ '_') => {
                escape.push(ch);
                more_chars_used += 1;
            }
            Some('}') => break,
            unexpected_char => {
                return Err(LexErrorKind::QuoteMalformedUnicodeEscape {
                    unexpected_char,
                    index: chars_used,
                    is_double,
                });
            }
        }
    }
    more_chars_used += 1;

    match u32::from_str_radix(&escape, 16) {
        Ok(codepoint) => match char::from_u32(codepoint) {
            Some(ch) => Ok((ch, more_chars_used)),
            _ => Err(LexErrorKind::QuoteInvalidEscapedUnicodeCodepoint {
                codepoint: escape,
                index: 3,
                is_double,
            }),
        },
        _ => Err(LexErrorKind::QuoteInvalidEscapedUnicodeCodepoint {
            codepoint: escape,
            index: 3,
            is_double,
        }),
    }
}

pub fn validate_number_literal(
    style: NumberLiteral,
    mut value: String,
    suffix: Option<String>,
) -> LexResult {
    let mut has_num = false;
    let mut has_decimal = false;
    let radix = style.radix();

    for ch in value.chars() {
        if ch != '_' && ch != '.' && !ch.is_digit(radix) {
            return Err(LexErrorKind::InvalidDigitForLiteral(style, ch));
        }
        has_num = has_num || ch != '_';
        has_decimal = has_decimal || ch == '.';
    }

    if !has_num {
        if let Some(suffix) = suffix {
            value.push_str(&suffix);
        }
        return Err(LexErrorKind::NoNumbers(style, value));
    }

    if has_decimal && style != NumberLiteral::Decimal {
        if let Some(suffix) = suffix {
            value.push_str(&suffix);
        }
        return Err(LexErrorKind::NonDecimalFloatingPoint(style, value));
    }

    if suffix.is_none() {
        return Ok(Token::NumberLiteral {
            style,
            value,
            suffix,
        });
    }

    let suffix = suffix.unwrap();
    let (is_integer_suffix, is_floating_point_suffix) = suffix_type(&suffix);

    if has_decimal && is_integer_suffix {
        return Err(LexErrorKind::IntegerSuffixForFloatingPoint(value, suffix));
    }

    if style != NumberLiteral::Decimal && is_floating_point_suffix {
        value.push_str(&suffix);
        return Err(LexErrorKind::NonDecimalFloatingPoint(style, value));
    }

    if !is_integer_suffix && !is_floating_point_suffix {
        return Err(LexErrorKind::InvalidNumericSuffix(style, value, suffix));
    }

    Ok(Token::NumberLiteral {
        style,
        value,
        suffix: Some(suffix),
    })
}

fn suffix_type(s: &str) -> (bool, bool) {
    match s {
        "i8" | "i16" | "i32" | "i64" | "isize" | "u8" | "u16" | "u32" | "u64" | "usize" => {
            (true, false)
        }
        "f32" | "f64" => (false, true),
        _ => (false, false),
    }
}

impl LexErrorKind {
    fn offset(&self) -> u32 {
        match self {
            LexErrorKind::QuoteUnknownEscape { index, .. } => *index,
            LexErrorKind::QuoteMalformedUnicodeEscape { index, .. } => *index,
            LexErrorKind::QuoteInvalidEscapedUnicodeCodepoint { index, .. } => *index,
            _ => 0,
        }
    }

    fn quote_type(is_double: bool) -> &'static str {
        if is_double {
            "string literal"
        } else {
            "character literal"
        }
    }

    fn eprint(&self) {
        match self {
            LexErrorKind::UnknownChar(ch) => {
                eprintln!("Unknown character: '{}'", ch);
            }
            LexErrorKind::QuoteUnknownEscape {
                escape_char,
                is_double,
                ..
            } => {
                eprintln!(
                    "Unknown escape character in {}: '{}'",
                    LexErrorKind::quote_type(*is_double),
                    escape_char,
                );
            }
            LexErrorKind::QuoteMalformedUnicodeEscape {
                unexpected_char,
                is_double,
                ..
            } => match unexpected_char {
                None => eprintln!("Unterminated Unicode escape."),
                Some(ch) => {
                    eprintln!("Unexpected character in Unicode escape sequence: '{}'", ch);
                }
            },
            LexErrorKind::QuoteInvalidEscapedUnicodeCodepoint {
                codepoint,
                is_double,
                ..
            } => {
                eprintln!(
                    "Invalid Unicode codepoint; expected hexadecimal \
                     value between 0 and 10FFFF: {}",
                    codepoint,
                );
            }
            LexErrorKind::UnterminatedQuote {
                content,
                via_eof,
                is_double,
            } => {
                if *via_eof {
                    eprintln!(
                        "EOF encountered during {}: \"{}\"",
                        LexErrorKind::quote_type(*is_double),
                        content
                    );
                } else {
                    eprintln!(
                        "Newline encountered during {}: \"{}",
                        LexErrorKind::quote_type(*is_double),
                        content
                    );
                }
            }
            LexErrorKind::SingleQuoteNotSingleCharacter(s) => {
                eprintln!(
                    "Character literal doesn't contain a single character: '{}'",
                    s
                );
            }
            LexErrorKind::UnterminatedBlockComment(_, _) => {
                eprintln!("EOF encountered during multi-line block comment");
            }
            LexErrorKind::NoNumbers(style, s) => {
                eprintln!(
                    "{} literal requires at least one digit: {}{}",
                    style.to_s(),
                    style.prefix(),
                    s
                );
            }
            LexErrorKind::InvalidDigitForLiteral(style, ch) => {
                eprintln!("Invalid digit in {} number literal: {}", style.to_s(), ch);
            }
            LexErrorKind::InvalidNumericSuffix(style, num, suffix) => {
                eprintln!(
                    "Unknown numeric suffix \"{}\" in {} literal {}{}",
                    suffix,
                    style.to_s(),
                    num,
                    suffix,
                );
            }
            LexErrorKind::NonDecimalFloatingPoint(style, num_str) => {
                eprintln!(
                    "Floating point {} literals are not supported: {}",
                    style.to_s(),
                    num_str,
                );
            }
            LexErrorKind::IntegerSuffixForFloatingPoint(num_str, suffix) => {
                eprintln!(
                    "Invalid integer suffix \"{}\" for floating point literal {}{}",
                    suffix, num_str, suffix,
                );
            }
        }
    }
}

impl<'a> LexError<'a> {
    pub fn eprint(&'a self) {
        let LexErrorLocation {
            filename,
            line_no,
            col_no,
        } = self.location;
        eprint!("{}:{}:{} ", filename, line_no, col_no + self.kind.offset());
        self.kind.eprint();
    }
}
