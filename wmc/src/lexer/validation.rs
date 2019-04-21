use std::char;
use std::str::Chars;

use crate::lexer::lex_error::*;
use crate::lexer::{LexedToken, Token};

pub fn check_for_lexer_errors(tokens: &[LexedToken]) {
    for token in tokens.iter() {
        println!("{:?}", token);
    }
}

pub fn validate_double_quote_literal(content: String) -> Result<Token, LexErrorKind> {
    match validate_quoted_content(&content, true) {
        Ok(actual_content) => Ok(Token::DoubleQuoteLiteral(actual_content)),
        Err(err) => Err(err),
    }
}

pub fn validate_single_quote_literal(content: String) -> Result<Token, LexErrorKind> {
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
