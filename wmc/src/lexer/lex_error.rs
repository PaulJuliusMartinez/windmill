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
    NoNumbers(String),
    InvalidNumericSuffix(String),
    IntegerSuffixForFloatingPoint(String, String),
}
