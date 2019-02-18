use std::error;
use std::fmt;
use std::io;
use std::ops::Deref;

// #[derive(Debug)]
pub enum ErrorBox {
    Lex(LexError),
    Parse(ParseError),
    Io(io::Error),
}

impl ErrorBox {
    pub fn from_lex_error(error: LexError) -> ErrorBox {
        ErrorBox::Lex(error)
    }

    pub fn from_parse_error(error: ParseError) -> ErrorBox {
        ErrorBox::Parse(error)
    }

    pub fn from_io_error(error: io::Error) -> ErrorBox {
        ErrorBox::Io(error)
    }
}

impl Deref for ErrorBox {
    type Target = error::Error;

    fn deref(&self) -> &Self::Target {
        match self {
            ErrorBox::Lex(ref error) => error,
            ErrorBox::Parse(ref error) => error,
            ErrorBox::Io(ref error) => error,
        }
    }
}

impl fmt::Display for ErrorBox {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&**self, formatter)
    }
}

#[derive(Debug)]
pub struct ParseOrLexError {
    desc: String,
}

pub type LexError = ParseOrLexError;
pub type ParseError = ParseOrLexError;

impl ParseOrLexError {
    pub fn new(filename: &str, line: u32, column: u32, message: &str) -> ParseOrLexError {
        let mut desc = filename.to_string();
        desc += ":";
        desc += &line.to_string();
        desc += ":";
        desc += &column.to_string();
        desc += ": ";
        desc += message;
        ParseOrLexError { desc }
    }
}

impl fmt::Display for ParseOrLexError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{}", &self.desc)
    }
}

impl error::Error for ParseOrLexError {
    fn description(&self) -> &str {
        &self.desc
    }
}
