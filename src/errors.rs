use std::error;
use std::fmt;
use std::io;
use std::ops::Deref;

#[derive(Debug)]
pub enum ErrorBox {
    ParseOrLex(ParseOrLexError),
    Io(io::Error),
}

impl From<ParseOrLexError> for ErrorBox {
    fn from(error: LexError) -> Self {
        ErrorBox::ParseOrLex(error)
    }
}

impl From<io::Error> for ErrorBox {
    fn from(error: io::Error) -> Self {
        ErrorBox::Io(error)
    }
}

impl Deref for ErrorBox {
    type Target = error::Error;

    fn deref(&self) -> &Self::Target {
        match self {
            ErrorBox::ParseOrLex(ref error) => error,
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
