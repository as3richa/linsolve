use std::error;
use std::fmt;
use std::io;
use std::ops::Deref;

pub enum ErrorBox {
    Lex(LexError),
    Io(io::Error),
}

impl ErrorBox {
    pub fn from_lex_error(error: LexError) -> ErrorBox {
        ErrorBox::Lex(error)
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
            ErrorBox::Io(ref error) => error,
        }
    }
}

impl fmt::Display for ErrorBox {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{}", *self)
    }
}

#[derive(Debug)]
pub struct LexError {
    desc: String,
}

impl LexError {
    pub fn new(filename: &str, line: u32, column: u32, message: &str) -> LexError {
        let mut desc = filename.to_string();
        desc += ":";
        desc += &line.to_string();
        desc += ":";
        desc += &column.to_string();
        desc += ": ";
        desc += message;
        LexError { desc }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{}", &self.desc)
    }
}

impl error::Error for LexError {
    fn description(&self) -> &str {
        &self.desc
    }
}
