use std::error;
use std::fmt;
use std::io;
use std::rc::Rc;

use crate::stream::{Stream, StreamValue};

pub enum LexResult {
    Ok(Token),
    EndOfFile,
    Err(Rc<dyn error::Error>),
}

#[derive(Debug)]
struct LexError {
    desc: String,
}

impl LexError {
    fn new(filename: &str, line: u32, column: u32, message: &str) -> LexError {
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

pub enum TokenData {
    Whitespace,
    Comment,
    EndOfLine,
}

pub struct Token {
    pub data: TokenData,
    pub line: u32,
    pub column: u32,
}

pub struct Lexer<I: Iterator<Item = Result<u8, io::Error>>> {
    stream: Stream<I>,
}

impl<I: Iterator<Item = Result<u8, io::Error>>> Lexer<I> {
    pub fn new(stream: Stream<I>) -> Lexer<I> {
        Self { stream }
    }

    pub fn lex(&mut self) -> LexResult {
        match self.stream.peek() {
            StreamValue::Byte(byte) => self.lex_something(byte),
            StreamValue::Err(error) => LexResult::Err(error),
            StreamValue::EndOfFile => LexResult::EndOfFile,
        }
    }

    fn lex_something(&mut self, byte: u8) -> LexResult {
        let line = self.stream.line;
        let column = self.stream.column;

        let result = match byte {
            b'#' => self.lex_comment(),
            b' ' | b'\t' => self.lex_whitespace(),
            b'\r' | b'\n' => self.lex_end_of_line(),
            _ => {
                let message = format!(
                    "Non-ASCII byte {:#x} (Unicode is permitted only in comments)",
                    byte
                );
                let error = LexError::new(
                    &self.stream.filename,
                    self.stream.line,
                    self.stream.column,
                    &message,
                );
                let rc: Rc<error::Error> = Rc::new(error);
                Err(rc)
            }
        };

        match result {
            Ok(data) => LexResult::Ok(Token { data, line, column }),
            Err(error) => LexResult::Err(error),
        }
    }

    fn lex_comment(&mut self) -> Result<TokenData, Rc<dyn error::Error>> {
        loop {
            self.stream.forward();
            match self.stream.peek() {
                StreamValue::Byte(byte) => {
                    if byte == b'\r' || byte == b'\n' {
                        return Ok(TokenData::Comment);
                    }
                }
                StreamValue::EndOfFile => return Ok(TokenData::Comment),
                StreamValue::Err(error) => return Err(error),
            }
        }
    }

    fn lex_whitespace(&mut self) -> Result<TokenData, Rc<dyn error::Error>> {
        loop {
            self.stream.forward();
            match self.stream.peek() {
                StreamValue::Byte(byte) => {
                    if byte != b' ' && byte != b'\t' {
                        return Ok(TokenData::Whitespace);
                    }
                }
                StreamValue::EndOfFile => return Ok(TokenData::Whitespace),
                StreamValue::Err(error) => return Err(error),
            }
        }
    }

    fn lex_end_of_line(&mut self) -> Result<TokenData, Rc<dyn error::Error>> {
        loop {
            self.stream.forward();
            match self.stream.peek() {
                StreamValue::Byte(byte) => {
                    if byte != b'\r' && byte != b'\n' {
                        return Ok(TokenData::EndOfLine);
                    }
                }
                StreamValue::EndOfFile => return Ok(TokenData::EndOfLine),
                StreamValue::Err(error) => return Err(error),
            }
        }
    }
}
