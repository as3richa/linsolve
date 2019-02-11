use std::error::Error;
use std::io;
use std::rc::Rc;

use crate::stream::{Stream, StreamValue};

pub enum LexResult {
    Ok(Token),
    Err(Rc<Error>),
    EndOfFile,
}

pub enum Token {
    Comment,
    Whitespace,
    EndOfLine,
    Variable(String),
}

pub struct Lexer<I: Iterator<Item = Result<u8, io::Error>>> {
    stream: Stream<I>,
}

impl<I: Iterator<Item = Result<u8, io::Error>>> Lexer<I> {
    pub fn new(stream: Stream<I>) -> Lexer<I> {
        Self { stream }
    }

    pub fn lex(&mut self) -> LexResult {
        LexResult::EndOfFile
    }

    fn skip_comment(&mut self) {}

    fn skip_whitespace(&mut self) {}

    fn skip_newlines(&mut self) {}

    fn lex_variable(&mut self) {}
}
