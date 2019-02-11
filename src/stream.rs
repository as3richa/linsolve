use std::io;
use std::rc::Rc;

#[derive(Clone)]
pub enum StreamValue {
    Byte(u8),
    EndOfFile,
    Err(Rc<io::Error>),
}

pub struct Stream<I: Iterator<Item = Result<u8, io::Error>>> {
    pub filename: String,
    pub line: u32,
    pub column: u32,
    peeked: Option<StreamValue>,
    iter: I,
}

impl<I: Iterator<Item = Result<u8, io::Error>>> Stream<I> {
    pub fn new(filename: String, iter: I) -> Stream<I> {
        Self {
            filename,
            line: 1,
            column: 1,
            peeked: None,
            iter,
        }
    }

    pub fn forward(&mut self) {
        let maybe_byte = match self.peeked.take() {
            Some(StreamValue::Byte(byte)) => Some(byte),
            Some(StreamValue::Err(_)) => None,
            Some(StreamValue::EndOfFile) => None,
            None => match self.iter.next() {
                Some(Ok(byte)) => Some(byte),
                Some(Err(_)) => None,
                None => None,
            },
        };

        if let Some(byte) = maybe_byte {
            if byte == b'\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
    }

    pub fn peek(&mut self) -> StreamValue {
        match self.peeked {
            Some(ref result) => result.clone(),
            None => {
                let result = match self.iter.next() {
                    Some(Ok(byte)) => StreamValue::Byte(byte),
                    Some(Err(error)) => StreamValue::Err(Rc::new(error)),
                    None => StreamValue::EndOfFile,
                };
                self.peeked = Some(result.clone());
                result
            }
        }
    }
}
