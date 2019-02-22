use std::io;
use std::io::{Bytes, Read};

pub struct Stream<R: Read> {
    pub filename: String,
    pub line: u32,
    pub column: u32,
    peeked: Option<u8>,
    iter: Bytes<R>,
}

impl<R: Read> Stream<R> {
    pub fn new(filename: String, iter: Bytes<R>) -> Stream<R> {
        Self {
            filename,
            line: 1,
            column: 1,
            peeked: None,
            iter,
        }
    }

    pub fn forward(&mut self) {
        match self.peeked.take() {
            Some(byte) => {
                if byte == b'\n' {
                    self.line += 1;
                    self.column = 1;
                } else {
                    self.column += 1;
                }
            }
            None => assert!(false),
        };
    }

    pub fn peek(&mut self) -> Result<Option<u8>, io::Error> {
        match self.peeked {
            Some(byte) => Ok(Some(byte)),
            None => match self.iter.next() {
                Some(Ok(byte)) => {
                    self.peeked = Some(byte);
                    Ok(Some(byte))
                }
                Some(Err(error)) => Err(error),
                None => Ok(None),
            },
        }
    }
}
