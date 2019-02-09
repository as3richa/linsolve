use std::io;

pub struct Stream<I: Iterator<Item = Result<u8, io::Error>>> {
    pub filename: String,
    pub line: u32,
    pub column: u32,
    pub peeked: Option<u8>,
    pub error: Option<io::Error>,
    iter: I,
}

impl<I: Iterator<Item = Result<u8, io::Error>>> Stream<I> {
    pub fn new(filename: String, iter: I) -> Stream<I> {
        Stream {
            filename: filename,
            line: 1,
            column: 1,
            peeked: None,
            error: None,
            iter: iter,
        }
    }

    pub fn next(&mut self) -> Option<u8> {
        if self.peeked == None {
            self.peek();
        }
        if let Some(byte) = self.peeked {
            if byte == b'\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        let value = self.peeked;
        self.peeked = None;
        value
    }

    pub fn peek(&mut self) -> Option<u8> {
        assert_eq!(self.peeked, None);
        if let Some(result) = self.iter.next() {
            match result {
                Ok(byte) => self.peeked = Some(byte),
                Err(error) => self.error = Some(error),
            }
        }
        self.peeked
    }
}
