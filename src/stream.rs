use std::io;

pub struct Stream<I: Iterator<Item = Result<u8, io::Error>>> {
    pub filename: String,
    pub line: u32,
    pub column: u32,
    peeked: Option<u8>,
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
