use std::io;
use std::io::Read;

mod stream;
use stream::Stream;

mod lexer;
use lexer::{Lexer, LexResult};

fn main() {
    let stdin = io::stdin();
    let handle = stdin.lock();
    let stream = Stream::new("<standard input>".to_string(), handle.bytes());
    let mut lexer = Lexer::new(stream);

    loop {
        match lexer.lex() {
            LexResult::EndOfFile => break,
            _ => unreachable!()
        }
    }
}
