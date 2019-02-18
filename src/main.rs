use std::io;
use std::io::Read;

mod errors;
mod lexer;
mod parser;
mod stream;

use parser::Parser;
use stream::Stream;

fn main() {
    let stdin = io::stdin();
    let handle = stdin.lock();
    let stream = Stream::new("<standard input>".to_string(), handle.bytes());
    let mut parser = Parser::new(stream);
    if let Err(error) = parser.parse() {
        println!("{}", error);
    }
}
