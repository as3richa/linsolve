use std::io;
use std::io::Read;

mod errors;
mod lexer;
mod linear_system;
mod parser;
mod stream;

use parser::Parser;
use stream::Stream;

fn main() {
    let stdin = io::stdin();
    let handle = stdin.lock();
    let stream = Stream::new("<standard input>".to_string(), handle.bytes());
    let mut parser = Parser::new(stream);
    match parser.parse() {
        Ok(system) => println!("{:?}", system),
        Err(err) => println!("{}", err),
    }
}
