use std::io;
use std::io::Read;

mod errors;
mod lexer;
mod linear_system;
mod parser;
mod renderers;
mod stream;

use stream::Stream;

fn main() {
    let stdin = io::stdin();
    let handle = stdin.lock();
    let mut stream = Stream::new("<standard input>".to_string(), handle.bytes());

    let system = parser::parse(&mut stream).unwrap();
    system.solve();
}
