use std::io;
use std::io::Read;

mod errors;
mod lexer;
mod parser;
mod renderers;
mod solver;
mod stream;

fn main() {
    let stdin = io::stdin();
    let in_handle = stdin.lock();
    let mut stream = stream::Stream::new("<standard input>".to_string(), in_handle.bytes());

    let system = parser::parse(&mut stream).unwrap();
    system.solve();
}
