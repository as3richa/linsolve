use std::io;
use std::io::Read;

mod errors;
mod lexer;
mod linear_system;
mod parser;
mod stream;

use stream::Stream;

fn main() {
    let stdin = io::stdin();
    let handle = stdin.lock();
    let mut stream = Stream::new("<standard input>".to_string(), handle.bytes());

    let mut system = parser::parse(&mut stream).unwrap();
    println!("{:?}\n\n\n", system);
    system.solve();
    println!("{:?}\n\n\n", system);
}
