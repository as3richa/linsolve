use std::io;
use std::io::Read;
mod stream;

fn main() {
    let stdin = io::stdin();
    let handle = stdin.lock();
    let mut ss = stream::Stream::new("<standard input>".to_string(), handle.bytes());

    loop {
        match ss.peek() {
            Some(byte) => println!("{}:{}:{}: {}", ss.filename, ss.line, ss.column, byte),
            None => break,
        }
        ss.next();
    }
}
