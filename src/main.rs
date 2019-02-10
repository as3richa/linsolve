use std::io;
use std::io::Read;

mod stream;
use stream::{Stream, StreamValue};

fn main() {
    let stdin = io::stdin();
    let handle = stdin.lock();
    let mut ss = Stream::new("<standard input>".to_string(), handle.bytes());

    loop {
        match ss.peek() {
            StreamValue::Byte(byte) => {
                println!("{}:{}:{}: {}", ss.filename, ss.line, ss.column, byte)
            }
            StreamValue::Err(error) => {
                println!("Error: {}", error);
                break;
            }
            StreamValue::EndOfFile => {
                println!("EOF");
                break;
            }
        }
        ss.forward();
    }
}
