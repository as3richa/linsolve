use std::io;
use std::io::Read;

mod stream;
use stream::Stream;

mod lexer;
use lexer::{LexResult, Lexer, TokenData};

fn main() {
    let stdin = io::stdin();
    let handle = stdin.lock();
    let stream = Stream::new("<standard input>".to_string(), handle.bytes());
    let mut lexer = Lexer::new(stream);

    loop {
        match lexer.lex() {
            LexResult::Ok(token) => {
                let description = match token.data {
                    TokenData::Whitespace => "whitespace",
                    TokenData::Comment => "comment",
                    TokenData::EndOfLine => "end of line",
                };
                println!("{}:{}: {}", token.line, token.column, description);
            }
            LexResult::EndOfFile => {
                println!("End of file");
                break;
            }
            LexResult::Err(error) => {
                println!("Error: {}", error);
                break;
            }
        }
    }
}
