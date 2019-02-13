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
                    TokenData::Variable(identifier) => "variable ".to_string() + &identifier,
                    TokenData::Decimal(value) => "decimal ".to_string() + &value,
                    TokenData::Whitespace => "whitespace".to_string(),
                    TokenData::Comment => "comment".to_string(),
                    TokenData::EndOfLine => "end of line".to_string(),
                    TokenData::Plus => "plus".to_string(),
                    TokenData::Minus => "minus".to_string(),
                    TokenData::Times => "times".to_string(),
                    TokenData::DividedBy => "divided by".to_string(),
                    TokenData::Equals => "equals".to_string(),
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
