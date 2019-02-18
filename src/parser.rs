use std::io;

use crate::errors::{ErrorBox, ParseError};
use crate::stream::Stream;
use lexer::{Lexer, Token, TokenData};

pub struct Parser<I: Iterator<Item = Result<u8, io::Error>>> {
    lexer: Lexer<I>,
}

#[derive(Debug)]
enum Term {
    Constant(f64),
    Linear(f64, String),
}

macro_rules! parse_error {
    ($filename:expr, $line:expr, $column:expr, $message:expr) => {{
        let error = ParseError::new(&$filename, $line, $column, &$message);
        Err(ErrorBox::from_parse_error(error))
    }};
}

macro_rules! token_data_matches {
    ($maybe_token:ident, $data:pat) => {{
        if let Some(Token { data: $data, .. }) = $maybe_token {
            true
        } else {
            false
        }
    }};
}

impl<I: Iterator<Item = Result<u8, io::Error>>> Parser<I> {
    pub fn new(stream: Stream<I>) -> Parser<I> {
        let lexer = Lexer::new(stream);
        Self { lexer }
    }

    pub fn parse(&mut self) -> Result<(), ErrorBox> {
        let mut token = self.lex()?;

        while let Some(_) = token {
            if token_data_matches!(token, TokenData::EndOfLine) {
                continue;
            }

            println!("LHS");

            loop {
                let (term, next_token) = self.parse_term(token)?;
                token = next_token;

                println!("{:?}", term);

                if !token_data_matches!(token, TokenData::Plus)
                    && !token_data_matches!(token, TokenData::Minus)
                {
                    break;
                }
            }

            match token {
                Some(ref wrapped) => {
                    match wrapped.data {
                        TokenData::Equals => (),
                        _ => {
                            return parse_error!(
                                self.lexer.stream.filename,
                                wrapped.line,
                                wrapped.column,
                                "expected an equals sign"
                            );
                        }
                    };
                }
                None => (),
            }

            token = self.lex()?;

            println!("RHS");

            loop {
                let (term, next_token) = self.parse_term(token)?;
                token = next_token;

                println!("{:?}", term);

                if !token_data_matches!(token, TokenData::Plus)
                    && !token_data_matches!(token, TokenData::Minus)
                {
                    break;
                }
            }

            match token {
                Some(ref wrapped) => {
                    match wrapped.data {
                        TokenData::EndOfLine => (),
                        _ => {
                            return parse_error!(
                                self.lexer.stream.filename,
                                wrapped.line,
                                wrapped.column,
                                "expected a new line or the end of the file"
                            );
                        }
                    };
                }
                None => (),
            }

            token = self.lex()?;

            println!("End\n\n\n");
        }

        Ok(())
    }

    fn parse_term(&mut self, mut token: Option<Token>) -> Result<(Term, Option<Token>), ErrorBox> {
        enum State {
            Empty,
            Plus,
            Minus,
            Constant(f64),
            Linear(f64, String),
        }

        let mut state = State::Empty;
        let mut last: Option<Token> = None;

        loop {
            let unwrapped = match token {
                Some(wrapped) => wrapped,
                None => break,
            };

            match unwrapped.data {
                TokenData::Variable(identifier) => {
                    state = match state {
                        State::Empty | State::Plus => State::Linear(1.0, identifier),
                        State::Minus => State::Linear(-1.0, identifier),
                        State::Constant(coeff) => State::Linear(coeff, identifier),
                        State::Linear(_, _) => {
                            let error = parse_error!(
                                self.lexer.stream.filename,
                                unwrapped.line,
                                unwrapped.column,
                                "term is not linear"
                            );
                            return error;
                        }
                    };
                }

                TokenData::Decimal(string_value) => {
                    let value = string_value.parse::<f64>().unwrap();
                    state = match state {
                        State::Empty | State::Plus => State::Constant(value),
                        State::Minus => State::Constant(-1.0 * value),
                        State::Constant(coeff) => State::Constant(coeff * value),
                        State::Linear(coeff, identifier) => {
                            State::Linear(coeff * value, identifier)
                        }
                    };
                }

                TokenData::Times => {
                    let next_token = match self.lex()? {
                        Some(token) => token,
                        None => {
                            let error = parse_error!(
                                self.lexer.stream.filename,
                                unwrapped.line,
                                unwrapped.column,
                                "expected a number or variable"
                            );
                            return error;
                        }
                    };

                    match next_token.data {
                        TokenData::Variable(identifier) => {
                            state = match state {
                                State::Empty | State::Plus => State::Linear(1.0, identifier),
                                State::Minus => State::Linear(-1.0, identifier),
                                State::Constant(coeff) => State::Linear(coeff, identifier),
                                State::Linear(_, _) => {
                                    let error = parse_error!(
                                        self.lexer.stream.filename,
                                        next_token.line,
                                        next_token.column,
                                        "term is not linear"
                                    );
                                    return error;
                                }
                            };
                        }
                        TokenData::Decimal(string_value) => {
                            let value = string_value.parse::<f64>().unwrap();
                            state = match state {
                                State::Empty | State::Plus | State::Minus => {
                                    return parse_error!(
                                        self.lexer.stream.filename,
                                        unwrapped.line,
                                        unwrapped.column,
                                        "expected a number or variable"
                                    );
                                }
                                State::Constant(coeff) => State::Constant(coeff * value),
                                State::Linear(coeff, identifier) => {
                                    State::Linear(coeff * value, identifier)
                                }
                            };
                        }
                        _ => {
                            let error = parse_error!(
                                self.lexer.stream.filename,
                                next_token.line,
                                next_token.column,
                                "expected a number or variable"
                            );
                            return error;
                        }
                    }
                }

                TokenData::DividedBy => {
                    let next_token = match self.lex()? {
                        Some(token) => token,
                        None => {
                            return parse_error!(
                                self.lexer.stream.filename,
                                self.lexer.stream.line,
                                self.lexer.stream.column,
                                "expected a number"
                            );
                        }
                    };

                    match next_token.data {
                        TokenData::Decimal(string_value) => {
                            let value = string_value.parse::<f64>().unwrap();
                            state = match state {
                                State::Empty | State::Plus | State::Minus => {
                                    return parse_error!(
                                        self.lexer.stream.filename,
                                        unwrapped.line,
                                        unwrapped.column,
                                        "expected a number or variable"
                                    );
                                }
                                State::Constant(coeff) => State::Constant(coeff / value),
                                State::Linear(coeff, identifier) => {
                                    State::Linear(coeff / value, identifier)
                                }
                            };
                        }
                        _ => {
                            return parse_error!(
                                self.lexer.stream.filename,
                                next_token.line,
                                next_token.column,
                                "expected a number"
                            );
                        }
                    }
                }

                TokenData::Plus => {
                    state = match state {
                        State::Empty | State::Plus => State::Plus,
                        State::Minus => State::Minus,
                        _ => {
                            last = Some(unwrapped);
                            break;
                        }
                    };
                }

                TokenData::Minus => {
                    state = match state {
                        State::Empty | State::Plus => State::Minus,
                        State::Minus => State::Plus,
                        _ => {
                            last = Some(unwrapped);
                            break;
                        }
                    };
                }

                TokenData::Equals | TokenData::EndOfLine => {
                    last = Some(unwrapped);
                    break;
                }

                TokenData::Whitespace | TokenData::Comment => unreachable!(),
            }

            token = self.lex()?;
        }

        match state {
            State::Constant(value) => Ok((Term::Constant(value), last)),
            State::Linear(coeff, identifier) => Ok((Term::Linear(coeff, identifier), last)),
            State::Empty | State::Plus | State::Minus => parse_error!(
                self.lexer.stream.filename,
                self.lexer.stream.line,
                self.lexer.stream.column,
                "expected a number or variable"
            ),
        }
    }

    fn lex(&mut self) -> Result<Option<Token>, ErrorBox> {
        loop {
            let result = self.lexer.lex();

            if let Ok(Some(token)) = result {
                match token.data {
                    TokenData::Whitespace | TokenData::Comment => continue,
                    _ => return Ok(Some(token)),
                }
            }

            return result;
        }
    }
}
