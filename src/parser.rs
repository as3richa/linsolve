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
    ($self:ident, $token:ident, $message:expr) => {{
        let error = ParseError::new(&$self.lexer.stream.filename, $token.line, $token.column, &$message);
        Err(ErrorBox::from_parse_error(error))
    }};

    ($self:ident, $message:expr) => {{
        let error = ParseError::new(
            &$self.lexer.stream.filename,
            $self.lexer.stream.line,
            $self.lexer.stream.column,
            &$message,
        );
        Err(ErrorBox::from_parse_error(error))
    }};
}

impl<I: Iterator<Item = Result<u8, io::Error>>> Parser<I> {
    pub fn new(stream: Stream<I>) -> Parser<I> {
        let lexer = Lexer::new(stream);
        Self { lexer }
    }

    pub fn parse(&mut self) -> Result<(), ErrorBox> {
        loop {
            println!("Left-hand side:");
            let lhs_ = match self.parse_lhs()? {
                Some(expr) => expr,
                None => break,
            };
            println!("Right-hand side:");
            let rhs_ = self.parse_rhs()?;
        }

        Ok(())
    }

    fn parse_lhs(&mut self) -> Result<Option<()>, ErrorBox> {
        let first = {
            loop {
                match self.lex()? {
                    Some(token) => match token.data {
                        TokenData::EndOfLine => continue,
                        _ => break token,
                    },
                    None => return Ok(None),
                }
            }
        };

        let (expr, last) = self.parse_expr(first)?;

        match last {
            Some(last) => match last.data {
                TokenData::Equals => Ok(Some(expr)),
                _ => parse_error!(self, last, "expected `=` after left-hand side of equation"),
            },
            None => parse_error!(self, "expected `=` after left-hand side of equation"),
        }
    }

    fn parse_rhs(&mut self) -> Result<(), ErrorBox> {
        let first = {
            match self.lex()? {
                Some(token) => match token.data {
                    TokenData::EndOfLine => return parse_error!(self, token, "expected an expression after `=`"),
                    _ => token,
                },
                None => return parse_error!(self, "expected an expression after `=`"),
            }
        };

        let (expr, last) = self.parse_expr(first)?;

        match last {
            Some(last) => match last.data {
                TokenData::EndOfLine => Ok(expr),
                _ => parse_error!(self, last, "expected end of file or end of line after equation"),
            },
            None => Ok(expr),
        }
    }

    fn parse_expr(&mut self, first: Token) -> Result<((), Option<Token>), ErrorBox> {
        let mut token = first;

        loop {
            let (term, next) = self.parse_term(token)?;

            println!("{:?}", term);

            match next {
                Some(next) => match next.data {
                    TokenData::Plus | TokenData::Minus => {
                        token = next;
                        continue;
                    }
                    _ => return Ok(((), Some(next))),
                },
                _ => return Ok(((), None)),
            }
        }
    }

    fn parse_term(&mut self, mut token: Token) -> Result<(Term, Option<Token>), ErrorBox> {
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
            match token.data {
                TokenData::Variable(identifier) => {
                    state = match state {
                        State::Empty | State::Plus => State::Linear(1.0, identifier),
                        State::Minus => State::Linear(-1.0, identifier),
                        State::Constant(coeff) => State::Linear(coeff, identifier),
                        State::Linear(_, _) => {
                            let error = parse_error!(self, token, "term is not linear");
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
                        State::Linear(coeff, identifier) => State::Linear(coeff * value, identifier),
                    };
                }

                TokenData::Times => {
                    let next = match self.lex()? {
                        Some(token) => token,
                        None => return parse_error!(self, "expected a number or variable"),
                    };

                    match next.data {
                        TokenData::Variable(identifier) => {
                            state = match state {
                                State::Empty | State::Plus => State::Linear(1.0, identifier),
                                State::Minus => State::Linear(-1.0, identifier),
                                State::Constant(coeff) => State::Linear(coeff, identifier),
                                State::Linear(_, _) => {
                                    return parse_error!(self, next, "term is not linear");
                                }
                            };
                        }
                        TokenData::Decimal(string_value) => {
                            let value = string_value.parse::<f64>().unwrap();
                            state = match state {
                                State::Empty | State::Plus | State::Minus => {
                                    return parse_error!(self, token, "expected a number or variable");
                                }
                                State::Constant(coeff) => State::Constant(coeff * value),
                                State::Linear(coeff, identifier) => State::Linear(coeff * value, identifier),
                            };
                        }
                        _ => {
                            return parse_error!(self, next, "expected a number or variable");
                        }
                    }
                }

                TokenData::DividedBy => {
                    let next = match self.lex()? {
                        Some(token) => token,
                        None => return parse_error!(self, "expected a number"),
                    };

                    match next.data {
                        TokenData::Decimal(string_value) => {
                            let value = string_value.parse::<f64>().unwrap();
                            state = match state {
                                State::Empty | State::Plus | State::Minus => {
                                    return parse_error!(self, token, "expected a number or variable");
                                }
                                State::Constant(coeff) => State::Constant(coeff / value),
                                State::Linear(coeff, identifier) => State::Linear(coeff / value, identifier),
                            };
                        }
                        _ => return parse_error!(self, next, "expected a number"),
                    }
                }

                TokenData::Plus => {
                    state = match state {
                        State::Empty | State::Plus => State::Plus,
                        State::Minus => State::Minus,
                        _ => {
                            last = Some(token);
                            break;
                        }
                    };
                }

                TokenData::Minus => {
                    state = match state {
                        State::Empty | State::Plus => State::Minus,
                        State::Minus => State::Plus,
                        _ => {
                            last = Some(token);
                            break;
                        }
                    };
                }

                TokenData::Equals | TokenData::EndOfLine => {
                    last = Some(token);
                    break;
                }

                TokenData::Whitespace | TokenData::Comment => unreachable!(),
            }

            token = match self.lex()? {
                Some(token) => token,
                None => break,
            }
        }

        match state {
            State::Constant(value) => Ok((Term::Constant(value), last)),
            State::Linear(coeff, identifier) => Ok((Term::Linear(coeff, identifier), last)),
            State::Empty | State::Plus | State::Minus => parse_error!(self, "expected a number or variable"),
        }
    }

    fn lex(&mut self) -> Result<Option<Token>, ErrorBox> {
        loop {
            match self.lexer.lex()? {
                Some(token) => match token.data {
                    TokenData::Whitespace | TokenData::Comment => continue,
                    _ => return Ok(Some(token)),
                },
                None => return Ok(None),
            }
        }
    }
}
