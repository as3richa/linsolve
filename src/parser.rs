use std::io::Read;

use crate::errors::{ErrorBox, ParseError};
use crate::lexer::{lex, Token, TokenData};
use crate::linear_system::{LinearExpression, LinearSystem};
use crate::stream::Stream;

pub struct Parser<R: Read> {
    stream: Stream<R>,
}

#[derive(Debug)]
enum Term {
    Constant(f64),
    Linear(f64, String),
}

macro_rules! parse_error {
    ($self:ident, $token:ident, $message:expr) => {{
        let error = ParseError::new(&$self.stream.filename, $token.line, $token.column, &$message);
        Err(ErrorBox::from_parse_error(error))
    }};

    ($self:ident, $message:expr) => {{
        let error = ParseError::new(
            &$self.stream.filename,
            $self.stream.line,
            $self.stream.column,
            &$message,
        );
        Err(ErrorBox::from_parse_error(error))
    }};
}

impl<R: Read> Parser<R> {
    pub fn new(stream: Stream<R>) -> Parser<R> {
        Self { stream }
    }

    pub fn parse(&mut self) -> Result<LinearSystem, ErrorBox> {
        let mut system = LinearSystem::new();

        while let Some(lhs) = self.parse_lhs(&mut system)? {
            let rhs = self.parse_rhs(&mut system)?;
            system.push_eqn(lhs, rhs);
        }

        Ok(system)
    }

    fn parse_lhs(&mut self, system: &mut LinearSystem) -> Result<Option<LinearExpression>, ErrorBox> {
        let first = {
            loop {
                match lex(&mut self.stream)? {
                    Some(token) => match token.data {
                        TokenData::EndOfLine => continue,
                        _ => break token,
                    },
                    None => return Ok(None),
                }
            }
        };

        let (expr, last) = self.parse_expr(system, first)?;

        match last {
            Some(last) => match last.data {
                TokenData::Equals => Ok(Some(expr)),
                _ => parse_error!(self, last, "expected `=` after left-hand side of equation"),
            },
            None => parse_error!(self, "expected `=` after left-hand side of equation"),
        }
    }

    fn parse_rhs(&mut self, system: &mut LinearSystem) -> Result<LinearExpression, ErrorBox> {
        let first = {
            match lex(&mut self.stream)? {
                Some(token) => match token.data {
                    TokenData::EndOfLine => return parse_error!(self, token, "expected an expression after `=`"),
                    _ => token,
                },
                None => return parse_error!(self, "expected an expression after `=`"),
            }
        };

        let (expr, last) = self.parse_expr(system, first)?;

        match last {
            Some(last) => match last.data {
                TokenData::EndOfLine => Ok(expr),
                _ => parse_error!(self, last, "expected end of file or end of line after equation"),
            },
            None => Ok(expr),
        }
    }

    fn parse_expr(
        &mut self,
        system: &mut LinearSystem,
        first: Token,
    ) -> Result<(LinearExpression, Option<Token>), ErrorBox> {
        let mut token = first;
        let mut expr = system.new_expr();

        loop {
            let (term, next) = self.parse_term(token)?;

            match term {
                Term::Constant(value) => system.add_constant_to_expr(&mut expr, value),
                Term::Linear(coeff, identifier) => system.add_linear_term_to_expr(&mut expr, coeff, identifier),
            }

            match next {
                Some(next) => match next.data {
                    TokenData::Plus | TokenData::Minus => {
                        token = next;
                        continue;
                    }
                    _ => return Ok((expr, Some(next))),
                },
                _ => return Ok((expr, None)),
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
                    let next = match lex(&mut self.stream)? {
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
                    let next = match lex(&mut self.stream)? {
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
            }

            token = match lex(&mut self.stream)? {
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
}
