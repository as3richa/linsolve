use std::io::Read;

use crate::errors::{ErrorBox, ParseError};
use crate::lexer::{lex, Token, TokenData};
use crate::solver::{InputExpression, InputSystem, InputTerm};
use crate::stream::Stream;

macro_rules! parse_error {
    ($stream:ident, $token:ident, $message:expr) => {{
        let error = ParseError::new(&$stream.filename, $token.line, $token.column, &$message);
        Err(ErrorBox::from(error))
    }};

    ($stream:ident, $message:expr) => {{
        let error = ParseError::new(&$stream.filename, $stream.line, $stream.column, &$message);
        Err(ErrorBox::from(error))
    }};
}

pub fn parse<R: Read>(stream: &mut Stream<R>) -> Result<InputSystem, ErrorBox> {
    let mut system = InputSystem::new();

    while let Some(lhs) = parse_lhs(stream)? {
        let rhs = parse_rhs(stream)?;
        system.push_equation(lhs, rhs);
    }

    Ok(system)
}

fn parse_lhs<R: Read>(stream: &mut Stream<R>) -> Result<Option<InputExpression>, ErrorBox> {
    let first = {
        loop {
            match lex(stream)? {
                Some(token) => match token.data {
                    TokenData::EndOfLine => continue,
                    _ => break token,
                },
                None => return Ok(None),
            }
        }
    };

    let (expr, last) = parse_expr(stream, first)?;

    match last {
        Some(last) => match last.data {
            TokenData::Equals => Ok(Some(expr)),
            _ => parse_error!(stream, last, "expected `=` after left-hand side of equation"),
        },
        None => parse_error!(stream, "expected `=` after left-hand side of equation"),
    }
}

fn parse_rhs<R: Read>(stream: &mut Stream<R>) -> Result<InputExpression, ErrorBox> {
    let first = {
        match lex(stream)? {
            Some(token) => match token.data {
                TokenData::EndOfLine => return parse_error!(stream, token, "expected an expression after `=`"),
                _ => token,
            },
            None => return parse_error!(stream, "expected an expression after `=`"),
        }
    };

    let (expr, last) = parse_expr(stream, first)?;

    match last {
        Some(last) => match last.data {
            TokenData::EndOfLine => Ok(expr),
            _ => parse_error!(stream, last, "expected end of file or end of line after equation"),
        },
        None => Ok(expr),
    }
}

fn parse_expr<R: Read>(stream: &mut Stream<R>, first: Token) -> Result<(InputExpression, Option<Token>), ErrorBox> {
    let mut token = first;
    let mut expr = InputExpression::new();

    loop {
        let (term, next) = parse_term(stream, token)?;
        expr += term;

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

fn parse_term<R: Read>(stream: &mut Stream<R>, mut token: Token) -> Result<(InputTerm, Option<Token>), ErrorBox> {
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
            TokenData::Variable(name) => {
                state = match state {
                    State::Empty | State::Plus => State::Linear(1.0, name),
                    State::Minus => State::Linear(-1.0, name),
                    State::Constant(coeff) => State::Linear(coeff, name),
                    State::Linear(_, _) => {
                        let error = parse_error!(stream, token, "term is not linear");
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
                    State::Linear(coeff, name) => State::Linear(coeff * value, name),
                };
            }

            TokenData::Times => {
                let next = match lex(stream)? {
                    Some(token) => token,
                    None => return parse_error!(stream, "expected a number or variable"),
                };

                match next.data {
                    TokenData::Variable(name) => {
                        state = match state {
                            State::Empty | State::Plus => State::Linear(1.0, name),
                            State::Minus => State::Linear(-1.0, name),
                            State::Constant(coeff) => State::Linear(coeff, name),
                            State::Linear(_, _) => {
                                return parse_error!(stream, next, "term is not linear");
                            }
                        };
                    }
                    TokenData::Decimal(string_value) => {
                        let value = string_value.parse::<f64>().unwrap();
                        state = match state {
                            State::Empty | State::Plus | State::Minus => {
                                return parse_error!(stream, token, "expected a number or variable");
                            }
                            State::Constant(coeff) => State::Constant(coeff * value),
                            State::Linear(coeff, name) => State::Linear(coeff * value, name),
                        };
                    }
                    _ => {
                        return parse_error!(stream, next, "expected a number or variable");
                    }
                }
            }

            TokenData::DividedBy => {
                let next = match lex(stream)? {
                    Some(token) => token,
                    None => return parse_error!(stream, "expected a number"),
                };

                match next.data {
                    TokenData::Decimal(string_value) => {
                        let value = string_value.parse::<f64>().unwrap();
                        state = match state {
                            State::Empty | State::Plus | State::Minus => {
                                return parse_error!(stream, token, "expected a number or variable");
                            }
                            State::Constant(coeff) => State::Constant(coeff / value),
                            State::Linear(coeff, name) => State::Linear(coeff / value, name),
                        };
                    }
                    _ => return parse_error!(stream, next, "expected a number"),
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

        token = match lex(stream)? {
            Some(token) => token,
            None => break,
        }
    }

    match state {
        State::Constant(value) => Ok((InputTerm::Constant(value), last)),
        State::Linear(coeff, name) => Ok((InputTerm::Linear(coeff, name), last)),
        State::Empty | State::Plus | State::Minus => parse_error!(stream, "expected a number or variable"),
    }
}
