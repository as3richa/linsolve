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

macro_rules! is_end_of_line {
    ($token:ident) => {{
        match $token {
            Some(Token {
                data: TokenData::EndOfLine,
                ..
            }) => true,
            _ => false,
        }
    }};
}

macro_rules! is_end_of_line_or_file {
    ($token:ident) => {{
        match $token {
            None
            | Some(Token {
                data: TokenData::EndOfLine,
                ..
            }) => true,
            _ => false,
        }
    }};
}

macro_rules! is_equals {
    ($token:ident) => {{
        match $token {
            Some(Token {
                data: TokenData::Equals,
                ..
            }) => true,
            _ => false,
        }
    }};
}

macro_rules! is_plus_or_minus {
    ($token:ident) => {{
        match $token {
            Some(Token {
                data: TokenData::Plus, ..
            }) => true,
            Some(Token {
                data: TokenData::Minus, ..
            }) => true,
            _ => false,
        }
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
            let lhs = match self.parse_lhs()? {
                Some(expr) => expr,
                None => break,
            };
            println!("Right-hand side:");
            let rhs = self.parse_rhs()?;
        }

        Ok(())
    }

    fn parse_lhs(&mut self) -> Result<Option<()>, ErrorBox> {
        let first_token = {
            loop {
                let token = self.lex()?;
                if !is_end_of_line!(token) {
                    break token;
                }
            }
        };

        if is_end_of_line_or_file!(first_token) {
            return Ok(None);
        }

        let (expr, last_token) = self.parse_expr(first_token)?;

        if !is_equals!(last_token) {
            let (line, column) = self.line_and_column(last_token);
            return parse_error!(
                self.lexer.stream.filename,
                line,
                column,
                "expected `=` after left-hand side of equation"
            );
        }

        Ok(Some(expr))
    }

    fn parse_rhs(&mut self) -> Result<(), ErrorBox> {
        let first_token = self.lex()?;

        if is_end_of_line_or_file!(first_token) {
            let (line, column) = self.line_and_column(first_token);
            return parse_error!(self.lexer.stream.filename, line, column, "expected an expr after `=`");
        }

        let (expr, last_token) = self.parse_expr(first_token)?;

        if !is_end_of_line_or_file!(last_token) {
            let (line, column) = self.line_and_column(last_token);
            return parse_error!(
                self.lexer.stream.filename,
                line,
                column,
                "expected end of line or end of file after right-hand side of equation"
            );
        }

        Ok(expr)
    }

    fn parse_expr(&mut self, first_token: Option<Token>) -> Result<((), Option<Token>), ErrorBox> {
        let mut token = first_token;

        loop {
            let (term, next_token) = self.parse_term(token)?;
            token = next_token;

            println!("{:?}", term);

            if is_plus_or_minus!(token) {
                continue;
            }

            break;
        }

        Ok(((), token))
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

        while let Some(unwrapped) = token {
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
                        State::Linear(coeff, identifier) => State::Linear(coeff * value, identifier),
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
                                State::Linear(coeff, identifier) => State::Linear(coeff * value, identifier),
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
                                State::Linear(coeff, identifier) => State::Linear(coeff / value, identifier),
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

    fn line_and_column(&self, token: Option<Token>) -> (u32, u32) {
        match token {
            Some(unwrapped) => (unwrapped.line, unwrapped.column),
            None => (self.lexer.stream.line, self.lexer.stream.column),
        }
    }
}
