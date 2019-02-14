use std::io;

use crate::errors::{ErrorBox, LexError};
use crate::stream::Stream;

macro_rules! lex_error {
    ($self:ident, $( $format_params:expr ),+) => {
        {
            let message = format!($($format_params),*);
            let error = LexError::new(&$self.stream.filename, $self.stream.line, $self.stream.column, &message);
            Err(ErrorBox::from_lex_error(error))
        }
    };
}

macro_rules! consume {
    ($self:ident, $( $patterns:pat )|+, $matched:ident, $unmatched:ident) => {
        loop {
            match $self.stream.peek() {
                Ok(Some(byte)) => {
                    match byte {
                        $($patterns)|* => $matched!(byte),
                        _ => $unmatched!(byte)
                    }
                    $self.stream.forward()
                },
                Ok(None) => break,
                Err(error) => return Err(ErrorBox::from_io_error(error)),
            }
        }
    };
}

macro_rules! munch_while {
    ($self:ident, $lexeme:ident, $( $patterns:pat )|+) => {
        macro_rules! matched {
            ($byte:ident) => { $lexeme.push($byte as char); }
        }
        macro_rules! unmatched {
            ($byte:ident) => { break; }
        }
        consume!($self, $($patterns)|*, matched, unmatched)
    };

    ($self:ident, $lexeme:ident, :alpha) => {
        munch_while!($self, $lexeme, b'a'...b'z' | b'A'...b'Z')
    };

    ($self:ident, $lexeme:ident, :alphanumeric) => {
        munch_while!($self, $lexeme, b'a'...b'z' | b'A'...b'Z' | b'0'...b'9')
    };

    ($self:ident, $lexeme:ident, :numeric) => {
        munch_while!($self, $lexeme, b'0'...b'9')
    };
}

macro_rules! skip_while {
    ($self:ident, $( $patterns:pat )|+) => {
        macro_rules! matched {
            ($byte:ident) => { (); }
        }
        macro_rules! unmatched {
            ($byte:ident) => { break; }
        }
        consume!($self, $($patterns)|*, matched, unmatched)
    };

    ($self:ident, :whitespace) => {
        skip_while!($self, b' ' | b'\t')
    };
}

macro_rules! skip_until {
    ($self:ident, $( $patterns:pat )|+) => {
        macro_rules! matched {
            ($byte:ident) => { break; }
        }
        macro_rules! unmatched {
            ($byte:ident) => { (); }
        }
        consume!($self, $($patterns)|*, matched, unmatched)
    };
}

macro_rules! assert_byte {
    ($self: ident, $( $patterns:pat )|+, $message:expr) => {
        match $self.stream.peek() {
            Ok(Some(byte)) => {
                match byte {
                    $($patterns)|* => (),
                    _ => return lex_error!($self, $message)
                }
            },
            Ok(None) => return lex_error!($self, $message),
            Err(error) => return Err(ErrorBox::from_io_error(error)),
        }
    };

    ($self: ident, :alphanumeric, $message:expr) => {
        assert_byte!($self, b'a'...b'z' | b'A'...b'Z' | b'0'...b'9', $message)
    };

    ($self: ident, :numeric, $message:expr) => {
        assert_byte!($self, b'0'...b'9', $message)
    };
}

pub enum TokenData {
    Variable { identifier: String },
    Decimal { value: String },
    Plus,
    Minus,
    Times,
    DividedBy,
    Equals,
    Whitespace,
    Comment,
    EndOfLine,
}

pub struct Token {
    pub data: TokenData,
    pub line: u32,
    pub column: u32,
}

pub struct Lexer<I: Iterator<Item = Result<u8, io::Error>>> {
    stream: Stream<I>,
}

impl<I: Iterator<Item = Result<u8, io::Error>>> Lexer<I> {
    pub fn new(stream: Stream<I>) -> Lexer<I> {
        Self { stream }
    }

    pub fn lex(&mut self) -> Result<Option<Token>, ErrorBox> {
        match self.stream.peek() {
            Ok(Some(byte)) => self.lex_something(byte),
            Ok(None) => Ok(None),
            Err(error) => Err(ErrorBox::from_io_error(error)),
        }
    }

    fn lex_something(&mut self, byte: u8) -> Result<Option<Token>, ErrorBox> {
        let line = self.stream.line;
        let column = self.stream.column;

        let result = match byte {
            b'a'...b'z' | b'A'...b'Z' => self.lex_variable(),
            b'.' | b'0'...b'9' => self.lex_decimal(),
            b'#' => self.lex_comment(),
            b' ' | b'\t' => self.lex_whitespace(),
            b'\r' | b'\n' => self.lex_end_of_line(),

            b'+' | b'-' | b'*' | b'/' | b'=' => {
                self.stream.forward();
                match byte {
                    b'+' => Ok(TokenData::Plus),
                    b'-' => Ok(TokenData::Minus),
                    b'*' => Ok(TokenData::Times),
                    b'/' => Ok(TokenData::DividedBy),
                    b'=' => Ok(TokenData::Equals),
                    _ => unreachable!(),
                }
            }

            /* Printable ASCII that isn't a valid leading character for a token */
            b'!'
            | b'"'
            | b'$'...b')'
            | b','
            | b':'...b'<'
            | b'>'...b'@'
            | b'['...b'`'
            | b'{'...b'~' => lex_error!(self, "unexpected character {}", byte as char),

            /* Unprintable ASCII, or a byte that isn't valid ASCII */
            _ => lex_error!(
                self,
                "unexpected byte {:#x} (Unicode is permitted only in comments)",
                byte
            ),
        };

        match result {
            Ok(data) => Ok(Some(Token { data, line, column })),
            Err(error) => Err(error),
        }
    }

    fn lex_variable(&mut self) -> Result<TokenData, ErrorBox> {
        /* Variables look like: (i) Strings of letters, e.g. x, alpha, Gamma; (ii) Strings
         * of letters followed by an unbraced alphanumeric subscript, e.g. x_1, alpha_zero
         * Gamma_k1; (iii) Strings of letters followed by a Latex-esque braced alphanumeric
         * subscript, with leading and trailing spaces permitted, e.g. x_{1}, alpha_{ zero },
         * Gamma_{k1} */

        let mut identifier = String::new();
        munch_while!(self, identifier, :alpha);

        match self.stream.peek() {
            Ok(Some(b'_')) => {
                identifier.push('_');
                self.stream.forward();
            }
            _ => return Ok(TokenData::Variable { identifier }), /* Case (i) */
        }

        match self.stream.peek() {
            Ok(Some(byte)) => {
                match byte {
                    b'a'...b'z' | b'A'...b'Z' | b'0'...b'9' => {
                        /* Case (ii) */
                        munch_while!(self, identifier, :alphanumeric);
                    }
                    b'{' => {
                        /* Case (iii) */
                        self.stream.forward();
                        skip_while!(self, :whitespace);

                        assert_byte!(self, :alphanumeric, "expected an braced-alphanumeric subscript for variable");
                        munch_while!(self, identifier, :alphanumeric);

                        skip_while!(self, :whitespace);

                        assert_byte!(self, b'}', "expected a closing brace");
                        self.stream.forward();
                    }
                    _ => {
                        return lex_error!(self, "expected an alphanumeric or braced-alphanumeric subscript for variable");
                    }
                }

                Ok(TokenData::Variable { identifier })
            }
            Ok(None) => lex_error!(self, "expected a subscript for variable"),
            Err(error) => Err(ErrorBox::from_io_error(error)),
        }
    }

    fn lex_decimal(&mut self) -> Result<TokenData, ErrorBox> {
        let mut value = String::new();
        munch_while!(self, value, :numeric);

        match self.stream.peek() {
            Ok(Some(byte)) => {
                if byte == b'.' {
                    value.push('.');
                    self.stream.forward();
                    assert_byte!(self, :numeric, "expected fractional part of value to be non-empty");
                    munch_while!(self, value, :numeric);
                }
            }
            Ok(None) => return Ok(TokenData::Decimal { value }),
            Err(error) => return Err(ErrorBox::from_io_error(error)),
        }

        match self.stream.peek() {
            Ok(Some(byte)) => {
                if byte == b'e' || byte == b'E' {
                    value.push('e');
                    self.stream.forward();
                    if let Ok(Some(byte)) = self.stream.peek() {
                        match byte {
                            b'-' => {
                                value.push('-');
                                self.stream.forward();
                            }
                            b'+' => {
                                self.stream.forward();
                            }
                            _ => (),
                        }
                    }
                    assert_byte!(self, :numeric, "expected an optional sign followed by an integer in exponential part of decimal");
                    munch_while!(self, value, :numeric);
                }
                Ok(TokenData::Decimal { value })
            }
            Ok(None) => Ok(TokenData::Decimal { value }),
            Err(error) => Err(ErrorBox::from_io_error(error)),
        }
    }

    fn lex_comment(&mut self) -> Result<TokenData, ErrorBox> {
        skip_until!(self, b'\r' | b'\n');
        Ok(TokenData::Comment)
    }

    fn lex_whitespace(&mut self) -> Result<TokenData, ErrorBox> {
        skip_while!(self, :whitespace);
        Ok(TokenData::Whitespace)
    }

    fn lex_end_of_line(&mut self) -> Result<TokenData, ErrorBox> {
        skip_while!(self, b'\r' | b'\n');
        Ok(TokenData::EndOfLine)
    }
}
