use std::error;
use std::fmt;
use std::io;
use std::rc::Rc;

use crate::stream::{Stream, StreamValue};

pub enum LexResult {
    Ok(Token),
    EndOfFile,
    Err(Rc<dyn error::Error>),
}

#[derive(Debug)]
struct LexError {
    desc: String,
}

impl LexError {
    fn new(filename: &str, line: u32, column: u32, message: &str) -> LexError {
        let mut desc = filename.to_string();
        desc += ":";
        desc += &line.to_string();
        desc += ":";
        desc += &column.to_string();
        desc += ": ";
        desc += message;
        LexError { desc }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{}", &self.desc)
    }
}

impl error::Error for LexError {
    fn description(&self) -> &str {
        &self.desc
    }
}

macro_rules! lex_error {
    ($self:ident, $( $format_params:expr ),+) => {
        {
            let message = format!($($format_params),*);
            let error = LexError::new(&$self.stream.filename, $self.stream.line, $self.stream.column, &message);
            let rc: Rc<error::Error> = Rc::new(error);
            Err(rc)
        }
    };
}

macro_rules! consume {
    ($self:ident, $( $patterns:pat )|+, $matched:ident, $unmatched:ident) => {
        {
            while let StreamValue::Byte(byte) = $self.stream.peek() {
                match byte {
                    $($patterns)|* => $matched!(byte),
                    _ => $unmatched!(byte)
                }
                $self.stream.forward()
            }

            if let StreamValue::Err(error) = $self.stream.peek() {
                return Err(error);
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
            StreamValue::Byte(byte) => {
                match byte {
                    $($patterns)|* => (),
                    _ => return lex_error!($self, $message)
                }
            },
            StreamValue::Err(error) => return Err(error),
            StreamValue::EndOfFile => return lex_error!($self, $message)
        }
    };

    ($self: ident, :alphanumeric, $message:expr) => {
        assert_byte!($self, b'a'...b'z' | b'A'...b'Z' | b'0'...b'9', $message)
    };
}

pub enum TokenData {
    Variable(String),
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

    pub fn lex(&mut self) -> LexResult {
        match self.stream.peek() {
            StreamValue::Byte(byte) => self.lex_something(byte),
            StreamValue::Err(error) => LexResult::Err(error),
            StreamValue::EndOfFile => LexResult::EndOfFile,
        }
    }

    fn lex_something(&mut self, byte: u8) -> LexResult {
        let line = self.stream.line;
        let column = self.stream.column;

        let result = match byte {
            b'a'...b'z' | b'A'...b'Z' => self.lex_variable(),
            b'0'...b'9' => unreachable!(), /* FIXME */
            b'#' => self.lex_comment(),
            b' ' | b'\t' => self.lex_whitespace(),
            b'\r' | b'\n' => self.lex_end_of_line(),

            /* Printable ASCII that isn't a valid leading character for a token */
            b'!' | b'"' | b'$'...b'/' | b':'...b'@' | b'['...b'`' | b'{'...b'~' => {
                lex_error!(self, "unexpected character {}", byte as char)
            }

            /* Unprintable ASCII, or a byte that isn't valid ASCII */
            _ => lex_error!(
                self,
                "unexpected byte {:#x} (Unicode is permitted only in comments)",
                byte
            ),
        };

        match result {
            Ok(data) => LexResult::Ok(Token { data, line, column }),
            Err(error) => LexResult::Err(error),
        }
    }

    fn lex_variable(&mut self) -> Result<TokenData, Rc<dyn error::Error>> {
        /* Variables look like: (i) Strings of letters, e.g. x, alpha, Gamma; (ii) Strings
         * of letters followed by an unbraced alphanumeric subscript, e.g. x_1, alpha_zero
         * Gamma_k1; (iii) Strings of letters followed by a Latex-esque braced alphanumeric
         * subscript, with leading and trailing spaces permitted, e.g. x_{1}, alpha_{ zero },
         * Gamma_{k1} */

        let mut lexeme = String::new();
        munch_while!(self, lexeme, :alpha);

        match self.stream.peek() {
            StreamValue::Byte(b'_') => {
                lexeme.push('_');
                self.stream.forward();
            }
            _ => return Ok(TokenData::Variable(lexeme)), /* Case (i) */
        }

        match self.stream.peek() {
            StreamValue::Byte(byte) => {
                match byte {
                    b'a'...b'z' | b'A'...b'Z' | b'0'...b'9' => {
                        /* Case (ii) */
                        munch_while!(self, lexeme, :alphanumeric);
                    }
                    b'{' => {
                        /* Case (iii) */
                        self.stream.forward();
                        skip_while!(self, :whitespace);

                        assert_byte!(self, :alphanumeric, "expected an braced-alphanumeric subscript for variable");
                        munch_while!(self, lexeme, :alphanumeric);

                        skip_while!(self, :whitespace);

                        assert_byte!(self, b'}', "expected a closing brace");
                        self.stream.forward();
                    }
                    _ => {
                        return lex_error!(self, "expected an alphanumeric or braced-alphanumeric subscript for variable");
                    }
                }

                Ok(TokenData::Variable(lexeme))
            }
            StreamValue::EndOfFile => lex_error!(self, "expected a subscript for variable"),
            StreamValue::Err(error) => Err(error),
        }
    }

    fn lex_comment(&mut self) -> Result<TokenData, Rc<dyn error::Error>> {
        skip_until!(self, b'\r' | b'\n');
        Ok(TokenData::Comment)
    }

    fn lex_whitespace(&mut self) -> Result<TokenData, Rc<dyn error::Error>> {
        skip_while!(self, :whitespace);
        Ok(TokenData::Whitespace)
    }

    fn lex_end_of_line(&mut self) -> Result<TokenData, Rc<dyn error::Error>> {
        skip_while!(self, b'\r' | b'\n');
        Ok(TokenData::EndOfLine)
    }
}
