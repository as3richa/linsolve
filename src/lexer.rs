use std::io::Read;

use crate::errors::{ErrorBox, LexError};
use crate::stream::Stream;

pub struct Token {
    pub data: TokenData,
    pub line: u32,
    pub column: u32,
}

pub enum TokenData {
    Variable(String),
    Decimal(String),
    Plus,
    Minus,
    Times,
    DividedBy,
    Equals,
    Whitespace,
    Comment,
    EndOfLine,
}

macro_rules! lex_error {
    ($stream:ident, $( $format_params:expr ),+) => {
        {
            let message = format!($($format_params),*);
            let error = LexError::new(&$stream.filename, $stream.line, $stream.column, &message);
            Err(ErrorBox::from_lex_error(error))
        }
    };
}

macro_rules! peek {
    ($stream:ident) => {{
        match $stream.peek() {
            Ok(value) => value,
            Err(error) => return Err(ErrorBox::from_io_error(error)),
        }
    }};
}

macro_rules! consume {
    ($stream:ident, $( $patterns:pat )|+, $matched:ident, $unmatched:ident) => {
        while let Some(byte) = peek!($stream) {
            match byte {
                $($patterns)|* => $matched!(byte),
                _ => $unmatched!(byte)
            }
            $stream.forward();
        }
    };
}

macro_rules! munch_while {
    ($stream:ident, $lexeme:ident, $( $patterns:pat )|+) => {
        macro_rules! matched {
            ($byte:ident) => { $lexeme.push($byte as char); }
        }
        macro_rules! unmatched {
            ($byte:ident) => { break; }
        }
        consume!($stream, $($patterns)|*, matched, unmatched)
    };

    ($stream:ident, $lexeme:ident, :alpha) => {
        munch_while!($stream, $lexeme, b'a'...b'z' | b'A'...b'Z')
    };

    ($stream:ident, $lexeme:ident, :alphanumeric) => {
        munch_while!($stream, $lexeme, b'a'...b'z' | b'A'...b'Z' | b'0'...b'9')
    };

    ($stream:ident, $lexeme:ident, :numeric) => {
        munch_while!($stream, $lexeme, b'0'...b'9')
    };
}

macro_rules! skip_while {
    ($stream:ident, $( $patterns:pat )|+) => {
        macro_rules! matched {
            ($byte:ident) => { (); }
        }
        macro_rules! unmatched {
            ($byte:ident) => { break; }
        }
        consume!($stream, $($patterns)|*, matched, unmatched)
    };

    ($stream:ident, :whitespace) => {
        skip_while!($stream, b' ' | b'\t')
    };
}

macro_rules! skip_until {
    ($stream:ident, $( $patterns:pat )|+) => {
        macro_rules! matched {
            ($byte:ident) => { break; }
        }
        macro_rules! unmatched {
            ($byte:ident) => { (); }
        }
        consume!($stream, $($patterns)|*, matched, unmatched)
    };
}

macro_rules! assert_byte {
    ($stream:ident, $( $patterns:pat )|+, $message:expr) => {
        match peek!($stream) {
            Some(byte) => {
                match byte {
                    $($patterns)|* => (),
                    _ => return lex_error!($stream, $message)
                }
            },
            None => return lex_error!($stream, $message),
        }
    };

    ($stream: ident, :alphanumeric, $message:expr) => {
        assert_byte!($stream, b'a'...b'z' | b'A'...b'Z' | b'0'...b'9', $message)
    };

    ($stream: ident, :numeric, $message:expr) => {
        assert_byte!($stream, b'0'...b'9', $message)
    };
}

pub fn lex<R: Read>(stream: &mut Stream<R>) -> Result<Option<Token>, ErrorBox> {
    match peek!(stream) {
        Some(byte) => lex_something(stream, byte),
        None => Ok(None),
    }
}

fn lex_something<R: Read>(stream: &mut Stream<R>, byte: u8) -> Result<Option<Token>, ErrorBox> {
    let line = stream.line;
    let column = stream.column;

    let result = match byte {
        b'a'...b'z' | b'A'...b'Z' => lex_variable(stream),
        b'.' | b'0'...b'9' => lex_decimal(stream),
        b'#' => lex_comment(stream),
        b' ' | b'\t' => lex_whitespace(stream),
        b'\r' | b'\n' => lex_end_of_line(stream),

        b'+' | b'-' | b'*' | b'/' | b'=' => {
            stream.forward();
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
        b'!' | b'"' | b'$'...b')' | b',' | b':'...b'<' | b'>'...b'@' | b'['...b'`' | b'{'...b'~' => {
            lex_error!(stream, "unexpected character {}", byte as char)
        }

        /* Unprintable ASCII, or a byte that isn't valid ASCII */
        _ => lex_error!(
            stream,
            "unexpected byte {:#x} (Unicode is permitted only in comments)",
            byte
        ),
    };

    match result {
        Ok(data) => Ok(Some(Token { data, line, column })),
        Err(error) => Err(error),
    }
}

fn lex_variable<R: Read>(stream: &mut Stream<R>) -> Result<TokenData, ErrorBox> {
    /* Variables look like: (i) Strings of letters, e.g. x, alpha, Gamma; (ii) Strings
     * of letters followed by an unbraced alphanumeric subscript, e.g. x_1, alpha_zero
     * Gamma_k1; (iii) Strings of letters followed by a Latex-esque braced alphanumeric
     * subscript, with leading and trailing spaces permitted, e.g. x_{1}, alpha_{ zero },
     * Gamma_{k1} */

    let mut identifier = String::new();
    munch_while!(stream, identifier, :alpha);

    match peek!(stream) {
        Some(b'_') => {
            identifier.push('_');
            stream.forward();
        }
        _ => return Ok(TokenData::Variable(identifier)), /* Case (i) */
    }

    match peek!(stream) {
        Some(byte) => {
            match byte {
                b'a'...b'z' | b'A'...b'Z' | b'0'...b'9' => {
                    /* Case (ii) */
                    munch_while!(stream, identifier, :alphanumeric);
                }
                b'{' => {
                    /* Case (iii) */
                    stream.forward();
                    skip_while!(stream, :whitespace);

                    assert_byte!(stream, :alphanumeric, "expected an braced-alphanumeric subscript for variable");
                    munch_while!(stream, identifier, :alphanumeric);

                    skip_while!(stream, :whitespace);

                    assert_byte!(stream, b'}', "expected a closing brace");
                    stream.forward();
                }
                _ => {
                    return lex_error!(
                        stream,
                        "expected an alphanumeric or braced-alphanumeric subscript for variable"
                    );
                }
            }

            Ok(TokenData::Variable(identifier))
        }
        None => lex_error!(stream, "expected a subscript for variable"),
    }
}

fn lex_decimal<R: Read>(stream: &mut Stream<R>) -> Result<TokenData, ErrorBox> {
    let mut value = String::new();
    munch_while!(stream, value, :numeric);

    match peek!(stream) {
        Some(byte) => {
            if byte == b'.' {
                value.push('.');
                stream.forward();
                assert_byte!(stream, :numeric, "expected fractional part of value to be non-empty");
                munch_while!(stream, value, :numeric);
            }
        }
        None => return Ok(TokenData::Decimal(value)),
    }

    match peek!(stream) {
        Some(byte) => {
            if byte == b'e' || byte == b'E' {
                value.push('e');
                stream.forward();
                if let Some(byte) = peek!(stream) {
                    match byte {
                        b'-' => {
                            value.push('-');
                            stream.forward();
                        }
                        b'+' => {
                            stream.forward();
                        }
                        _ => (),
                    }
                }
                assert_byte!(stream, :numeric, "expected an optional sign followed by an integer in exponential part of value");
                munch_while!(stream, value, :numeric);
            }
            Ok(TokenData::Decimal(value))
        }
        None => Ok(TokenData::Decimal(value)),
    }
}

fn lex_comment<R: Read>(stream: &mut Stream<R>) -> Result<TokenData, ErrorBox> {
    skip_until!(stream, b'\r' | b'\n');
    Ok(TokenData::Comment)
}

fn lex_whitespace<R: Read>(stream: &mut Stream<R>) -> Result<TokenData, ErrorBox> {
    skip_while!(stream, :whitespace);
    Ok(TokenData::Whitespace)
}

fn lex_end_of_line<R: Read>(stream: &mut Stream<R>) -> Result<TokenData, ErrorBox> {
    skip_while!(stream, b'\r' | b'\n');
    Ok(TokenData::EndOfLine)
}
