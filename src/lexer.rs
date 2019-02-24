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
    EndOfLine,
}

macro_rules! lex_error {
    ($stream:ident, $( $format_params:expr ),+) => {{
        let message = format!($($format_params),*);
        let error = LexError::new(&$stream.filename, $stream.line, $stream.column, &message);
        Err(ErrorBox::from_lex_error(error))
    }};
}

pub fn lex<R: Read>(stream: &mut Stream<R>) -> Result<Option<Token>, ErrorBox> {
    while let Some(byte) = stream.peek()? {
        let line = stream.line;
        let column = stream.column;

        let data = match byte {
            b'a'...b'z' | b'A'...b'Z' => lex_variable(stream)?,

            b'0'...b'9' => lex_decimal(stream)?,

            b'#' => {
                loop {
                    stream.forward();
                    match stream.peek()? {
                        Some(b'\r') | Some(b'\n') => break,
                        _ => continue,
                    }
                }
                continue;
            }

            b' ' | b'\t' => {
                skip_whitespace(stream)?;
                continue;
            }

            b'\r' | b'\n' => {
                loop {
                    stream.forward();
                    match stream.peek()? {
                        Some(b'\r') | Some(b'\n') => continue,
                        _ => break,
                    }
                }
                TokenData::EndOfLine
            }

            b'+' => {
                stream.forward();
                TokenData::Plus
            }

            b'-' => {
                stream.forward();
                TokenData::Minus
            }

            b'*' => {
                stream.forward();
                TokenData::Times
            }

            b'/' => {
                stream.forward();
                TokenData::DividedBy
            }

            b'=' => {
                stream.forward();
                TokenData::Equals
            }

            /* Printable ASCII that isn't a valid leading character for a token */
            b'!' | b'"' | b'$'...b')' | b',' | b':'...b'<' | b'>'...b'@' | b'['...b'`' | b'{'...b'~' => {
                return lex_error!(stream, "unexpected character {}", byte as char);
            }

            /* Unprintable ASCII, or a byte that isn't valid ASCII */
            _ => {
                return lex_error!(
                    stream,
                    "unexpected byte {:#x} (Unicode is permitted only in comments)",
                    byte
                );
            }
        };

        return Ok(Some(Token { data, line, column }));
    }

    Ok(None)
}

fn lex_variable<R: Read>(stream: &mut Stream<R>) -> Result<TokenData, ErrorBox> {
    /* Variables look like: (i) Strings of letters, e.g. x, alpha, Gamma; (ii) Strings
     * of letters followed by an unbraced alphanumeric subscript, e.g. x_1, alpha_zero
     * Gamma_k1; (iii) Strings of letters followed by a Latex-esque braced alphanumeric
     * subscript, with leading and trailing spaces permitted, e.g. x_{1}, alpha_{ zero },
     * Gamma_{k1} */

    let mut name = String::new();
    munch_alphabetical(&mut name, stream)?;

    match stream.peek()? {
        Some(b'_') => {
            name.push('_');
            stream.forward();
        }
        _ => return Ok(TokenData::Variable(name)), /* Case (i) */
    }

    match stream.peek()? {
        Some(byte) => {
            match byte {
                b'a'...b'z' | b'A'...b'Z' | b'0'...b'9' => {
                    /* Case (ii) */
                    munch_alphanumeric(&mut name, stream)?;
                }

                b'{' => {
                    /* Case (iii) */
                    stream.forward();
                    skip_whitespace(stream)?;

                    if !munch_alphanumeric(&mut name, stream)? {
                        return lex_error!(stream, "expected a braced alphanumeric subscript for variable");
                    }

                    skip_whitespace(stream)?;

                    match stream.peek()? {
                        Some(b'}') => stream.forward(),
                        _ => return lex_error!(stream, "expected a closing brace"),
                    }
                }

                _ => {
                    return lex_error!(
                        stream,
                        "expected an alphanumeric or braced-alphanumeric subscript for variable"
                    );
                }
            }

            Ok(TokenData::Variable(name))
        }
        None => lex_error!(stream, "expected a subscript for variable"),
    }
}

fn lex_decimal<R: Read>(stream: &mut Stream<R>) -> Result<TokenData, ErrorBox> {
    let mut value = String::new();
    munch_numeric(&mut value, stream)?;

    match stream.peek()? {
        Some(byte) => {
            if byte == b'.' {
                value.push('.');
                stream.forward();
                if !munch_numeric(&mut value, stream)? {
                    return lex_error!(stream, "expected fractional part of value to be non-empty");
                }
            }
        }
        None => return Ok(TokenData::Decimal(value)),
    }

    match stream.peek()? {
        Some(byte) => {
            if byte == b'e' || byte == b'E' {
                value.push('e');
                stream.forward();
                match stream.peek()? {
                    Some(b'-') => {
                        value.push('-');
                        stream.forward();
                    }
                    Some(b'+') => stream.forward(),
                    _ => (),
                }
                if !munch_numeric(&mut value, stream)? {
                    return lex_error!(
                        stream,
                        "expected an optional sign followed by an integer in exponential part of value"
                    );
                }
            }
            Ok(TokenData::Decimal(value))
        }
        None => Ok(TokenData::Decimal(value)),
    }
}

fn skip_whitespace<R: Read>(stream: &mut Stream<R>) -> Result<(), ErrorBox> {
    loop {
        match stream.peek()? {
            Some(b' ') | Some(b'\t') => {
                stream.forward();
                continue;
            }
            _ => {
                return Ok(());
            }
        }
    }
}

fn munch_alphabetical<R: Read>(buffer: &mut String, stream: &mut Stream<R>) -> Result<bool, ErrorBox> {
    let mut munched = false;

    while let Some(byte) = stream.peek()? {
        match byte {
            b'a'...b'z' | b'A'...b'Z' => {
                munched = true;
                buffer.push(byte as char);
                stream.forward();
            }
            _ => break,
        }
    }

    Ok(munched)
}

fn munch_alphanumeric<R: Read>(buffer: &mut String, stream: &mut Stream<R>) -> Result<bool, ErrorBox> {
    let mut munched = false;

    while let Some(byte) = stream.peek()? {
        match byte {
            b'a'...b'z' | b'A'...b'Z' | b'0'...b'9' => {
                munched = true;
                buffer.push(byte as char);
                stream.forward();
            }
            _ => break,
        }
    }

    Ok(munched)
}

fn munch_numeric<R: Read>(buffer: &mut String, stream: &mut Stream<R>) -> Result<bool, ErrorBox> {
    let mut munched = false;

    while let Some(byte) = stream.peek()? {
        match byte {
            b'0'...b'9' => {
                munched = true;
                buffer.push(byte as char);
                stream.forward();
            }
            _ => break,
        }
    }

    Ok(munched)
}
