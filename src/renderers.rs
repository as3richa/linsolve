// use std::fmt::Write;
use std::io;
use std::io::Write;

use crate::errors::ErrorBox;
use crate::solver::IndexedTerm;

pub trait Renderer: io::Write {
    fn set_names(&mut self, names: Vec<String>);
    fn name(&self, index: usize) -> String;
    fn write_system<L: Iterator<Item = IndexedTerm>, R: Iterator<Item = IndexedTerm>, E: Iterator<Item = (L, R)>>(
        &mut self,
        equation_iter: E,
    ) -> Result<(), ErrorBox>;
}

pub struct LatexRenderer<W: io::Write> {
    stream: W,
    names: Vec<String>,
}

impl<W: io::Write> LatexRenderer<W> {
    pub fn new(stream: W) -> Self {
        Self { stream, names: vec![] }
    }

    fn write_expr<T: Iterator<Item = IndexedTerm>>(&mut self, mut expr_iter: T) -> Result<(), ErrorBox> {
        match expr_iter.next() {
            Some(term) => match term {
                IndexedTerm::Linear(coeff, index) => {
                    if is_zero(coeff - 1.0) {
                        write!(self.stream, "{}", self.names[index])?;
                    } else {
                        write!(self.stream, "{}{}", coeff, self.names[index])?;
                    }
                }
                IndexedTerm::Constant(value) => write!(self.stream, "{}", value)?,
            },
            None => {
                write!(self.stream, "0")?;
                return Ok(());
            }
        }

        for term in expr_iter {
            match term {
                IndexedTerm::Linear(coeff, index) => {
                    write!(self.stream, " {} ", sign(coeff));
                    if is_zero(coeff.abs() - 1.0) {
                        write!(self.stream, "{}", self.names[index])?;
                    } else {
                        write!(self.stream, "{}{}", coeff.abs(), self.names[index])?;
                    }
                }
                IndexedTerm::Constant(value) => write!(self.stream, " {} {}", sign(value), value.abs())?,
            }
        }

        Ok(())
    }
}

impl<W: io::Write> io::Write for LatexRenderer<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.stream.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.stream.flush()
    }
}

impl<W: io::Write> Renderer for LatexRenderer<W> {
    fn set_names(&mut self, names: Vec<String>) {
        let texify_name = |name: String| {
            let (identifier, subscript) = match name.find('_') {
                Some(index) => name.split_at(index),
                None => name.split_at(name.len()),
            };

            assert!(!identifier.is_empty());

            let mut result = {
                if identifier.len() == 1 {
                    identifier.to_string()
                } else {
                    format!("\\text{{{}}}", identifier)
                }
            };

            if !subscript.is_empty() {
                assert!(subscript.len() >= 2);

                if subscript.len() == 2 {
                    result += subscript;
                } else if subscript[1..].chars().all(|c| c.is_digit(10)) {
                    result = format!("{}_{{{}}}", result, subscript);
                } else {
                    result = format!("{}_{{\\text{{{}}}}}", result, subscript);
                }
            }

            result
        };

        self.names = names.into_iter().map(texify_name).collect()
    }

    fn name(&self, index: usize) -> String {
        assert!(index < self.names.len());
        format!("${}$", self.names[index])
    }

    fn write_system<L: Iterator<Item = IndexedTerm>, R: Iterator<Item = IndexedTerm>, E: Iterator<Item = (L, R)>>(
        &mut self,
        equation_iter: E,
    ) -> Result<(), ErrorBox> {
        write!(self.stream, "\\begin{{align*}}\n")?;

        let mut first = true;

        for (lhs_iter, rhs_iter) in equation_iter {
            if !first {
                write!(self.stream, " \\\\\n")?;
            } else {
                first = false;
            }

            write!(self.stream, "  ")?;
            self.write_expr(lhs_iter)?;
            write!(self.stream, " &= ")?;
            self.write_expr(rhs_iter)?;
        }

        write!(self.stream, "\n\\end{{align*}}\n")?;

        Ok(())
    }
}

fn sign(value: f64) -> char {
    if value >= 0.0 {
        '+'
    } else {
        '-'
    }
}

const EPSILON: f64 = 1.0e-9;

fn is_zero(value: f64) -> bool {
    value.abs() < EPSILON
}
