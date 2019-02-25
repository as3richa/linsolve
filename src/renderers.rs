use std::fmt::Write;
use std::io;

use crate::errors::ErrorBox;
use crate::linear_system::IndexedTerm;

pub trait Renderer {
    fn set_names(&mut self, names: Vec<String>);
    fn write_str(&mut self, string: &str) -> Result<(), ErrorBox>;
    fn write_inline_name(&mut self, id: usize) -> Result<(), ErrorBox>;
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
                IndexedTerm::Linear(coeff, index) => write!(self.stream, "{}{}", coeff, self.names[index])?,
                IndexedTerm::Constant(value) => write!(self.stream, "{}", value)?,
            },
            None => {
                self.write_str("0")?;
                return Ok(());
            }
        }

        for term in expr_iter {
            match term {
                IndexedTerm::Linear(coeff, index) => {
                    write!(self.stream, " {} {}{}", sign(coeff), coeff.abs(), self.names[index])?;
                }
                IndexedTerm::Constant(value) => {
                    write!(self.stream, " {} {}", sign(value), value.abs())?;
                }
            }
        }
        Ok(())
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
                    write!(result, "_{{{}}}", subscript).unwrap();
                } else {
                    write!(result, "_{{\\text{{{}}}}}", subscript).unwrap();
                }
            }

            result
        };

        self.names = names.into_iter().map(texify_name).collect()
    }

    fn write_str(&mut self, string: &str) -> Result<(), ErrorBox> {
        self.stream.write_all(string.as_bytes())?;
        Ok(())
    }

    fn write_inline_name(&mut self, id: usize) -> Result<(), ErrorBox> {
        assert!(id < self.names.len());
        write!(self.stream, "${}$", self.names[id])?;
        Ok(())
    }

    fn write_system<L: Iterator<Item = IndexedTerm>, R: Iterator<Item = IndexedTerm>, E: Iterator<Item = (L, R)>>(
        &mut self,
        equation_iter: E,
    ) -> Result<(), ErrorBox> {
        self.write_str("\\begin{align*}\n")?;

        let mut first = true;

        for (lhs_iter, rhs_iter) in equation_iter {
            if !first {
                self.write_str(" \\\\\n")?;
            } else {
                first = false;
            }

            self.write_str("  ")?;
            self.write_expr(lhs_iter)?;
            self.write_str(" &= ")?;
            self.write_expr(rhs_iter)?;
        }

        self.write_str("\n\\end{align*}\n")?;

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
