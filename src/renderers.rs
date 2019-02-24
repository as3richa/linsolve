use std::fmt::Write;
use std::io;

use crate::errors::ErrorBox;

pub enum RendererTerm {
    Constant(f64),
    Linear(f64, usize),
}

pub trait Renderer {
    fn set_names(&mut self, names: Vec<String>);
    fn write_str(&mut self, string: &str) -> Result<(), ErrorBox>;
    fn write_inline_name(&mut self, id: usize) -> Result<(), ErrorBox>;
    fn write_system<T: Iterator<Item = RendererTerm>, E: Iterator<Item = T>>(
        &mut self,
        equation_iter: E,
    ) -> Result<(), ErrorBox>;
}

pub struct LatexRenderer<W: io::Write> {
    stream: W,
    names: Vec<String>,
}

impl<W: io::Write> LatexRenderer<W> {
    fn new(stream: W) -> Self {
        Self { stream, names: vec![] }
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

            if subscript.len() > 0 {
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

    fn write_system<T: Iterator<Item = RendererTerm>, E: Iterator<Item = T>>(
        &mut self,
        equation_iter: E,
    ) -> Result<(), ErrorBox> {
        self.write_str("\\begin{align*}\n")?;
        self.write_str("\\end{align*}\n")?;
        Ok(())
    }
}