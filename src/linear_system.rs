use std::mem;

const EPSILON: f64 = 1.0e-9;

#[derive(Debug)]
pub struct LinearSystem {
    identifiers: Vec<String>,
    equations: Vec<(LinearExpression, LinearExpression)>,
}

#[derive(Debug)]
pub struct LinearExpression {
    coeffs: Vec<f64>,
    constant: f64,
}

pub enum Term {
    Constant(f64),
    Linear(f64, String),
}

fn is_zero(value: f64) -> bool {
    value.abs() < EPSILON
}

fn is_eq(x: f64, y: f64) -> bool {
    is_zero(x - y)
}

impl LinearExpression {
    pub fn new() -> LinearExpression {
        Self {
            coeffs: vec![],
            constant: 0.0,
        }
    }

    fn is_constant(&self) -> bool {
        match self.nonzero_coeff() {
            Some(_) => false,
            None => true,
        }
    }

    fn is_zero(&self) -> bool {
        if !is_zero(self.constant) {
            return false;
        }
        self.is_constant()
    }

    fn set_to_zero(&mut self) {
        self.coeffs.resize(0, 0.0);
        self.constant = 0.0;
    }

    fn nonzero_coeff(&self) -> Option<(usize, f64)> {
        for (index, coeff) in self.coeffs.iter().enumerate() {
            if !is_zero(*coeff) {
                return Some((index, *coeff));
            }
        }
        None
    }

    fn scalar_divide(&mut self, scalar: f64) {
        for coeff in self.coeffs.iter_mut() {
            *coeff /= scalar;
        }
        self.constant /= scalar;
    }

    fn subtract(&mut self, other: &LinearExpression) {
        if self.coeffs.len() < other.coeffs.len() {
            self.coeffs.resize(other.coeffs.len(), 0.0);
        }

        for (coeff, other_coeff) in self.coeffs.iter_mut().zip(other.coeffs.iter()) {
            *coeff -= other_coeff;
        }

        self.constant -= other.constant;
    }

    fn eliminate(&mut self, other: &LinearExpression, index: usize) {
        assert!(is_eq(other.coeffs[index], 1.0));

        if index >= self.coeffs.len() || is_zero(self.coeffs[index]) {
            return;
        }

        if self.coeffs.len() < other.coeffs.len() {
            self.coeffs.resize(other.coeffs.len(), 0.0);
        }

        let scalar = self.coeffs[index];

        for (coeff, other_coeff) in self.coeffs.iter_mut().zip(other.coeffs.iter()) {
            *coeff -= other_coeff * scalar;
        }

        self.constant -= other.constant * scalar;

        self.coeffs[index] = 0.0;
    }
}

impl LinearSystem {
    pub fn new() -> LinearSystem {
        Self {
            identifiers: vec![],
            equations: vec![],
        }
    }

    pub fn add_term_to_expr(&mut self, expr: &mut LinearExpression, term: Term) {
        match term {
            Term::Constant(value) => expr.constant += value,
            Term::Linear(coeff, identifier) => {
                let position = self.identifiers.iter().position(|id| *id == identifier);

                let index = match position {
                    Some(index) => index,
                    None => {
                        self.identifiers.push(identifier);
                        self.identifiers.len() - 1
                    }
                };

                if expr.coeffs.len() <= index {
                    expr.coeffs.resize(index + 1, 0.0);
                }

                expr.coeffs[index] += coeff;
            }
        }
    }

    pub fn push_eqn(&mut self, lhs: LinearExpression, rhs: LinearExpression) {
        self.equations.push((lhs, rhs))
    }

    pub fn solve(&mut self) {
        for (lhs, rhs) in self.equations.iter_mut() {
            lhs.subtract(rhs);
            rhs.set_to_zero();
        }

        self.remove_trivial_equations();

        let mut eqn_index = 0usize;

        while eqn_index < self.equations.len() {
            let mut lhs = mem::replace(&mut self.equations[eqn_index].0, LinearExpression::new());

            match lhs.nonzero_coeff() {
                Some((index, value)) => {
                    lhs.scalar_divide(value);

                    for (other_lhs, _) in self.equations.iter_mut() {
                        other_lhs.eliminate(&lhs, index);
                    }

                    self.equations[eqn_index].0 = lhs;
                }
                None => {
                    assert!(!is_zero(lhs.constant));
                    self.equations[eqn_index].0 = lhs;
                    break;
                }
            }

            eqn_index += 1;
            self.remove_trivial_equations();
        }
    }

    fn remove_trivial_equations(&mut self) {
        assert!(self.equations.len() > 0);

        let mut index = 0usize;

        while index < self.equations.len() {
            assert!(self.equations[index].1.is_zero());

            if self.equations[index].0.is_zero() {
                if self.equations.len() == 1 {
                    break;
                }
                self.equations.remove(index);
                continue;
            }

            index += 1;
        }
    }
}
