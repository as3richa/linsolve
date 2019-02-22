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

impl LinearSystem {
    pub fn new() -> LinearSystem {
        Self {
            identifiers: vec![],
            equations: vec![],
        }
    }

    pub fn new_expr(&self) -> LinearExpression {
        LinearExpression {
            coeffs: vec![],
            constant: 0.0,
        }
    }

    pub fn add_constant_to_expr(&self, expr: &mut LinearExpression, value: f64) {
        expr.constant += value;
    }

    pub fn add_linear_term_to_expr(&mut self, expr: &mut LinearExpression, coeff: f64, identifier: String) {
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

    pub fn push_eqn(&mut self, lhs: LinearExpression, rhs: LinearExpression) {
        self.equations.push((lhs, rhs))
    }
}
