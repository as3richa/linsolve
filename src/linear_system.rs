use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::mem;
use std::ops::AddAssign;

const EPSILON: f64 = 1.0e-9;

#[derive(Debug)]
pub struct LinearSystem {
    system: Vec<(LinearExpression, LinearExpression)>,
}

#[derive(Debug)]
pub struct LinearExpression {
    terms: Vec<Term>,
}

#[derive(Debug)]
pub enum Term {
    Constant(f64),
    Linear(f64, String),
}

#[derive(Debug)]
pub enum SolutionSet {
    Empty,
}

#[derive(Debug)]
struct CollectedExpression {
    coeffs: Vec<f64>,
    constant: f64,
}

fn is_zero(value: f64) -> bool {
    value.abs() < EPSILON
}

impl LinearSystem {
    pub fn new() -> Self {
        Self { system: vec![] }
    }

    pub fn push_equation(&mut self, lhs: LinearExpression, rhs: LinearExpression) {
        self.system.push((lhs, rhs))
    }

    pub fn solve(self) -> SolutionSet {
        println!("{:?}", self.system);

        let (system, names) = collect(self.system);
        println!("{:?}\n---", system);

        let mut system = normalize(system);
        println!("{:?}\n---", system);

        let mut eqn_index = 0;

        while eqn_index < system.len() {
            system = remove_tautologies(system);

            println!("{:?}\n---", system);

            if system.len() <= 1 {
                break;
            }

            let (mut coeffs, mut constant) = mem::replace(&mut system[eqn_index], (vec![], 0.0));
            assert!(!is_zero(constant));

            let nonzero_coeff = coeffs.iter().cloned().enumerate().find(|(_, coeff)| !is_zero(*coeff));

            match nonzero_coeff {
                Some((index, value)) => {
                    for coeff in coeffs.iter_mut() {
                        *coeff /= value;
                    }
                    constant /= value;

                    for (other_coeffs, other_constant) in system.iter_mut() {
                        if other_coeffs.len() <= index || is_zero(other_coeffs[index]) {
                            continue;
                        }

                        if other_coeffs.len() < coeffs.len() {
                            other_coeffs.resize(coeffs.len(), 0.0);
                        }

                        let factor = other_coeffs[index];

                        for (other_coeff, coeff) in other_coeffs.iter_mut().zip(coeffs.iter()) {
                            *other_coeff -= coeff * factor;
                        }
                        *other_constant -= factor * constant;

                        other_coeffs[index] = 0.0;
                    }

                    system[eqn_index] = (coeffs, constant);
                }
                None => {
                    system[eqn_index] = (coeffs, constant);
                    break;
                }
            }

            println!("{:?}\n---", system);
            eqn_index += 1;
        }

        SolutionSet::Empty
    }
}

impl LinearExpression {
    pub fn new() -> LinearExpression {
        Self { terms: vec![] }
    }
}

impl AddAssign<Term> for LinearExpression {
    fn add_assign(&mut self, term: Term) {
        self.terms.push(term);
    }
}

fn collect(
    system: Vec<(LinearExpression, LinearExpression)>,
) -> (Vec<(CollectedExpression, CollectedExpression)>, Vec<String>) {
    let mut names: Vec<String> = vec![];
    let mut ids_by_name: HashMap<String, usize> = HashMap::new();

    let mut collected_system: Vec<(CollectedExpression, CollectedExpression)> = Vec::with_capacity(system.len());

    {
        let mut find_id = |name: String| match ids_by_name.entry(name) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let id = names.len();
                names.push(entry.key().clone());
                entry.insert(id);
                id
            }
        };

        let mut collect_expr = |expr: LinearExpression| {
            let mut collected_expr = CollectedExpression {
                coeffs: vec![],
                constant: 0.0,
            };

            for term in expr.terms.into_iter() {
                match term {
                    Term::Linear(coeff, name) => {
                        let id = find_id(name);
                        if collected_expr.coeffs.len() <= id {
                            collected_expr.coeffs.resize(id + 1, 0.0);
                        }
                        collected_expr.coeffs[id] += coeff;
                    }
                    Term::Constant(value) => collected_expr.constant += value,
                }
            }

            collected_expr
        };

        for (lhs, rhs) in system.into_iter() {
            collected_system.push((collect_expr(lhs), collect_expr(rhs)));
        }
    }

    (collected_system, names)
}

fn normalize(system: Vec<(CollectedExpression, CollectedExpression)>) -> Vec<(Vec<f64>, f64)> {
    let normalize_equation = |(lhs, rhs): (CollectedExpression, CollectedExpression)| {
        let mut coeffs = lhs.coeffs;

        if coeffs.len() < rhs.coeffs.len() {
            coeffs.resize(rhs.coeffs.len(), 0.0);
        }

        for (left, right) in coeffs.iter_mut().zip(rhs.coeffs.into_iter()) {
            *left -= right;
        }

        let constant = rhs.constant - lhs.constant;

        (coeffs, constant)
    };

    system.into_iter().map(normalize_equation).collect()
}

fn remove_tautologies(system: Vec<(Vec<f64>, f64)>) -> Vec<(Vec<f64>, f64)> {
    let result: Vec<(Vec<f64>, f64)> = system
        .into_iter()
        .filter(|(coeffs, constant)| !(is_zero(*constant) && coeffs.iter().all(|&coeff| is_zero(coeff))))
        .collect();

    if result.is_empty() {
        return vec![(vec![], 0.0)];
    }

    result
}
