use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::iter;
use std::mem;
use std::ops::AddAssign;

use crate::renderers::Renderer;

const EPSILON: f64 = 1.0e-9;

/* ================================================================================ */

pub enum InputTerm {
    Constant(f64),
    Linear(f64, String),
}

pub struct LinearExpression {
    terms: Vec<InputTerm>,
}

pub struct LinearSystem {
    equations: Vec<(LinearExpression, LinearExpression)>,
}

#[derive(Clone)]
pub enum IndexedTerm {
    Constant(f64),
    Linear(f64, usize),
}

type ExtractedExpression = Vec<IndexedTerm>;
type ExtractedSystem = Vec<(ExtractedExpression, ExtractedExpression)>;

type CollectedExpression = (Vec<f64>, f64);
type CollectedSystem = Vec<(CollectedExpression, CollectedExpression)>;

type NormalizedSystem = Vec<(Vec<f64>, f64)>;

fn is_zero(value: f64) -> bool {
    value.abs() < EPSILON
}

impl LinearExpression {
    pub fn new() -> LinearExpression {
        Self { terms: vec![] }
    }
}

impl AddAssign<InputTerm> for LinearExpression {
    fn add_assign(&mut self, term: InputTerm) {
        self.terms.push(term);
    }
}

fn extract(system: LinearSystem) -> (ExtractedSystem, Vec<String>) {
    let mut names: Vec<String> = vec![];
    let mut ids_by_name: HashMap<String, usize> = HashMap::new();

    let extracted = {
        let mut find_id = |name: String| match ids_by_name.entry(name) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let id = names.len();
                names.push(entry.key().clone());
                entry.insert(id);
                id
            }
        };

        let mut extract_expr = |expr: LinearExpression| {
            expr.terms
                .into_iter()
                .map(|term| match term {
                    InputTerm::Linear(coeff, name) => IndexedTerm::Linear(coeff, find_id(name)),
                    InputTerm::Constant(value) => IndexedTerm::Constant(value),
                })
                .collect()
        };

        let iter = system.equations.into_iter();
        iter.map(|(lhs, rhs)| (extract_expr(lhs), extract_expr(rhs))).collect()
    };

    (extracted, names)
}

fn collect(system: ExtractedSystem, variables: usize) -> CollectedSystem {
    let collect_expr = |expr: ExtractedExpression| {
        let mut coeffs = vec![0.0; variables];
        let mut constant = 0.0;

        for term in expr.into_iter() {
            match term {
                IndexedTerm::Linear(coeff, id) => coeffs[id] += coeff,
                IndexedTerm::Constant(value) => constant += value,
            }
        }

        (coeffs, constant)
    };

    let iter = system.into_iter();
    iter.map(|(lhs, rhs)| (collect_expr(lhs), collect_expr(rhs))).collect()
}

fn normalize(system: CollectedSystem) -> NormalizedSystem {
    let normalize_equation = |(lhs, rhs): (CollectedExpression, CollectedExpression)| {
        let (left_coeffs, left_constant) = lhs;
        let (right_coeffs, right_constant) = rhs;

        let mut coeffs = left_coeffs;

        for (left, right) in coeffs.iter_mut().zip(right_coeffs.into_iter()) {
            *left -= right;
        }

        let constant = right_constant - left_constant;

        (coeffs, constant)
    };

    system.into_iter().map(normalize_equation).collect()
}

fn remove_tautologies(system: NormalizedSystem) -> NormalizedSystem {
    let result: NormalizedSystem = system
        .into_iter()
        .filter(|(coeffs, constant)| !(is_zero(*constant) && coeffs.iter().all(|&coeff| is_zero(coeff))))
        .collect();

    if result.is_empty() {
        return vec![(vec![], 0.0)];
    }

    result
}

impl LinearSystem {
    pub fn new() -> Self {
        Self { equations: vec![] }
    }

    pub fn push_equation(&mut self, lhs: LinearExpression, rhs: LinearExpression) {
        self.equations.push((lhs, rhs))
    }

    pub fn solve<R: Renderer>(self, mut renderer: R) {
        let (system, names) = extract(self);
        let variables = names.len();
        renderer.set_names(names);
        renderer.write_str("Consider the system of linear equations:\n");
        render_extracted(&mut renderer, &system);

        let system = collect(system, variables);
        renderer.write_str("Collecting like terms:\n");
        // render_collected(&mut renderer, &system);

        let mut system = normalize(system);
        renderer.write_str("Gathering linear terms on the left and constant terms on the right:\n");
        render_normalized(&mut renderer, &system);

        let mut eqn_index = 0;

        while eqn_index < system.len() {
            system = remove_tautologies(system);
            renderer.write_str("Yada yada yada:\n");
            render_normalized(&mut renderer, &system);

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

            eqn_index += 1;
        }
    }
}

fn render_extracted<R: Renderer>(renderer: &mut R, system: &ExtractedSystem) {
    let iter = system
        .iter()
        .map(|(lhs, rhs)| (lhs.iter().cloned(), rhs.iter().cloned()));
    renderer.write_system(iter);
}

/*fn render_collected<R: Renderer>(renderer: &mut R, system: &CollectedSystem) {
    let iter = system.iter().map(|(lhs, rhs)| {
        let expr_iterator = |expression: &CollectedExpression| {
            let (coeffs, constant) = expression;

            let coeffs_iter = coeffs
                .iter()
                .cloned()
                .enumerate()
                .map(|(index, coeff)| IndexedTerm::Linear(coeff, index));
            let constant_iter = iter::once(IndexedTerm::Constant(*constant));
            coeffs_iter.chain(constant_iter)
        };

        let lhs_iter = expr_iterator(lhs);
        let rhs_iter = expr_iterator(rhs);

        (lhs_iter, rhs_iter)
    });

    renderer.write_system(iter);
}*/

fn render_normalized<R: Renderer>(renderer: &mut R, system: &NormalizedSystem) {
    let iter = system.iter().map(|(coeffs, constant)| {
        (
            coeffs
                .iter()
                .cloned()
                .enumerate()
                .map(|(index, coeff)| IndexedTerm::Linear(coeff, index)),
            iter::once(IndexedTerm::Constant(*constant)),
        )
    });

    renderer.write_system(iter);
}
