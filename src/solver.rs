use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::iter;
use std::mem;
use std::ops::AddAssign;

use crate::renderers::Renderer;

const EPSILON: f64 = 1.0e-9;

fn is_zero(value: f64) -> bool {
    value.abs() < EPSILON
}

macro_rules! pairify {
    ($function:ident) => {
        |(left, right)| ($function(left), $function(right))
    };
}

pub enum InputTerm {
    Constant(f64),
    Linear(f64, String),
}

pub struct InputExpression {
    terms: Vec<InputTerm>,
}

pub struct InputSystem {
    equations: Vec<(InputExpression, InputExpression)>,
}

#[derive(Clone)]
pub enum IndexedTerm {
    Constant(f64),
    Linear(f64, usize),
}

type ExtractedExpression = Vec<IndexedTerm>;

struct ExtractedSystem {
    equations: Vec<(ExtractedExpression, ExtractedExpression)>,
}

type CollectedExpression = (Vec<f64>, f64);

struct CollectedSystem {
    equations: Vec<(CollectedExpression, CollectedExpression)>,
}

struct CollectedExpressionIterator<'a> {
    expression: &'a CollectedExpression,
    index: usize,
}

struct NormalizedSystem {
    equations: Vec<(Vec<f64>, f64)>,
}

impl InputExpression {
    pub fn new() -> InputExpression {
        Self { terms: vec![] }
    }
}

impl AddAssign<InputTerm> for InputExpression {
    fn add_assign(&mut self, term: InputTerm) {
        self.terms.push(term);
    }
}

impl InputSystem {
    pub fn new() -> Self {
        Self { equations: vec![] }
    }

    pub fn push_equation(&mut self, lhs: InputExpression, rhs: InputExpression) {
        self.equations.push((lhs, rhs))
    }

    pub fn solve<R: Renderer>(self, renderer: &mut R) {
        let (extracted_system, names) = self.extract();
        let variables = names.len();
        renderer.set_names(names);
        renderer.write_str("Consider the system of linear equations:\n");
        extracted_system.render(renderer);

        let collected_system = extracted_system.collect(variables);
        renderer.write_str("Collecting like terms:\n");
        collected_system.render(renderer);

        let mut normalized_system = collected_system.normalize();
        renderer.write_str("Gathering linear terms on the left and constant terms on the right:\n");
        normalized_system.render(renderer);

        normalized_system.solve(renderer);
    }

    fn extract(self) -> (ExtractedSystem, Vec<String>) {
        let mut names: Vec<String> = vec![];
        let mut indices_by_name: HashMap<String, usize> = HashMap::new();

        let extracted_system = {
            let mut find_index = |name: String| match indices_by_name.entry(name) {
                Entry::Occupied(entry) => *entry.get(),
                Entry::Vacant(entry) => {
                    let index = names.len();
                    names.push(entry.key().clone());
                    entry.insert(index);
                    index
                }
            };

            let mut extract_expr = |expr: InputExpression| {
                expr.terms
                    .into_iter()
                    .map(|term| match term {
                        InputTerm::Linear(coeff, name) => IndexedTerm::Linear(coeff, find_index(name)),
                        InputTerm::Constant(value) => IndexedTerm::Constant(value),
                    })
                    .collect()
            };

            let equations = self.equations.into_iter();
            ExtractedSystem::new(equations.map(pairify!(extract_expr)).collect())
        };

        (extracted_system, names)
    }
}

impl ExtractedSystem {
    fn new(equations: Vec<(ExtractedExpression, ExtractedExpression)>) -> Self {
        Self { equations }
    }

    fn collect(self, variables: usize) -> CollectedSystem {
        let collect_expr = |expr: ExtractedExpression| {
            let mut coeffs = vec![0.0; variables];
            let mut constant = 0.0;

            for term in expr.into_iter() {
                match term {
                    IndexedTerm::Linear(coeff, index) => coeffs[index] += coeff,
                    IndexedTerm::Constant(value) => constant += value,
                }
            }

            (coeffs, constant)
        };

        let equations = self.equations.into_iter();
        CollectedSystem::new(equations.map(pairify!(collect_expr)).collect())
    }

    fn render<R: Renderer>(&self, renderer: &mut R) {
        let iterator = self
            .equations
            .iter()
            .map(|(lhs, rhs)| (lhs.iter().cloned(), rhs.iter().cloned()));

        renderer.write_system(iterator);
    }
}

impl CollectedSystem {
    fn new(equations: Vec<(CollectedExpression, CollectedExpression)>) -> Self {
        Self { equations }
    }

    fn normalize(self) -> NormalizedSystem {
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

        let equations = self.equations.into_iter();
        NormalizedSystem::new(equations.map(normalize_equation).collect())
    }

    fn render<R: Renderer>(&self, renderer: &mut R) {
        let iter = self.equations.iter().map(|(lhs, rhs)| {
            (
                CollectedExpressionIterator::new(lhs),
                CollectedExpressionIterator::new(rhs),
            )
        });

        renderer.write_system(iter);
    }
}

impl<'a> CollectedExpressionIterator<'a> {
    fn new(expression: &'a CollectedExpression) -> Self {
        Self { expression, index: 0 }
    }
}

impl<'a> Iterator for CollectedExpressionIterator<'a> {
    type Item = IndexedTerm;

    fn next(&mut self) -> Option<IndexedTerm> {
        let (coeffs, constant) = self.expression;

        while self.index < coeffs.len() {
            let coeff = coeffs[self.index];
            self.index += 1;

            if !is_zero(coeff) {
                return Some(IndexedTerm::Linear(coeff, self.index - 1));
            }
        }

        if self.index == coeffs.len() {
            self.index = coeffs.len() + 1;
            if !is_zero(*constant) {
                return Some(IndexedTerm::Constant(*constant));
            }
        }

        None
    }
}

impl NormalizedSystem {
    fn new(equations: Vec<(Vec<f64>, f64)>) -> Self {
        Self { equations }
    }

    fn solve<R: Renderer>(&mut self, renderer: &mut R) {
        let mut equation_index = 0;

        while equation_index < self.equations.len() {
            self.remove_tautologies();
            renderer.write_str("Removing trivial equations:\n");
            self.render(renderer);

            if self.equations.len() <= 1 {
                break;
            }

            self.step(equation_index, renderer);
            renderer.write_str("Meep meep FIXME:\n");
            self.render(renderer);

            equation_index += 1;
        }
    }

    fn remove_tautologies(&mut self) {
        let is_not_tautological =
            |(coeffs, constant): &(Vec<f64>, f64)| !is_zero(*constant) || coeffs.iter().any(|&coeff| !is_zero(coeff));

        self.equations.retain(is_not_tautological);
    }

    fn step<R: Renderer>(&mut self, equation_index: usize, renderer: &mut R) {
        let (mut coeffs, mut constant) = mem::replace(&mut self.equations[equation_index], (vec![], 0.0));
        //        assert!(!is_zero(constant));

        let nonzero_coeff = coeffs.iter().cloned().enumerate().find(|(_, coeff)| !is_zero(*coeff));

        match nonzero_coeff {
            Some((index, value)) => {
                for coeff in coeffs.iter_mut() {
                    *coeff /= value;
                }
                constant /= value;

                for (other_coeffs, other_constant) in self.equations.iter_mut() {
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

                    assert!(is_zero(other_coeffs[index]));
                }
            }
            None => (),
        }

        self.equations[equation_index] = (coeffs, constant);
    }

    fn render<R: Renderer>(&mut self, renderer: &mut R) {
        let iter = self.equations.iter().map(|(coeffs, constant)| {
            let coeffs_iterator = coeffs
                .iter()
                .cloned()
                .enumerate()
                .filter(|(_, coeff)| !is_zero(*coeff))
                .map(|(index, coeff)| IndexedTerm::Linear(coeff, index));

            (coeffs_iterator, iter::once(IndexedTerm::Constant(*constant)))
        });

        renderer.write_system(iter);
    }
}
