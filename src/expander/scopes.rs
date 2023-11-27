use std::{cell::Cell, collections::HashSet, num::NonZeroUsize};

use crate::syntax::cst::{SynExp, SynSymbol};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scope(u64);

#[allow(clippy::new_without_default)]
impl Scope {
    pub fn new() -> Self {
        SCOPE_ID.with(|s| {
            let id = s.get();
            s.set(id.saturating_add(1));
            Self(id)
        })
    }

    pub const fn core() -> Self {
        Self(0)
    }

    pub const fn value(self) -> u64 {
        self.0
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Scopes(HashSet<Scope>);

impl Scopes {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn core() -> Self {
        Self([Scope::core()].into())
    }

    pub fn with(&self, scope: Scope) -> Self {
        let mut new = self.clone();
        new.add(scope);
        new
    }

    pub fn add(&mut self, scope: Scope) {
        self.0.insert(scope);
    }

    pub fn flip(&mut self, scope: Scope) {
        if self.0.take(&scope).is_none() {
            self.0.insert(scope);
        }
    }

    pub fn subset(&self, other: &Self) -> Option<NonZeroUsize> {
        let mut count = 0;
        for s in &self.0 {
            if !other.0.contains(s) {
                return None;
            }
            count += 1;
        }
        NonZeroUsize::new(count)
    }
}

trait Scoped {
    fn add_scope(&mut self, scope: Scope);
    fn flip_scope(&mut self, scope: Scope);
}

impl Scoped for SynExp {
    fn add_scope(&mut self, scope: Scope) {
        match self {
            SynExp::List(list) => {
                for sexp in list.sexps_mut() {
                    sexp.add_scope(scope);
                }
            }
            SynExp::Symbol(sy) => sy.add_scope(scope),
            SynExp::Boolean(_) => todo!(),
            SynExp::Char(_) => todo!(),
        }
    }

    fn flip_scope(&mut self, scope: Scope) {
        match self {
            SynExp::List(list) => {
                for sexp in list.sexps_mut() {
                    sexp.flip_scope(scope);
                }
            }
            SynExp::Symbol(sy) => sy.flip_scope(scope),
            SynExp::Boolean(_) => todo!(),
            SynExp::Char(_) => todo!(),
        }
    }
}

impl Scoped for SynSymbol {
    fn add_scope(&mut self, scope: Scope) {
        self.scopes_mut().add(scope);
    }

    fn flip_scope(&mut self, scope: Scope) {
        self.scopes_mut().flip(scope);
    }
}

thread_local! {
    static SCOPE_ID: Cell<u64> = Cell::new(1);
}
