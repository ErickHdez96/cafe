use std::{cell::Cell, collections::HashSet, hash, num::NonZeroUsize};

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

impl hash::Hash for Scopes {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        for scope in &self.0 {
            scope.hash(state);
        }
    }
}

thread_local! {
    static SCOPE_ID: Cell<u64> = const { Cell::new(1) };
}
