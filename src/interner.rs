use std::{cell::RefCell, collections::HashMap, hash::Hash};

use crate::{
    new_syntax::ast::{ModId, ModuleName},
    utils::Id,
};

#[derive(Debug)]
pub struct Store {
    pub modules: Interner<ModId, ModuleName>,
}

impl Store {
    fn new() -> Self {
        Self {
            modules: Interner::new(),
        }
    }

    pub fn with<F: FnOnce(&mut Store) -> T, T>(f: F) -> T {
        STORE.with(|s| f(&mut s.borrow_mut()))
    }
}

#[derive(Debug)]
pub struct Interner<I, T> {
    to_id: HashMap<T, I>,
    from_id: HashMap<I, T>,
}

impl<I, T> Interner<I, T> {
    fn new() -> Self {
        Self {
            to_id: HashMap::default(),
            from_id: HashMap::default(),
        }
    }
}

impl<I, T> Interner<I, T>
where
    I: Id,
{
    pub fn intern(&mut self, v: T) -> I
    where
        T: Clone + PartialEq + Eq + Hash,
    {
        match self.to_id.get(&v) {
            Some(i) => *i,
            None => {
                let id = I::new();
                self.to_id.insert(v.clone(), id);
                self.from_id.insert(id, v);
                id
            }
        }
    }

    pub fn resolve(&mut self, id: I) -> &'static T {
        // SAFETY: The interner can only be constructed in the thread_local below, meaning all
        // references returned from it will live until the end of the program.
        unsafe { std::mem::transmute(self.from_id.get(&id).unwrap()) }
    }
}

thread_local! {
    static STORE: RefCell<Store> = RefCell::new(Store::new());
}
