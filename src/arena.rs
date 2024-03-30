use std::{cell::Cell, marker::PhantomData};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Id<T> {
    arena_id: u32,
    idx: usize,
    _phantom: PhantomData<T>,
}

impl<T: Clone> Copy for Id<T> {}

pub struct Arena<T> {
    id: u32,
    items: Vec<T>,
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        let id = ID.with(|n| {
            let id = n.get();
            n.set(id + 1);
            id
        });
        Self {
            id,
            items: Vec::new(),
        }
    }

    pub fn intern(&mut self, value: T) -> Id<T> {
        let id = self.next_id();
        self.items.push(value);
        id
    }

    pub fn get(&self, id: Id<T>) -> &T {
        assert_eq!(self.id, id.arena_id, "id belongs to a different arena");
        self.items.get(id.idx).expect("invalid id")
    }

    fn next_id(&self) -> Id<T> {
        Id {
            arena_id: self.id,
            idx: self.items.len(),
            _phantom: PhantomData,
        }
    }
}

thread_local! {
    static ID: Cell<u32> = Cell::default();
}
