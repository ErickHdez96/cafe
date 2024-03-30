use std::{cell::RefCell, collections::HashMap, fmt};

use crate::arena::{Arena, Id};

use super::ModuleName;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModId(Id<Box<ModuleName>>);

impl ModId {
    pub fn intern(value: ModuleName) -> Self {
        MODNAMES.with(|modids| {
            let mut modids = modids.borrow_mut();
            match modids.items.get(&value) {
                Some(s) => *s,
                None => {
                    let id = Self::intern_raw(value);
                    modids.items.insert(id.resolve(), id);
                    id
                }
            }
        })
    }

    fn intern_raw(value: ModuleName) -> Self {
        Self::with_mut(|arena| ModId(arena.intern(Box::new(value))))
    }

    pub fn resolve(self) -> &'static ModuleName {
        Self::with(|arena| unsafe {
            std::mem::transmute::<&ModuleName, &'static ModuleName>(arena.get(self.0).as_ref())
        })
    }

    fn with_mut<T>(f: impl FnOnce(&mut Arena<Box<ModuleName>>) -> T) -> T {
        ARENA.with(|arena| f(&mut arena.borrow_mut()))
    }

    fn with<T>(f: impl Fn(&Arena<Box<ModuleName>>) -> T) -> T {
        ARENA.with(|arena| f(&arena.borrow_mut()))
    }
}

impl fmt::Display for ModId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.resolve())
    }
}

#[derive(Default)]
struct ModIds {
    items: HashMap<&'static ModuleName, ModId>,
}

thread_local! {
    static ARENA: RefCell<Arena<Box<ModuleName>>> = RefCell::default();
    static MODNAMES: RefCell<ModIds> = RefCell::default();
}
