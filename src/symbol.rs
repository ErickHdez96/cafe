use crate::arena::{Arena, Id};
use std::{cell::RefCell, collections::HashMap, fmt};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(Id<String>);

impl Symbol {
    pub fn intern(value: impl AsRef<str>) -> Self {
        let value = value.as_ref();
        SYMBOLS.with(|symbols| {
            let mut symbols = symbols.borrow_mut();
            match symbols.items.get(value) {
                Some(s) => *s,
                None => {
                    let id = Self::intern_raw(value);
                    symbols.items.insert(id.resolve(), id);
                    id
                }
            }
        })
    }

    fn intern_raw(value: impl Into<String>) -> Self {
        let value = value.into();
        Self::with_mut(|arena| Symbol(arena.intern(value)))
    }

    pub fn resolve(self) -> &'static str {
        // SAFETY: The arena stores Strings, even if the Strings get moved when the arena grows,
        // the stored data of the Strings remain in place. The arena also remains in memory until
        // the end of the program.
        Self::with(|arena| unsafe {
            std::mem::transmute::<&str, &'static str>(arena.get(self.0).as_str())
        })
    }

    fn with_mut<T>(f: impl FnOnce(&mut Arena<String>) -> T) -> T {
        ARENA.with(|arena| f(&mut arena.borrow_mut()))
    }

    fn with<T>(f: impl Fn(&Arena<String>) -> T) -> T {
        ARENA.with(|arena| f(&arena.borrow_mut()))
    }
}

impl From<&str> for Symbol {
    fn from(value: &str) -> Self {
        Self::intern(value)
    }
}

impl From<String> for Symbol {
    fn from(value: String) -> Self {
        Self::intern(value)
    }
}

impl From<&String> for Symbol {
    fn from(value: &String) -> Self {
        Self::intern(value)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.resolve())
    }
}

#[derive(Default)]
struct Symbols {
    items: HashMap<&'static str, Symbol>,
}

thread_local! {
    static ARENA: RefCell<Arena<String>> = RefCell::default();
    static SYMBOLS: RefCell<Symbols> = RefCell::default();
}
