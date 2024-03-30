use std::fmt::Write;
use std::hash::Hash;

use crate::symbol::Symbol;

pub fn mangle_symbol(symbol: Symbol) -> Symbol {
    let sym = symbol.resolve();
    if !sym.contains(' ') {
        symbol
    } else {
        let paths = sym.split(' ');
        paths
            .into_iter()
            .fold(String::from("_T"), |mut out, p| {
                let p = encode_symbol(p);
                let _ = write!(out, "{}{}", p.len(), p);
                out
            })
            .into()
    }
}

fn encode_symbol(symbol: &str) -> String {
    let mut out = String::with_capacity(symbol.len());

    for c in symbol.chars() {
        if c.is_ascii_alphanumeric() {
            out.push(c);
            continue;
        }

        out.push_str(&format!("_{}_", c as u32));
    }

    out
}

pub trait Id: PartialEq + Eq + Copy + Hash {
    fn new() -> Self;
}

pub trait Intern {
    type Id;
    fn intern(self) -> Self::Id;
}

pub trait Resolve {
    type Target;
    fn resolve(self) -> &'static Self::Target;
}

#[macro_export]
macro_rules! new_id {
    ($(#[$attr:meta])* $vis:vis struct $name:ident $(, $target:ident, $store:ident)?) => {
        new_id!($(#[$attr])* $vis struct $name(u64) $(, $target, $store)?);
    };
    ($(#[$attr:meta])* $vis:vis struct $name:ident($ty:ty) $(, $target:ident, $store:ident)?) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        $(#[$attr])*
        $vis struct $name($ty);

        impl $crate::utils::Id for $name {
            fn new() -> Self {
                $name::$name.with(|n| {
                    let id = n.get();
                    n.set(id + 1);
                    Self(id)
                })
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        $(impl $crate::utils::Intern for $target {
            type Id = $name;
            fn intern(self) -> Self::Id {
                $crate::interner::Store::with(|s| s.$store.intern(self))
            }
        }

        impl $crate::utils::Resolve for $name {
            type Target = $target;
            fn resolve(self) -> &'static Self::Target {
                $crate::interner::Store::with(|s| s.$store.resolve(self))
            }
        })?

        impl $name {
            thread_local! {
                #[allow(non_upper_case_globals)]
                static $name: std::cell::Cell<$ty> = std::cell::Cell::new(0);
            }
        }
    };
}
