use std::hash::Hash;

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

        $(impl $crate::utils::Intern for $target {
            type Id = $name;
            fn intern(self) -> Self::Id {
                Store::with(|s| s.$store.intern(self))
            }
        }

        impl $crate::utils::Resolve for $name {
            type Target = $target;
            fn resolve(self) -> &'static Self::Target {
                Store::with(|s| s.$store.resolve(self))
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
