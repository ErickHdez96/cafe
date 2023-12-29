#[macro_export]
macro_rules! new_id {
    ($(#[$attr:meta])* $vis:vis $name:ident) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        $(#[$attr])*
        $vis struct $name(u64);

        impl $name {
            #[allow(clippy::new_without_default)]
            pub fn new() -> Self {
                $name::$name.with(|n| {
                    let id = n.get();
                    n.set(id + 1);
                    Self(id)
                })
            }

            thread_local! {
                #[allow(non_upper_case_globals)]
                static $name: std::cell::Cell<u64> = std::cell::Cell::new(0);
            }
        }

    };
}
