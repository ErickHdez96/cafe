use std::{
    cell::RefCell,
    collections::{
        hash_map::{DefaultHasher, Entry},
        HashMap, HashSet,
    },
    hash::{Hash, Hasher},
    path::PathBuf,
    rc::Rc,
};

use crate::{
    diagnostics::Diagnostic,
    file::{FileId, SourceFile},
    syntax::parser::ParseResult,
};

mod providers;

pub type Res<T> = Result<T, Diagnostic>;
pub type QCtx = BuildSystem;

fn get_hash(h: impl Hash) -> u64 {
    let mut hasher = DefaultHasher::new();
    h.hash(&mut hasher);
    hasher.finish()
}

macro_rules! build_system {
    ($bsname:ident, $storagename:ident, $({
        name: $name:ident,
        provider: $provider:path,
        key: $key:ty,
        result: $result:ty,
    }),+ $(,)?) => {
        #[derive(Default)]
        struct $storagename {
            $($name: std::collections::HashMap<$key, $result>),+
        }

        pub struct $bsname {
            qctx: RefCell<QueryCtx>,
            cache: RefCell<$storagename>,
            $(pub $name: fn(&$bsname, $key) -> $result),+
        }

        impl $bsname {
            pub fn new() -> Self {
                Self::default()
            }

            pub fn mark_dirty(&self, task: usize, key: impl Hash) {
                self.qctx.borrow_mut().mark_dirty(Query {
                    task,
                    key: get_hash(key),
                });
            }

            $(impl_query!($name, $key, $result);)+
        }

        impl Default for $bsname {
            fn default() -> Self {
                Self {
                    qctx: RefCell::default(),
                    cache: RefCell::default(),
                    $($name: $provider),+
                }
            }
        }
    };
}

macro_rules! impl_query {
    ($name:ident, $key:ty, $res:ty) => {
        pub fn $name(&self, key: $key) -> $res {
            let query = Query {
                task: self.$name as usize,
                key: get_hash(&key),
            };
            self.qctx.borrow_mut().prologue(query);

            if !self.qctx.borrow().is_dirty(query) {
                if let Some(cached_value) = self.cache.borrow().$name.get(&key) {
                    self.qctx.borrow_mut().epilogue(get_hash(&cached_value));
                    return cached_value.clone();
                }
            }

            let result = (self.$name)(self, key.clone());
            self.cache.borrow_mut().$name.insert(key, result.clone());
            self.qctx.borrow_mut().epilogue(get_hash(&result));
            result
        }
    };
}

build_system! {
    BuildSystem,
    BuildSystemStorage,
    {
        name: read_file,
        provider: providers::read_file_provider,
        key: PathBuf,
        result: Res<Rc<SourceFile>>,
    },
    {
        name: file,
        provider: providers::file_provider,
        key: FileId,
        result: Option<Rc<SourceFile>>,
    },
    {
        name: parse,
        provider: providers::parse_provider,
        key: PathBuf,
        result: Res<ParseResult>,
    },
}

/// A Query is the pair of the provider function (its pointer) and a key (its hash.)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Query {
    task: usize,
    key: u64,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
struct QueryInfo {
    deps: HashSet<Query>,
    parents: HashSet<Query>,
    dirty: bool,
    result_hash: u64,
}

#[derive(Debug, Default)]
struct QueryCtx {
    task_stack: Vec<Query>,
    queries: HashMap<Query, QueryInfo>,
}

impl QueryCtx {
    fn prologue(&mut self, child: Query) {
        if let Some(parent) = self.current_task() {
            self.register_dependency(parent, child);
        }
        self.push_task(child);
    }

    fn epilogue(&mut self, result_hash: u64) {
        let query = self.exit_task();
        match self.queries.entry(query) {
            Entry::Occupied(mut o) => {
                let info = o.get_mut();
                info.dirty = false;
                info.result_hash = result_hash;
            }
            Entry::Vacant(v) => {
                v.insert(QueryInfo {
                    dirty: false,
                    result_hash,
                    ..Default::default()
                });
            }
        }
    }

    fn current_task(&mut self) -> Option<Query> {
        self.task_stack.last().copied()
    }

    fn push_task(&mut self, query: Query) {
        self.task_stack.push(query);
    }

    fn exit_task(&mut self) -> Query {
        self.task_stack
            .pop()
            .expect("tried to exit from a task, but stack was empty")
    }

    fn register_dependency(&mut self, from: Query, to: Query) {
        match self.queries.entry(from) {
            Entry::Occupied(mut o) => {
                o.get_mut().deps.insert(to);
            }
            Entry::Vacant(v) => {
                v.insert(QueryInfo {
                    deps: [to].into(),
                    ..Default::default()
                });
            }
        }

        match self.queries.entry(to) {
            Entry::Occupied(mut o) => {
                o.get_mut().parents.insert(from);
            }
            Entry::Vacant(v) => {
                v.insert(QueryInfo {
                    parents: [from].into(),
                    ..Default::default()
                });
            }
        }
    }

    fn mark_dirty(&mut self, q: Query) {
        let parents = match self.queries.get_mut(&q) {
            Some(qi) => {
                if !qi.dirty {
                    qi.dirty = true;
                }
                // PERF
                qi.parents.clone()
            }
            None => return,
        };
        for p in parents {
            self.mark_dirty(p);
        }
    }

    fn is_dirty(&self, q: Query) -> bool {
        match self.queries.get(&q) {
            Some(i) => i.dirty,
            None => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;

    use super::*;

    #[test]
    fn partial_dirty_marking() {
        let mut qctx = QueryCtx::default();
        let qa1 = Query { task: 0, key: 0 };
        let qa2 = Query { task: 0, key: 1 };
        let qb1 = Query { task: 1, key: 1 };
        let qc1 = Query { task: 2, key: 1 };

        // c1 -------a2
        //  \    /
        //   --b1----a1
        qctx.register_dependency(qb1, qa1);
        qctx.register_dependency(qb1, qa2);

        qctx.register_dependency(qc1, qb1);
        qctx.register_dependency(qc1, qa2);

        // c1*-------a2
        //  \    /
        //   --b1*---a1
        qctx.mark_dirty(qb1);
        assert!(!qctx.is_dirty(qa1));
        assert!(!qctx.is_dirty(qa2));
        assert!(qctx.is_dirty(qb1));
        assert!(qctx.is_dirty(qc1));
    }

    #[test]
    fn transitive_dirty_marking() {
        let mut qctx = QueryCtx::default();
        let qa1 = Query { task: 0, key: 0 };
        let qa2 = Query { task: 0, key: 1 };
        let qb1 = Query { task: 1, key: 1 };
        let qc1 = Query { task: 2, key: 1 };

        // c1 -------a2
        //  \    /
        //   --b1----a1
        qctx.register_dependency(qb1, qa1);
        qctx.register_dependency(qb1, qa2);

        qctx.register_dependency(qc1, qb1);
        qctx.register_dependency(qc1, qa2);

        // c1*-------a2
        //  \    /
        //   --b1*---a1*
        qctx.mark_dirty(qa1);
        assert!(qctx.is_dirty(qa1));
        assert!(qctx.is_dirty(qb1));
        assert!(qctx.is_dirty(qc1));
        assert!(!qctx.is_dirty(qa2));
    }

    #[test]
    fn build_system() {
        thread_local! {
            static OUTSISE_A0: Cell<i32> = Cell::new(10);
            static OUTSISE_A1: Cell<i32> = Cell::new(20);
        }

        fn set_a0(v: i32) {
            OUTSISE_A0.with(|a0| a0.set(v));
        }
        fn set_a1(v: i32) {
            OUTSISE_A1.with(|a1| a1.set(v));
        }

        fn a_provider(_: &TestBS, key: i32) -> i32 {
            match key {
                0 => OUTSISE_A0.with(|a0| a0.get()),
                1 => OUTSISE_A1.with(|a0| a0.get()),
                _ => 0,
            }
        }

        fn b_provider(qctx: &TestBS, key: i32) -> i32 {
            match key {
                0 => qctx.a(0) + qctx.a(1),
                _ => 0,
            }
        }

        build_system! {
            TestBS,
            TestBSStorage,
            {
                name: a,
                provider: a_provider,
                key: i32,
                result: i32,
            },
            {
                name: b,
                provider: b_provider,
                key: i32,
                result: i32,
            },
        }

        let qctx = TestBS::new();

        assert_eq!(10, qctx.a(0));
        assert_eq!(20, qctx.a(1));
        set_a0(11);
        // Value cached until marked dirty.
        assert_eq!(10, qctx.a(0));
        qctx.mark_dirty(qctx.a as usize, 0);
        // New value is used now
        assert_eq!(11, qctx.a(0));

        assert_eq!(31, qctx.b(0));
        set_a0(12);
        set_a1(21);

        // b0's dependencies are still cached, so b0 runs again
        // but returns the same value.
        qctx.mark_dirty(qctx.b as usize, 0);
        assert_eq!(31, qctx.b(0));

        // Only one of its dependencies is recalculated
        qctx.mark_dirty(qctx.a as usize, 0);
        assert_eq!(32, qctx.b(0));

        // Recalculate the whole tree
        qctx.mark_dirty(qctx.a as usize, 0);
        qctx.mark_dirty(qctx.a as usize, 1);
        assert_eq!(33, qctx.b(0));
    }
}
