use std::{borrow::Borrow, collections::HashMap, hash::Hash, ops};

#[derive(Debug, Clone)]
enum Parent<'parent, T> {
    Owned(Box<T>),
    Ref(&'parent T),
}

impl<T> ops::Deref for Parent<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            Parent::Owned(o) => o,
            Parent::Ref(r) => r,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Env<'parent, K, V> {
    parent: Option<Parent<'parent, Self>>,
    bindings: HashMap<K, V>,
}

impl<K, V> Default for Env<'_, K, V> {
    fn default() -> Self {
        Self {
            parent: None,
            bindings: HashMap::default(),
        }
    }
}

impl<'p, K, V> Env<'p, K, V> {
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn enter_consume(self) -> Self {
        Self {
            parent: Some(Parent::Owned(Box::new(self))),
            ..Default::default()
        }
    }

    #[must_use]
    pub fn enter(&'p self) -> Env<'p, K, V> {
        Self {
            parent: Some(Parent::Ref(self)),
            ..Default::default()
        }
    }
}

impl<K, V> Env<'_, K, V>
where
    K: Hash + Eq,
{
    pub fn insert(&mut self, k: K, v: V) {
        self.bindings.insert(k, v);
    }

    pub fn has_immediate<Q: ?Sized>(&self, k: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.bindings.contains_key(k)
    }

    pub fn get<Q: ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.bindings
            .get(k)
            .or_else(|| self.parent.as_ref().and_then(|p| p.get(k)))
    }

    pub fn get_all<Q: ?Sized>(&self, k: &Q) -> Vec<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        let mut bindings = vec![];
        let mut c = Some(self);
        while let Some(s) = c {
            if let Some(b) = s.bindings.get(k.borrow()) {
                bindings.push(b);
            }
            c = s.parent.as_deref();
        }
        bindings
    }
}
