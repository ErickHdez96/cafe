use std::{
    borrow::Borrow,
    collections::{
        hash_map::{self, Iter},
        HashMap,
    },
    hash::Hash,
    ops,
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Parent<'parent, T> {
    Owned(Box<T>),
    Rc(Rc<T>),
    Ref(&'parent T),
}

impl<T> ops::Deref for Parent<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            Parent::Owned(o) => o,
            Parent::Rc(r) => r,
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

impl<K, V> Hash for Env<'_, K, V>
where
    K: Hash,
    V: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.parent.hash(state);
        for (k, v) in &self.bindings {
            k.hash(state);
            v.hash(state);
        }
    }
}

impl<K, V> PartialEq for Env<'_, K, V>
where
    K: Hash + Eq,
    V: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.parent == other.parent
            && self.bindings.keys().len() == other.bindings.keys().len()
            && self
                .bindings
                .iter()
                .all(|(k, v)| other.bindings.get(k) == Some(v))
    }
}

impl<K, V> Eq for Env<'_, K, V>
where
    K: Hash + Eq,
    V: PartialEq,
{
}

impl<K, V> Env<'static, K, V> {
    #[must_use]
    pub fn from_rc(rc: Rc<Self>) -> Self {
        Self {
            parent: Some(Parent::Rc(rc)),
            ..Default::default()
        }
    }

    #[must_use]
    pub fn iter(&self) -> Iter<'_, K, V> {
        self.bindings.iter()
    }
}

impl<K, V> Env<'static, K, V> {
    pub fn with_bindings(bindings: HashMap<K, V>) -> Env<'static, K, V> {
        Self {
            bindings,
            ..Default::default()
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

    pub fn into_bindings(self) -> HashMap<K, V> {
        self.bindings
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

    pub fn get_immediate<Q: ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.bindings.get(k)
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

    pub fn bindings(&self) -> hash_map::Iter<K, V> {
        self.bindings.iter()
    }
}
