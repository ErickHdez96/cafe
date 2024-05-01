use std::{cmp::Ordering, collections::HashMap, fmt, rc::Rc};

use crate::arena::{Arena, Id};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyScheme {
    STy(Ty),
    QTy(Vec<Ty>, Rc<TyScheme>),
}

impl TyScheme {
    pub fn with_arena<'tyk>(&'tyk self, arena: &'tyk Arena<TyK>) -> TySchemeArena {
        let mut generic_vec = vec![];

        let mut tys = self;
        while let TyScheme::QTy(g, ty) = tys {
            tys = ty;
            generic_vec.extend_from_slice(g);
        }
        generic_vec.sort_by(|a, b| match (arena.get(a.value()), arena.get(b.value())) {
            (TyK::Var(l), TyK::Var(r)) => l.cmp(r),
            (_, _) => Ordering::Less,
        });

        TySchemeArena {
            tyscheme: self,
            arena,
            generics: GenericMap::Owned(HashMap::from_iter(generic_vec.into_iter().zip('a'..))),
        }
    }
}

impl From<Ty> for TyScheme {
    fn from(value: Ty) -> Self {
        Self::STy(value)
    }
}

impl From<Id<TyK>> for TyScheme {
    fn from(value: Id<TyK>) -> Self {
        Self::STy(Ty(value))
    }
}

impl From<&Ty> for TyScheme {
    fn from(value: &Ty) -> Self {
        Self::STy(*value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty(Id<TyK>);

impl Ty {
    pub const fn value(self) -> Id<TyK> {
        self.0
    }

    pub fn with_arena(self, arena: &Arena<TyK>) -> TyArena {
        TyArena {
            ty: self,
            tyk: arena.get(self.0),
            arena,
            generics: None,
        }
    }
}

impl From<Id<TyK>> for Ty {
    fn from(value: Id<TyK>) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum TyK {
    #[default]
    None,
    Boolean,
    Char,
    Number(NumberTy),
    String,
    Null,
    Void,
    Symbol,
    Error,

    Lambda {
        params: Vec<Ty>,
        rest: Option<Ty>,
        ret: Ty,
        generics: Vec<Ty>,
    },
    Var(usize),
    Array(Ty),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum NumberTy {
    I64,
}

impl fmt::Debug for NumberTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::I64 => write!(f, "i64"),
        }
    }
}

#[derive(Debug, Clone)]
enum GenericMap<'m> {
    Ref(&'m HashMap<Ty, char>),
    Owned(HashMap<Ty, char>),
}

impl<'m> GenericMap<'m> {
    fn get(&self, k: &Ty) -> Option<char> {
        match self {
            GenericMap::Ref(m) => m.get(k),
            GenericMap::Owned(m) => m.get(k),
        }
        .copied()
    }

    fn as_ref<'s: 'm>(&'s self) -> GenericMap<'s> {
        match self {
            GenericMap::Ref(m) => Self::Ref(m),
            GenericMap::Owned(o) => Self::Ref(o),
        }
    }
}

pub struct TySchemeArena<'tyc, 'g> {
    pub tyscheme: &'tyc TyScheme,
    pub arena: &'tyc Arena<TyK>,
    generics: GenericMap<'g>,
}

impl fmt::Debug for TySchemeArena<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self.tyscheme {
                TyScheme::STy(ty) => write!(
                    f,
                    "{:#?}",
                    TyArena {
                        ty: *ty,
                        tyk: self.arena.get(ty.value()),
                        arena: self.arena,
                        generics: Some(self.generics.as_ref()),
                    }
                ),
                TyScheme::QTy(generics, ty) => {
                    write!(
                        f,
                        "(∀ '({}) {:#?})",
                        generics
                            .iter()
                            .map(|g| format!("{}", self.generics.get(g).unwrap()))
                            .collect::<Vec<_>>()
                            .join(" "),
                        TySchemeArena {
                            tyscheme: ty,
                            arena: self.arena,
                            generics: self.generics.as_ref(),
                        },
                    )
                }
            }
        } else {
            match self.tyscheme {
                TyScheme::STy(ty) => f
                    .debug_tuple("STy")
                    .field(&ty.with_arena(self.arena))
                    .finish(),
                TyScheme::QTy(gens, tys) => f
                    .debug_tuple("QTy")
                    .field(
                        &gens
                            .iter()
                            .map(|ty| ty.with_arena(self.arena))
                            .collect::<Vec<_>>(),
                    )
                    .field(&tys.with_arena(self.arena))
                    .finish(),
            }
        }
    }
}

pub struct TyArena<'tyc, 'm> {
    pub ty: Ty,
    pub tyk: &'tyc TyK,
    pub arena: &'tyc Arena<TyK>,
    generics: Option<GenericMap<'m>>,
}

impl TyArena<'_, '_> {
    pub fn is_boolean(&self) -> bool {
        matches!(self.tyk, TyK::Boolean)
    }

    pub fn is_fn(&self) -> bool {
        matches!(self.tyk, TyK::Lambda { .. })
    }

    fn with_ty(&self, ty: Ty) -> Self {
        Self {
            ty,
            tyk: self.arena.get(ty.value()),
            arena: self.arena,
            generics: self.generics.clone(),
        }
    }
}

impl fmt::Debug for TyArena<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self.tyk {
                TyK::None => write!(f, "none"),
                TyK::Boolean => write!(f, "boolean"),
                TyK::Char => write!(f, "char"),
                TyK::Number(n) => n.fmt(f),
                TyK::String => write!(f, "string"),
                TyK::Null => write!(f, "null"),
                TyK::Void => write!(f, "void"),
                TyK::Symbol => write!(f, "symbol"),
                TyK::Lambda {
                    params, rest, ret, ..
                } => write!(
                    f,
                    "(-> {}{} {:#?})",
                    params
                        .iter()
                        .map(|p| format!("{:#?}", self.with_ty(*p)))
                        .collect::<Vec<_>>()
                        .join(" "),
                    match rest {
                        Some(r) => format!(" ({:#?} ...)", self.with_ty(*r)),
                        None => String::new(),
                    },
                    self.with_ty(*ret),
                ),
                TyK::Var(id) => match self.generics.as_ref().and_then(|m| m.get(&self.ty)) {
                    Some(c) => write!(f, "{c}"),
                    None => write!(f, "'{id}"),
                },
                TyK::Array(ty) => write!(f, "(array {:#?})", self.with_ty(*ty)),
                TyK::Error => write!(f, "error"),
            }
        } else {
            match self.tyk {
                TyK::None => f.debug_struct("None").finish(),
                TyK::Boolean => f.debug_struct("Boolean").finish(),
                TyK::Char => f.debug_struct("Char").finish(),
                TyK::Number(n) => f.debug_tuple("Number").field(&n).finish(),
                TyK::String => f.debug_struct("String").finish(),
                TyK::Null => f.debug_struct("Null").finish(),
                TyK::Void => f.debug_struct("Void").finish(),
                TyK::Symbol => f.debug_struct("Symbol").finish(),
                TyK::Lambda {
                    params, rest, ret, ..
                } => f
                    .debug_struct("Lambda")
                    .field(
                        "params",
                        &params
                            .iter()
                            .map(|p| p.with_arena(self.arena))
                            .collect::<Vec<_>>(),
                    )
                    .field("rest", &rest.map(|rest| rest.with_arena(self.arena)))
                    .field("ret", &ret.with_arena(self.arena))
                    .finish(),
                TyK::Var(id) => f.debug_tuple("Var").field(&id).finish(),
                TyK::Array(ty) => f
                    .debug_tuple("Array")
                    .field(&ty.with_arena(self.arena))
                    .finish(),
                TyK::Error => f.debug_struct("Error").finish(),
            }
        }
    }
}
