use std::fmt;

use crate::{
    arena::{self, Arena},
    new_id,
    utils::Id,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty(arena::Id<TyK>);

impl Ty {
    pub const fn value(self) -> arena::Id<TyK> {
        self.0
    }

    pub fn display(self, arena: &Arena<TyK>) -> TyDisplay {
        TyDisplay { ty: self, arena }
    }
}

impl From<arena::Id<TyK>> for Ty {
    fn from(value: arena::Id<TyK>) -> Self {
        Self(value)
    }
}

new_id!(pub struct GenericId(usize));

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum TyK {
    #[default]
    None,
    /// Generic Scheme object.
    SObject,
    Boolean,
    Char,
    Number(NumberTy),
    String,
    Null,
    Void,
    Symbol,
    Lambda {
        params: Vec<Ty>,
        rest: Option<Ty>,
        ret: Ty,
    },
    Pair(Ty, Ty),
    Generic(GenericId),
    Uninit,
    Error,
}

impl TyK {
    pub fn is_boolean(&self) -> bool {
        matches!(self, TyK::Boolean)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
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

/// Type Constraint
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyCo {
    Ty(Ty),
}

impl TyCo {
    pub fn from_ty(ty: Ty) -> Self {
        Self::Ty(ty)
    }

    pub fn new_generic(arena: &mut Arena<TyK>) -> TyCo {
        Self::Ty(arena.alloc(TyK::Generic(GenericId::new())).into())
    }
}

impl From<TyCo> for Ty {
    fn from(value: TyCo) -> Self {
        match value {
            TyCo::Ty(ty) => ty,
        }
    }
}

pub struct TyDisplay<'tyc> {
    pub ty: Ty,
    pub arena: &'tyc Arena<TyK>,
}

impl fmt::Debug for TyDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self.arena.get(self.ty.0) {
                TyK::None => write!(f, "none"),
                TyK::SObject => write!(f, "object"),
                TyK::Boolean => write!(f, "boolean"),
                TyK::Char => write!(f, "char"),
                TyK::Number(n) => n.fmt(f),
                TyK::String => write!(f, "string"),
                TyK::Null => write!(f, "null"),
                TyK::Void => write!(f, "void"),
                TyK::Symbol => write!(f, "symbol"),
                TyK::Lambda { params, rest, ret } => write!(
                    f,
                    "(-> {}{} {:#?})",
                    params
                        .iter()
                        .map(|p| format!("{:#?}", p.display(self.arena)))
                        .collect::<Vec<_>>()
                        .join(" "),
                    match rest {
                        Some(r) => format!(" ({:#?} ...)", r.display(self.arena)),
                        None => String::new(),
                    },
                    ret.display(self.arena),
                ),
                TyK::Pair(car, cdr) => write!(
                    f,
                    "pair({:?}, {:?})",
                    car.display(self.arena),
                    cdr.display(self.arena)
                ),
                TyK::Generic(id) => write!(f, "'{id}"),
                TyK::Uninit => write!(f, "uninit"),
                TyK::Error => write!(f, "error"),
            }
        } else {
            match self.arena.get(self.ty.0) {
                TyK::None => write!(f, "None"),
                TyK::SObject => write!(f, "SObject"),
                TyK::Boolean => write!(f, "Boolean"),
                TyK::Char => write!(f, "Char"),
                TyK::Number(n) => n.fmt(f),
                TyK::String => write!(f, "String"),
                TyK::Null => write!(f, "Null"),
                TyK::Void => write!(f, "Void"),
                TyK::Symbol => write!(f, "Symbol"),
                TyK::Lambda { params, rest, ret } => write!(
                    f,
                    "Lambda {{ params: {params:?}, rest: {rest:?}, ret: {ret:?}}}",
                ),
                TyK::Pair(car, cdr) => write!(
                    f,
                    "Pair({:?}, {:?})",
                    car.display(self.arena),
                    cdr.display(self.arena)
                ),
                TyK::Generic(id) => write!(f, "Generic({id})"),
                TyK::Uninit => write!(f, "Uninit"),
                TyK::Error => write!(f, "Error"),
            }
        }
    }
}
