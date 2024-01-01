use std::{fmt, rc::Rc};

use crate::{new_id, utils::Id};

new_id!(pub struct GenericId(usize));

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    /// Generic Scheme object.
    SObject,
    Boolean,
    Char,
    String,
    Null,
    Void,
    Symbol,
    Union(Vec<Rc<Ty>>),
    Except(Vec<Rc<Ty>>),
    Lambda {
        params: Vec<Rc<Ty>>,
        rest: Option<Rc<Ty>>,
        ret: Rc<Ty>,
    },
    Pair(Rc<Ty>, Rc<Ty>),
    Uninit,
    Error,
    Generic(GenericId),
}

impl fmt::Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self {
                Ty::SObject => write!(f, "object"),
                Ty::Boolean => write!(f, "boolean"),
                Ty::Char => write!(f, "char"),
                Ty::String => write!(f, "string"),
                Ty::Null => write!(f, "null"),
                Ty::Void => write!(f, "void"),
                Ty::Symbol => write!(f, "symbol"),
                Ty::Union(v) => write!(f, "union({v:?})"),
                Ty::Except(v) => write!(f, "except({v:?})"),
                Ty::Lambda { params, rest, ret } => write!(
                    f,
                    "(-> {}{} {ret:#?})",
                    params
                        .iter()
                        .map(|p| format!("{p:#?}"))
                        .collect::<Vec<_>>()
                        .join(" "),
                    match rest {
                        Some(r) => format!(" ({r:#?} ...)"),
                        None => String::new(),
                    }
                ),
                Ty::Pair(car, cdr) => write!(f, "pair({car:?}, {cdr:?})"),
                Ty::Uninit => write!(f, "uninit"),
                Ty::Error => write!(f, "error"),
                Ty::Generic(id) => write!(f, "'{id}"),
            }
        } else {
            match self {
                Ty::SObject => write!(f, "SObject"),
                Ty::Boolean => write!(f, "Boolean"),
                Ty::Char => write!(f, "Char"),
                Ty::String => write!(f, "String"),
                Ty::Null => write!(f, "Null"),
                Ty::Void => write!(f, "Void"),
                Ty::Symbol => write!(f, "Symbol"),
                Ty::Union(v) => write!(f, "Union({v:?})"),
                Ty::Except(v) => write!(f, "Except({v:?})"),
                Ty::Lambda { params, rest, ret } => write!(
                    f,
                    "Lambda {{ params: {params:?}, rest: {rest:?}, ret: {ret:?}}}"
                ),
                Ty::Pair(car, cdr) => write!(f, "Pair({car:?}, {cdr:?})"),
                Ty::Uninit => write!(f, "Uninit"),
                Ty::Error => write!(f, "Error"),
                Ty::Generic(id) => write!(f, "Generic({id})"),
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct BuiltinTys {
    pub object: Rc<Ty>,
    pub boolean: Rc<Ty>,
    pub char: Rc<Ty>,
    pub string: Rc<Ty>,
    pub null: Rc<Ty>,
    pub void: Rc<Ty>,
    pub uninit: Rc<Ty>,
}

impl Default for BuiltinTys {
    fn default() -> Self {
        Self {
            object: Rc::new(Ty::SObject),
            boolean: Rc::new(Ty::Boolean),
            char: Rc::new(Ty::Char),
            string: Rc::new(Ty::String),
            null: Rc::new(Ty::Null),
            void: Rc::new(Ty::Void),
            uninit: Rc::new(Ty::Uninit),
        }
    }
}

impl BuiltinTys {
    pub fn new() -> Self {
        Self::default()
    }
}

/// Type Constraint
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyCo {
    Ty(Rc<Ty>),
    /// The variable can be any of these types.
    Union(Vec<Rc<Ty>>),
    /// The variable is a generic scheme object, except any of these types.
    Except(Vec<Rc<Ty>>),
}

impl TyCo {
    pub fn from_ty(ty: &Rc<Ty>) -> Self {
        Self::Ty(Rc::clone(ty))
    }

    pub fn new_generic() -> TyCo {
        Self::Ty(Rc::new(Ty::Generic(GenericId::new())))
    }
}

impl From<TyCo> for Rc<Ty> {
    fn from(value: TyCo) -> Self {
        match value {
            TyCo::Ty(ty) => ty,
            TyCo::Union(mut v) if v.len() == 1 => v.remove(0),
            TyCo::Union(v) => Rc::new(Ty::Union(v)),
            TyCo::Except(v) => Rc::new(Ty::Except(v)),
        }
    }
}
