use std::{fmt, rc::Rc};

use crate::{new_id, utils::Id};

new_id!(pub struct GenericId(usize));

#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub enum Ty {
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
        params: Vec<Rc<Ty>>,
        rest: Option<Rc<Ty>>,
        ret: Rc<Ty>,
    },
    Pair(Rc<Ty>, Rc<Ty>),
    Generic(GenericId),
    Uninit,
    Error,
}

impl Ty {
    pub fn is_boolean(&self) -> bool {
        matches!(self, Ty::Boolean)
    }
}

impl fmt::Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self {
                Self::None => write!(f, "none"),
                Self::SObject => write!(f, "object"),
                Self::Boolean => write!(f, "boolean"),
                Self::Char => write!(f, "char"),
                Self::Number(n) => n.fmt(f),
                Self::String => write!(f, "string"),
                Self::Null => write!(f, "null"),
                Self::Void => write!(f, "void"),
                Self::Symbol => write!(f, "symbol"),
                Self::Lambda { params, rest, ret } => write!(
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
                Self::Pair(car, cdr) => write!(f, "pair({car:?}, {cdr:?})"),
                Self::Generic(id) => write!(f, "'{id}"),
                Self::Uninit => write!(f, "uninit"),
                Self::Error => write!(f, "error"),
            }
        } else {
            match self {
                Self::None => write!(f, "None"),
                Self::SObject => write!(f, "SObject"),
                Self::Boolean => write!(f, "Boolean"),
                Self::Char => write!(f, "Char"),
                Self::Number(n) => n.fmt(f),
                Self::String => write!(f, "String"),
                Self::Null => write!(f, "Null"),
                Self::Void => write!(f, "Void"),
                Self::Symbol => write!(f, "Symbol"),
                Self::Lambda { params, rest, ret } => write!(
                    f,
                    "Lambda {{ params: {params:?}, rest: {rest:?}, ret: {ret:?}}}"
                ),
                Self::Pair(car, cdr) => write!(f, "Pair({car:?}, {cdr:?})"),
                Self::Generic(id) => write!(f, "Generic({id})"),
                Self::Uninit => write!(f, "Uninit"),
                Self::Error => write!(f, "Error"),
            }
        }
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

#[derive(Debug, Clone)]
pub struct BuiltinTys {
    pub object: Rc<Ty>,
    pub boolean: Rc<Ty>,
    pub char: Rc<Ty>,
    pub fixnum: Rc<Ty>,
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
            fixnum: Rc::new(Ty::Number(NumberTy::I64)),
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
        }
    }
}
