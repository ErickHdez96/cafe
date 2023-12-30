use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Ty {
    /// Generic Scheme object.
    SObject,
    Lambda {
        params: Vec<Rc<Ty>>,
        rest: Option<Rc<Ty>>,
        ret: Rc<Ty>,
    },
    Char,
    Boolean,
    String,
    Pair(Rc<Ty>, Rc<Ty>),
    Null,
    Void,
    Symbol,
    Uninit,
    Error,
}

#[derive(Debug, Clone)]
pub struct BuiltinTys {
    pub boolean: Rc<Ty>,
    pub void: Rc<Ty>,
    pub uninit: Rc<Ty>,
}

impl Default for BuiltinTys {
    fn default() -> Self {
        Self {
            boolean: Rc::new(Ty::Boolean),
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
