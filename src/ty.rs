use std::rc::Rc;

#[derive(Debug, PartialEq, Eq)]
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
}
