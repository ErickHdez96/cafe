use std::rc::Rc;

use crate::{
    new_syntax::cst::{Cst, CstKind, Delim, ListKind},
    span::Span,
    utils::Atom,
};

use super::scopes::Scopes;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Syntax {
    pub cst: Rc<Cst>,
    pub scopes: Scopes,
}

impl Syntax {
    pub fn new(cst: Rc<Cst>) -> Self {
        Self {
            cst,
            scopes: Scopes::core(),
        }
    }

    pub fn span(&self) -> Span {
        self.cst.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SynList {
    pub value: Vec<Rc<Cst>>,
    pub kind: ListKind,
    pub span: Span,
    pub scopes: Scopes,
}

impl SynList {
    pub fn expected_close_char(&self) -> Delim {
        match self.value.first().map(|c| &c.kind) {
            Some(CstKind::Delim(d)) => d.close(),
            _ => panic!("expected list to start with delimiter"),
        }
    }

    pub fn close_delim_span(&self) -> Span {
        self.value.last().expect("at least one element").span
    }
}

impl From<SynList> for Syntax {
    fn from(value: SynList) -> Self {
        Self {
            cst: Rc::new(Cst::new(CstKind::List(value.value, value.kind), value.span)),
            scopes: value.scopes,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SynSymbol {
    pub value: Atom,
    pub span: Span,
    pub scopes: Scopes,
}

impl SynSymbol {
    pub fn new(value: Atom, span: Span, scopes: Scopes) -> Self {
        Self {
            value,
            span,
            scopes,
        }
    }
}

impl From<SynSymbol> for Syntax {
    fn from(value: SynSymbol) -> Self {
        Self {
            cst: Rc::new(Cst::new(CstKind::Ident(value.value), value.span)),
            scopes: value.scopes,
        }
    }
}
