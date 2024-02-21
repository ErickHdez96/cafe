use std::rc::Rc;

use crate::{
    new_syntax::cst::{Cst, CstKind, Delim, ListKind},
    span::Span,
    utils::Atom,
};

use super::scopes::{Scope, Scopes};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Syntax {
    Raw(Rc<Cst>),
    List(SynList),
    Symbol(SynSymbol),
}

impl Syntax {
    pub fn source(&self) -> &Cst {
        match self {
            Syntax::Raw(r) => r,
            Syntax::List(l) => &l.source,
            Syntax::Symbol(s) => &s.source,
        }
    }

    pub fn kind(&self) -> &CstKind {
        &self.source().kind
    }

    pub fn span(&self) -> Span {
        match self {
            Syntax::Raw(c) => c.span,
            Syntax::List(l) => l.source.span,
            Syntax::Symbol(s) => s.source.span,
        }
    }

    pub fn with_scope(self, scope: Scope) -> Self {
        match self {
            Syntax::Raw(_) => self,
            Syntax::List(l) => l.with_scope(scope).into(),
            Syntax::Symbol(s) => s.with_scope(scope).into(),
        }
    }

    pub fn list(&self) -> Option<&SynList> {
        match self {
            Syntax::List(l) => Some(&l),
            _ => None,
        }
    }
}

impl From<Rc<Cst>> for Syntax {
    fn from(value: Rc<Cst>) -> Self {
        match &value.kind {
            CstKind::List(l, _) => Self::List(SynList {
                value: l.iter().cloned().map(|s| s.into()).collect(),
                source: value,
            }),
            CstKind::Ident(_) => Syntax::Symbol(SynSymbol {
                source: value,
                scopes: Scopes::core(),
            }),
            _ => Self::Raw(value),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SynList {
    pub value: Vec<Syntax>,
    pub source: Rc<Cst>,
}

impl SynList {
    pub fn expected_close_char(&self) -> Delim {
        match self.value().0.first().map(|c| &c.kind) {
            Some(CstKind::Delim(d)) => d.close(),
            _ => panic!("expected list to start with delimiter"),
        }
    }

    pub fn close_delim_span(&self) -> Span {
        self.value.last().expect("at least one element").span()
    }

    pub fn span(&self) -> Span {
        self.source.span
    }

    pub fn value(&self) -> (&[Rc<Cst>], ListKind) {
        match &self.source.kind {
            CstKind::List(l, k) => (&l, *k),
            _ => unreachable!(),
        }
    }

    pub fn with_scope(mut self, scope: Scope) -> Self {
        self.value = self
            .value
            .into_iter()
            .map(|syn| syn.with_scope(scope))
            .collect();
        self
    }
}

impl From<SynList> for Syntax {
    fn from(value: SynList) -> Self {
        Self::List(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SynSymbol {
    pub source: Rc<Cst>,
    pub scopes: Scopes,
}

impl SynSymbol {
    pub fn new(source: Rc<Cst>, scopes: Scopes) -> Self {
        Self { source, scopes }
    }

    pub fn value(&self) -> &Atom {
        match &self.source.kind {
            CstKind::Ident(i) => i,
            _ => unreachable!(),
        }
    }

    pub fn span(&self) -> Span {
        self.source.span
    }

    pub fn with_scope(mut self, scope: Scope) -> Self {
        self.scopes.add(scope);
        self
    }
}

impl From<SynSymbol> for Syntax {
    fn from(value: SynSymbol) -> Self {
        Self::Symbol(value)
    }
}
