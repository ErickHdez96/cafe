pub mod green;
pub mod red;

use std::{fmt, iter, rc::Rc, vec};

pub use green::{GreenNodeBuilder, GreenTree};
pub use red::RedTree;

use crate::{
    expander::scopes::{Scope, Scopes},
    file::FileId,
    span::Span,
};

use super::{
    parser::{parse_char, Delim},
    SyntaxKind,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SynRoot {
    red: Rc<RedTree>,
    file_id: FileId,
}

impl SynRoot {
    pub fn new(green: &Rc<GreenTree>, file_id: FileId) -> Self {
        Self {
            red: RedTree::new(green),
            file_id,
        }
    }

    pub fn new_red(red: &Rc<RedTree>, file_id: FileId) -> Self {
        Self {
            red: Rc::clone(red),
            file_id,
        }
    }

    pub fn children(&self) -> Vec<Rc<RedTree>> {
        RedTree::children(&self.red)
    }

    pub fn syn_children(&self) -> impl iter::Iterator<Item = SynExp> + '_ {
        self.children()
            .into_iter()
            .filter_map(|c| SynExp::cast(&c, self.file_id))
    }

    pub fn file_id(&self) -> FileId {
        self.file_id
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SynExp {
    List(SynList),
    Symbol(SynSymbol),
    Boolean(SynBoolean),
    Char(SynChar),
}

impl SynExp {
    pub fn red(&self) -> &Rc<RedTree> {
        match self {
            SynExp::List(l) => l.red(),
            SynExp::Boolean(b) => b.red(),
            SynExp::Char(c) => c.red(),
            SynExp::Symbol(s) => s.red(),
        }
    }

    pub fn cast(red: &Rc<RedTree>, file_id: FileId) -> Option<Self> {
        match red.kind() {
            SyntaxKind::List => {
                let mut children = vec![];
                let mut found_dot = false;
                let mut dot = None;
                for c in RedTree::children(red) {
                    match c.kind() {
                        SyntaxKind::Dot => {
                            found_dot = true;
                        }
                        _ => match SynExp::cast(&c, file_id) {
                            Some(sexp) if found_dot => {
                                dot = Some(Box::new(sexp));
                                break;
                            }
                            Some(sexp) => {
                                children.push(sexp);
                            }
                            None => {}
                        },
                    }
                }
                Some(Self::List(SynList {
                    red: Rc::clone(red),
                    sexps: children,
                    dot,
                    file_id,
                }))
            }
            SyntaxKind::Atom => RedTree::children(red)
                .iter()
                .find_map(|red| match red.kind() {
                    SyntaxKind::True | SyntaxKind::False => {
                        Some(Self::Boolean(SynBoolean::raw(red, file_id)))
                    }
                    SyntaxKind::Char => Some(Self::Char(SynChar::raw(red, file_id))),
                    SyntaxKind::Identifier => {
                        Some(Self::Symbol(SynSymbol::raw(red, Scopes::core(), file_id)))
                    }
                    _ => None,
                }),
            _ => None,
        }
    }

    pub fn list(&self) -> Option<&SynList> {
        match self {
            SynExp::List(l) => Some(l),
            SynExp::Boolean(_) | SynExp::Char(_) | SynExp::Symbol(_) => None,
        }
    }

    pub fn boolean(&self) -> Option<&SynBoolean> {
        match self {
            SynExp::Boolean(b) => Some(b),
            SynExp::List(_) | SynExp::Char(_) | SynExp::Symbol(_) => None,
        }
    }

    pub fn char(&self) -> Option<&SynChar> {
        match self {
            SynExp::Char(c) => Some(c),
            SynExp::Boolean(_) | SynExp::List(_) | SynExp::Symbol(_) => None,
        }
    }

    pub fn symbol(&self) -> Option<&SynSymbol> {
        match self {
            SynExp::Symbol(s) => Some(s),
            SynExp::Char(_) | SynExp::Boolean(_) | SynExp::List(_) => None,
        }
    }

    pub fn syn_string(&self) -> String {
        match self {
            SynExp::List(l) => l.syn_string(),
            SynExp::Boolean(b) => b.to_string(),
            SynExp::Char(c) => c.to_string(),
            SynExp::Symbol(s) => s.to_string(),
        }
    }

    pub fn file_id(&self) -> FileId {
        match self {
            SynExp::List(l) => l.file_id(),
            SynExp::Boolean(b) => b.file_id(),
            SynExp::Char(c) => c.file_id(),
            SynExp::Symbol(s) => s.file_id(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            SynExp::List(l) => l.span(),
            SynExp::Boolean(b) => b.span(),
            SynExp::Char(c) => c.span(),
            SynExp::Symbol(s) => s.span(),
        }
    }

    #[must_use]
    pub fn with_scope(&self, scope: Scope) -> Self {
        match self {
            SynExp::List(l) => SynExp::List(l.with_scope(scope)),
            SynExp::Symbol(s) => SynExp::Symbol(s.with_scope(scope)),
            SynExp::Boolean(_) | SynExp::Char(_) => self.clone(),
        }
    }

    pub fn flip_scope(&mut self, scope: Scope) {
        match self {
            SynExp::List(l) => l.flip_scope(scope),
            SynExp::Symbol(s) => s.flip_scope(scope),
            SynExp::Boolean(_) | SynExp::Char(_) => {}
        }
    }
}

impl fmt::Display for SynExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            SynExp::List(l) => l.fmt(f),
            SynExp::Boolean(b) => b.fmt(f),
            SynExp::Char(c) => c.fmt(f),
            SynExp::Symbol(s) => s.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SynList {
    red: Rc<RedTree>,
    pub(crate) sexps: Vec<SynExp>,
    pub(crate) dot: Option<Box<SynExp>>,
    file_id: FileId,
}

impl SynList {
    pub fn raw(
        red: &Rc<RedTree>,
        sexps: Vec<SynExp>,
        dot: Option<Box<SynExp>>,
        file_id: FileId,
    ) -> Self {
        Self {
            red: Rc::clone(red),
            sexps,
            dot,
            file_id,
        }
    }

    pub fn into_parts(self) -> (Vec<SynExp>, Option<Box<SynExp>>) {
        (self.sexps, self.dot)
    }

    pub fn sexps(&self) -> &[SynExp] {
        &self.sexps
    }

    pub fn dot(&self) -> Option<&SynExp> {
        self.dot.as_deref()
    }

    pub fn sexps_mut(&mut self) -> &mut [SynExp] {
        &mut self.sexps
    }

    pub const fn red(&self) -> &Rc<RedTree> {
        &self.red
    }

    pub fn syn_string(&self) -> String {
        let mut out = vec![String::from("(")];
        let len = self.sexps.len();
        for (i, s) in self.sexps.iter().enumerate() {
            out.push(s.syn_string());
            if i + 1 < len {
                out.push(String::from(" "));
            }
        }
        out.push(String::from(")"));
        out.join("")
    }

    pub const fn file_id(&self) -> FileId {
        self.file_id
    }

    pub fn span(&self) -> Span {
        Span::new(
            self.file_id(),
            self.red.offset().try_into().unwrap(),
            self.red.green().text_length().try_into().unwrap(),
        )
    }

    pub fn close_delim_char(&self) -> char {
        match self.red.green().as_ref() {
            GreenTree::Node(n) => match n.children().iter().find_map(|c| match c.as_ref() {
                GreenTree::Token(tok)
                    if tok.kind() == SyntaxKind::OpenDelim
                        || tok.kind() == SyntaxKind::SpecialOpenDelim =>
                {
                    Some(tok.text())
                }
                _ => None,
            }) {
                Some(d) => Delim::from(d).close(),
                None => panic!("expected an open delimiter child"),
            },
            GreenTree::Token(_) => panic!("expected a node"),
        }
    }

    pub fn close_delim_span(&self) -> Span {
        let children = RedTree::children(&self.red);
        let red = children
            .last()
            .expect("list must have at least one element");
        Span::new(
            self.file_id(),
            red.offset().try_into().unwrap(),
            red.green().text_length().try_into().unwrap(),
        )
    }

    #[must_use]
    pub fn with_scope(&self, scope: Scope) -> Self {
        SynList::raw(
            &self.red,
            self.sexps.iter().map(|e| e.with_scope(scope)).collect(),
            self.dot.as_ref().map(|d| Box::new(d.with_scope(scope))),
            self.file_id,
        )
    }

    pub fn flip_scope(&mut self, scope: Scope) {
        for s in &mut self.sexps {
            s.flip_scope(scope);
        }
        if let Some(dot) = &mut self.dot {
            dot.flip_scope(scope);
        }
    }
}

impl From<SynList> for SynExp {
    fn from(value: SynList) -> Self {
        Self::List(value)
    }
}

impl fmt::Display for SynList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.red.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SynBoolean {
    red: Rc<RedTree>,
    file_id: FileId,
}

impl SynBoolean {
    pub fn raw(red: &Rc<RedTree>, file_id: FileId) -> Self {
        debug_assert!(matches!(red.kind(), SyntaxKind::True | SyntaxKind::False));
        Self {
            red: Rc::clone(red),
            file_id,
        }
    }

    pub fn cast(red: &Rc<RedTree>, file_id: FileId) -> Option<Self> {
        match red.kind() {
            SyntaxKind::True | SyntaxKind::False => Some(Self {
                red: Rc::clone(red),
                file_id,
            }),
            _ => None,
        }
    }

    pub fn value(&self) -> bool {
        self.red().kind() == SyntaxKind::True
    }

    pub const fn red(&self) -> &Rc<RedTree> {
        &self.red
    }

    pub const fn file_id(&self) -> FileId {
        self.file_id
    }

    pub fn span(&self) -> Span {
        Span::new(
            self.file_id(),
            self.red.offset().try_into().unwrap(),
            self.red.green().text_length().try_into().unwrap(),
        )
    }
}

impl From<SynBoolean> for SynExp {
    fn from(value: SynBoolean) -> Self {
        Self::Boolean(value)
    }
}

impl fmt::Display for SynBoolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.red.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SynChar {
    red: Rc<RedTree>,
    file_id: FileId,
}

impl SynChar {
    pub fn raw(red: &Rc<RedTree>, file_id: FileId) -> Self {
        debug_assert!(matches!(red.kind(), SyntaxKind::Char));
        Self {
            red: Rc::clone(red),
            file_id,
        }
    }

    pub fn cast(red: &Rc<RedTree>, file_id: FileId) -> Option<Self> {
        match red.kind() {
            SyntaxKind::Char => Some(Self {
                red: Rc::clone(red),
                file_id,
            }),
            _ => None,
        }
    }

    pub fn value(&self) -> char {
        parse_char(&self.red().green().to_string())
    }

    pub const fn red(&self) -> &Rc<RedTree> {
        &self.red
    }

    pub const fn file_id(&self) -> FileId {
        self.file_id
    }

    pub fn span(&self) -> Span {
        Span::new(
            self.file_id(),
            self.red.offset().try_into().unwrap(),
            self.red.green().text_length().try_into().unwrap(),
        )
    }
}

impl From<SynChar> for SynExp {
    fn from(value: SynChar) -> Self {
        Self::Char(value)
    }
}

impl fmt::Display for SynChar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.red.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SynSymbol {
    red: Rc<RedTree>,
    scopes: Scopes,
    file_id: FileId,
}

impl SynSymbol {
    pub fn raw(red: &Rc<RedTree>, scopes: Scopes, file_id: FileId) -> Self {
        debug_assert_eq!(red.kind(), SyntaxKind::Identifier);
        Self {
            red: Rc::clone(red),
            scopes,
            file_id,
        }
    }

    pub const fn scopes(&self) -> &Scopes {
        &self.scopes
    }

    pub fn scopes_mut(&mut self) -> &mut Scopes {
        &mut self.scopes
    }

    pub const fn red(&self) -> &Rc<RedTree> {
        &self.red
    }

    pub fn value(&self) -> &str {
        match self.red().green().as_ref() {
            GreenTree::Node(_) => unreachable!(),
            GreenTree::Token(t) => t.text(),
        }
    }

    pub fn cast(red: &Rc<RedTree>, file_id: FileId) -> Option<Self> {
        match red.kind() {
            SyntaxKind::Identifier => Some(Self {
                red: Rc::clone(red),
                scopes: Scopes::core(),
                file_id,
            }),
            _ => None,
        }
    }

    pub const fn file_id(&self) -> FileId {
        self.file_id
    }

    pub fn span(&self) -> Span {
        Span::new(
            self.file_id(),
            self.red.offset().try_into().unwrap(),
            self.red.green().text_length().try_into().unwrap(),
        )
    }

    #[must_use]
    pub fn with_scope(&self, scope: Scope) -> Self {
        Self {
            red: Rc::clone(&self.red),
            scopes: self.scopes.with(scope),
            file_id: self.file_id,
        }
    }

    pub fn flip_scope(&mut self, scope: Scope) {
        self.scopes.flip(scope);
    }
}

impl From<SynSymbol> for SynExp {
    fn from(value: SynSymbol) -> Self {
        Self::Symbol(value)
    }
}

impl fmt::Display for SynSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.red.fmt(f)
    }
}
