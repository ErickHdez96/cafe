use std::{fmt, rc::Rc};

use crate::{
    env::Env,
    expander::binding::Binding,
    new_id,
    span::Span,
    ty::Ty,
    utils::{Intern, Resolve},
};

use super::parser::Number;

const INDENTATION_WIDTH: usize = 2;

new_id!(pub struct ModId(u32), ModuleName, modules);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub id: ModId,
    pub span: Span,
    pub dependencies: Vec<ModId>,
    /// Bindings exported from the Module.
    pub exports: Env<'static, String, Binding>,
    /// All the root bindings (e.g. macro, value) of the module.
    pub bindings: Env<'static, String, Binding>,
    pub types: Option<Env<'static, String, Rc<Ty>>>,
    pub body: Expr,
}

impl Module {
    /// Returns the interface defining the module.
    pub fn to_interface(&self) -> ModuleInterface {
        ModuleInterface {
            span: self.span,
            id: self.id,
            bindings: self.exports.clone(),
            dependencies: self.dependencies.clone(),
            types: None,
        }
    }
}

/// A [`ModuleInterface`] is the outer view of a [`Module`] as seen from another [`Module`]. It
/// contains its location, name and its list of exported bindings.
#[derive(Debug, Clone, Hash)]
pub struct ModuleInterface {
    pub id: ModId,
    pub span: Span,
    // TODO: bring Binding here
    /// Exported bindings.
    pub bindings: Env<'static, String, Binding>,
    pub types: Option<Env<'static, String, Rc<Ty>>>,
    pub dependencies: Vec<ModId>,
}

/// A [`ModuleName`] comprises of its individual path components (e.g. rnrs io simple) which must be
/// valid identifiers, and its optional version (e.g. 1 0 1).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName {
    pub paths: Vec<String>,
    pub versions: Vec<usize>,
}

impl ModuleName {
    /// Returns a special [`ModuleName`] identifying the root of the module tree when compiling a
    /// project.
    pub fn script() -> ModId {
        Self {
            paths: vec![String::from("#script")],
            versions: vec![],
        }
        .intern()
    }

    pub fn from_strings(strs: Vec<impl Into<String>>) -> ModId {
        Self {
            paths: strs.into_iter().map(|s| s.into()).collect(),
            versions: vec![],
        }
        .intern()
    }
}

impl fmt::Display for ModuleName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({}{}({}))",
            self.paths.join(" "),
            if self.paths.is_empty() { "" } else { " " },
            self.versions
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub span: Span,
    pub value: String,
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let padding = " ".repeat(f.width().unwrap_or_default());
            write!(f, "{padding}{{|{}| {}}}", self.value, self.span)
        } else {
            f.debug_struct("Ident")
                .field("span", &self.span)
                .field("value", &self.value)
                .finish()
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub span: Span,
    pub module: ModId,
    pub value: String,
}

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let padding = " ".repeat(f.width().unwrap_or_default());
            write!(
                f,
                "{padding}{{|{}| {} {}}}",
                self.value,
                self.module.resolve(),
                self.span
            )
        } else {
            f.debug_struct("Ident")
                .field("span", &self.span)
                .field("value", &self.value)
                .finish()
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Item {
    Import(ModId, Span),
    Mod(ModId, Span),
    Define(Define),
    Expr(Expr),
}

impl Item {
    pub fn span(&self) -> Span {
        match self {
            Item::Import(_, span) | Item::Mod(_, span) => *span,
            Item::Define(d) => d.span,
            Item::Expr(e) => e.span,
        }
    }

    pub fn into_expr(self) -> Option<Expr> {
        match self {
            Item::Expr(e) => Some(e),
            _ => None,
        }
    }

    pub fn into_define(self) -> Option<Define> {
        match self {
            Item::Define(d) => Some(d),
            _ => None,
        }
    }
}

impl From<Define> for Item {
    fn from(value: Define) -> Self {
        Self::Define(value)
    }
}

impl From<Expr> for Item {
    fn from(value: Expr) -> Self {
        Self::Expr(value)
    }
}

impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self {
                Item::Import(mid, span) => {
                    let width = f.width().unwrap_or_default();
                    let padding = " ".repeat(width);
                    write!(f, "{padding}{{import {}@{span}}}", mid.resolve(),)
                }
                Item::Mod(mid, span) => {
                    let width = f.width().unwrap_or_default();
                    let padding = " ".repeat(width);
                    write!(f, "{padding}{{module {}@{span}}}", mid.resolve(),)
                }
                Item::Define(d) => d.fmt(f),
                Item::Expr(e) => e.fmt(f),
            }
        } else {
            match self {
                Item::Import(mid, span) => f
                    .debug_tuple("DefExpr::Import")
                    .field(&mid)
                    .field(&span)
                    .finish(),
                Item::Mod(mid, span) => f
                    .debug_tuple("DefExpr::Mod")
                    .field(&mid)
                    .field(&span)
                    .finish(),
                Item::Define(d) => f.debug_tuple("DefExpr::Define").field(&d).finish(),
                Item::Expr(e) => f.debug_tuple("DefExpr::Expr").field(&e).finish(),
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Define {
    pub span: Span,
    pub name: Ident,
    pub expr: Option<Box<Expr>>,
}

impl fmt::Debug for Define {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let padding = " ".repeat(width);
            write!(
                f,
                "{padding}{{define@{}\n{:#width$?}{}}}",
                self.span,
                self.name,
                if let Some(e) = &self.expr {
                    format!("\n{e:#width$?}", width = width + INDENTATION_WIDTH)
                } else {
                    String::new()
                },
                width = width + INDENTATION_WIDTH,
            )
        } else {
            f.debug_struct("Define")
                .field("span", &self.span)
                .field("name", &self.name)
                .field("expr", &self.expr)
                .finish()
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    Body(Vec<Item>),
    Let {
        kind: LetKind,
        defs: Vec<Define>,
        exprs: Vec<Expr>,
    },
    Quote(Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Lambda {
        formals: Vec<Ident>,
        rest: Option<Ident>,
        expr: Box<Expr>,
    },
    List(Vec<Expr>),
    DottedList(Vec<Expr>, Box<Expr>),
    Boolean(bool),
    Char(char),
    Number(Number),
    Var(Path),
    Error(Box<Item>),
    Void,
    Begin(Vec<Expr>),
}

impl Expr {
    #[must_use]
    pub fn into_error(self) -> Self {
        Self {
            span: self.span,
            kind: ExprKind::Error(Box::new(Item::Expr(self))),
        }
    }

    #[must_use]
    pub fn into_quote(self) -> Self {
        Self {
            span: self.span,
            kind: ExprKind::Quote(Box::new(self)),
        }
    }

    pub fn dummy() -> Expr {
        Self {
            span: Span::dummy(),
            kind: ExprKind::Void,
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let indentation = " ".repeat(width);
            let span = self.span;

            match &self.kind {
                ExprKind::Body(items) => write!(
                    f,
                    "{indentation}{{body {span}\n{}}}",
                    items
                        .iter()
                        .map(|e| format!("{e:#width$?}", width = width + INDENTATION_WIDTH))
                        .collect::<Vec<_>>()
                        .join("\n"),
                ),
                ExprKind::List(l) if l.is_empty() => {
                    write!(f, "{indentation}{{() {span}}}")
                }
                ExprKind::List(l) => write!(
                    f,
                    "{indentation}{{list {span}\n{}}}",
                    l.iter()
                        .map(|e| format!("{e:#width$?}", width = width + INDENTATION_WIDTH))
                        .collect::<Vec<_>>()
                        .join("\n"),
                ),
                ExprKind::DottedList(l, dot) => write!(
                    f,
                    "{indentation}{{dotted-list {span}\n{}\n{}.\n{dot:#width$?}}}",
                    l.iter()
                        .map(|e| format!("{e:#width$?}", width = width + INDENTATION_WIDTH))
                        .collect::<Vec<_>>()
                        .join("\n"),
                    " ".repeat(width + INDENTATION_WIDTH),
                    width = width + INDENTATION_WIDTH
                ),
                ExprKind::Boolean(b) => {
                    write!(
                        f,
                        "{indentation}{{{} {span}}}",
                        if *b { "#t" } else { "#f" },
                    )
                }
                ExprKind::Char(c) => {
                    write!(
                        f,
                        "{indentation}{{#\\{} {span}}}",
                        if c.is_alphanumeric() {
                            format!("{c}")
                        } else {
                            format!("x{:X}", u32::from(*c))
                        },
                    )
                }
                ExprKind::Number(n) => {
                    write!(
                        f,
                        "{indentation}{{{} {span}}}",
                        match n {
                            Number::Fixnum(n) => format!("{n}"),
                        },
                    )
                }
                ExprKind::Var(path) => {
                    write!(
                        f,
                        "{indentation}{{var |{}| {} {span}}}",
                        path.value,
                        path.module.resolve(),
                    )
                }
                ExprKind::Lambda {
                    formals,
                    rest,
                    expr,
                } => write!(
                    f,
                    "{indentation}{{λ {span}\n{}({})\n{}{}\n{expr:#width$?}}}",
                    " ".repeat(width + INDENTATION_WIDTH),
                    formals
                        .iter()
                        .map(|f| format!("{f:#?}"))
                        .collect::<Vec<_>>()
                        .join(" "),
                    " ".repeat(width + INDENTATION_WIDTH),
                    match rest {
                        Some(rest) => format!("{rest:#?}"),
                        None => String::from("#f"),
                    },
                    width = width + INDENTATION_WIDTH
                ),
                ExprKind::Error(e) => {
                    write!(
                        f,
                        "{indentation}{{error {span}\n{e:#width$?}}}",
                        width = width + INDENTATION_WIDTH
                    )
                }
                ExprKind::Quote(e) => {
                    write!(
                        f,
                        "{indentation}{{quote {span}\n{e:#width$?}}}",
                        width = width + INDENTATION_WIDTH
                    )
                }
                ExprKind::If(cond, tru, fls) => write!(
                    f,
                    "{indentation}{{if {span}\n{cond:#width$?}\n{tru:#width$?}\n{fls:#width$?}}}",
                    width = width + INDENTATION_WIDTH,
                ),
                ExprKind::Void => write!(f, "{indentation}{{void {span}}}"),
                ExprKind::Let { kind, defs, exprs } => write!(
                    f,
                    "{indentation}{{{kind} {span}\n{}\n{}}}",
                    if defs.is_empty() {
                        format!("{}()", " ".repeat(width + INDENTATION_WIDTH))
                    } else {
                        defs.iter()
                            .map(|d| format!("{d:#width$?}", width = width + INDENTATION_WIDTH))
                            .collect::<Vec<_>>()
                            .join("\n")
                    },
                    if exprs.is_empty() {
                        panic!("must always have at least one expression")
                    } else {
                        exprs
                            .iter()
                            .map(|d| format!("{d:#width$?}", width = width + INDENTATION_WIDTH))
                            .collect::<Vec<_>>()
                            .join("\n")
                    },
                ),
                ExprKind::Begin(exprs) => write!(
                    f,
                    "{indentation}{{begin\n{}}}",
                    exprs
                        .iter()
                        .map(|e| format!("{e:#width$?}", width = width + INDENTATION_WIDTH))
                        .collect::<Vec<_>>()
                        .join("\n"),
                ),
            }
        } else {
            f.debug_struct("Expr")
                .field("span", &self.span)
                .field("kind", &self.kind)
                .finish()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LetKind {
    LetRec,
}

impl fmt::Display for LetKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LetKind::LetRec => "letrec",
            }
        )
    }
}
