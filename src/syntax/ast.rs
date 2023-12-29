use std::fmt;

use crate::{
    env::Env,
    expander::Binding,
    interner::Store,
    new_id,
    span::Span,
    utils::{Intern, Resolve},
};

const INDENTATION_WIDTH: usize = 2;

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

/// A [`ModuleInterface`] is the outer view of a [`Module`] as seen from another [`Module`]. It
/// contains its location, name and its list of exported bindings.
#[derive(Debug, Clone, Hash)]
pub struct ModuleInterface {
    pub id: ModId,
    pub span: Span,
    // TODO: bring Binding here
    /// Exported bindings.
    pub bindings: Env<'static, String, Binding>,
    pub dependencies: Vec<ModId>,
}

new_id!(pub struct ModId(u32), ModuleName, modules);

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub id: ModId,
    pub span: Span,
    pub dependencies: Vec<ModId>,
    /// Bindings exported from the Module.
    pub exports: Env<'static, String, Binding>,
    /// All the root bindings (e.g. macro, value) of the module.
    pub bindings: Env<'static, String, Binding>,
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
        }
    }
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let padding = " ".repeat(width);
            write!(
                f,
                "{padding}mod {} @{}\n{:#width$?}",
                self.id.resolve(),
                self.span,
                self.body,
                width = width + INDENTATION_WIDTH
            )
        } else {
            f.debug_struct("Module")
                .field("span", &self.span)
                .field("body", &self.body)
                .finish()
        }
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum NameId {
    Global(DefId),
    Local(ItemId),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ItemId {
    owner: LocalDefId,
    id: LocalItemId,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LocalItemId(u32);

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Item {
    Define(Define),
    Expr(Expr),
}

impl Item {
    pub fn span(&self) -> Span {
        match self {
            Item::Define(d) => d.span,
            Item::Expr(e) => e.span,
        }
    }
}

impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self {
                Item::Define(d) => d.fmt(f),
                Item::Expr(e) => e.fmt(f),
            }
        } else {
            match self {
                Item::Define(d) => f.debug_tuple("DefExpr::Define").field(&d).finish(),
                Item::Expr(e) => f.debug_tuple("DefExpr::Expr").field(&e).finish(),
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct DefId {
    mod_id: ModId,
    local_def_id: LocalDefId,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LocalDefId(u32);

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Define {
    pub span: Span,
    pub name: Ident,
    pub expr: Option<Expr>,
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
    LetRec {
        defs: Vec<Define>,
        exprs: Vec<Expr>,
    },
    Quote(Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Lambda {
        formals: Vec<Ident>,
        rest: Option<Ident>,
        exprs: Vec<Expr>,
    },
    List(Vec<Expr>),
    DottedList(Vec<Expr>, Box<Expr>),
    Boolean(bool),
    Char(char),
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
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let indentation = " ".repeat(width);

            match &self.kind {
                ExprKind::List(l) if l.is_empty() => {
                    write!(f, "{indentation}{{() {}}}", self.span,)
                }
                ExprKind::List(l) => write!(
                    f,
                    "{indentation}{{list {}\n{}}}",
                    self.span,
                    l.iter()
                        .map(|e| format!("{e:#width$?}", width = width + INDENTATION_WIDTH))
                        .collect::<Vec<_>>()
                        .join("\n"),
                ),
                ExprKind::DottedList(l, dot) => write!(
                    f,
                    "{indentation}{{dotted-list {}\n{}\n{}.\n{dot:#width$?}}}",
                    self.span,
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
                        "{indentation}{{{} {}}}",
                        if *b { "#t" } else { "#f" },
                        self.span,
                    )
                }
                ExprKind::Char(c) => {
                    write!(
                        f,
                        "{indentation}{{#\\{} {}}}",
                        if c.is_alphanumeric() {
                            format!("{c}")
                        } else {
                            format!("x{:X}", u32::from(*c))
                        },
                        self.span,
                    )
                }
                ExprKind::Var(path) => {
                    write!(
                        f,
                        "{indentation}{{var |{}| {} {}}}",
                        path.value,
                        path.module.resolve(),
                        self.span
                    )
                }
                ExprKind::Lambda {
                    formals,
                    rest,
                    exprs,
                } => write!(
                    f,
                    "{indentation}{{λ {}\n{}({})\n{}{}\n{}}}",
                    self.span,
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
                    exprs
                        .iter()
                        .map(|e| format!("{e:#width$?}", width = width + INDENTATION_WIDTH))
                        .collect::<Vec<_>>()
                        .join("\n"),
                ),
                ExprKind::Error(e) => {
                    write!(
                        f,
                        "{indentation}{{error {}\n{e:#width$?}}}",
                        self.span,
                        width = width + INDENTATION_WIDTH
                    )
                }
                ExprKind::Quote(e) => {
                    write!(
                        f,
                        "{indentation}{{quote {}\n{e:#width$?}}}",
                        self.span,
                        width = width + INDENTATION_WIDTH
                    )
                }
                ExprKind::If(cond, tru, fls) => write!(
                    f,
                    "{indentation}{{if {}\n{cond:#width$?}\n{tru:#width$?}\n{fls:#width$?}}}",
                    self.span,
                    width = width + INDENTATION_WIDTH,
                ),
                ExprKind::Void => write!(f, "{indentation}{{void {}}}", self.span),
                ExprKind::LetRec { defs, exprs } => write!(
                    f,
                    "{indentation}{{letrec {}\n{}\n{}}}",
                    self.span,
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
