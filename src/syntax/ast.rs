use std::fmt;

use crate::{
    arena::{Arena, WithOptionalArena},
    env::Env,
    expander::binding::Binding,
    span::Span,
    symbol::Symbol,
    ty::{Ty, TyK, TyScheme},
};

pub use self::mod_id::ModId;

use super::parser::Number;

mod mod_id;

const INDENTATION_WIDTH: usize = 2;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub id: ModId,
    pub span: Span,
    pub dependencies: Vec<ModId>,
    /// Bindings exported from the Module.
    pub exports: Env<'static, Symbol, Binding>,
    /// All the root bindings (e.g. macro, value) of the module.
    pub bindings: Env<'static, Symbol, Binding>,
    pub types: Option<Env<'static, Symbol, TyScheme>>,
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
            types: self.types.clone(),
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
    pub bindings: Env<'static, Symbol, Binding>,
    pub types: Option<Env<'static, Symbol, TyScheme>>,
    pub dependencies: Vec<ModId>,
}

/// A [`ModuleName`] comprises of its individual path components (e.g. rnrs io simple) which must be
/// valid identifiers, and its optional version (e.g. 1 0 1).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName {
    pub paths: Vec<Symbol>,
    pub versions: Vec<usize>,
}

impl ModuleName {
    pub fn intern(self) -> ModId {
        ModId::intern(self)
    }

    /// Returns a special [`ModuleName`] identifying the root of the module tree when compiling a
    /// project.
    pub fn script() -> ModId {
        Self {
            paths: vec![Symbol::from("#script")],
            versions: vec![],
        }
        .intern()
    }

    pub fn from_strings(strs: Vec<impl Into<Symbol>>) -> ModId {
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
            self.paths
                .iter()
                .copied()
                .map(Symbol::resolve)
                .collect::<Vec<_>>()
                .join(" "),
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
    pub value: Symbol,
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

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Path {
    pub span: Span,
    pub module: ModId,
    pub value: Symbol,
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
    Import(Vec<ModId>, Span),
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

    pub fn with_arena<'a>(&'a self, arena: &'a Arena<TyK>) -> WithOptionalArena<'a, TyK, Self> {
        WithOptionalArena {
            arena: Some(arena),
            item: self,
        }
    }

    pub fn with_maybe_arena<'a>(
        &'a self,
        arena: Option<&'a Arena<TyK>>,
    ) -> WithOptionalArena<'a, TyK, Self> {
        WithOptionalArena { arena, item: self }
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
        self.with_maybe_arena(None).fmt(f)
    }
}

impl fmt::Debug for WithOptionalArena<'_, TyK, Item> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self.item {
                Item::Import(mid, span) => {
                    let width = f.width().unwrap_or_default();
                    let padding = " ".repeat(width);
                    write!(
                        f,
                        "{padding}{{import {}@{span}}}",
                        mid.iter()
                            .map(|m| format!("{}", m.resolve()))
                            .collect::<Vec<_>>()
                            .join(" "),
                    )
                }
                Item::Mod(mid, span) => {
                    let width = f.width().unwrap_or_default();
                    let padding = " ".repeat(width);
                    write!(f, "{padding}{{module {}@{span}}}", mid.resolve(),)
                }
                Item::Define(d) => d.with_maybe_arena(self.arena).fmt(f),
                Item::Expr(e) => e.with_maybe_arena(self.arena).fmt(f),
            }
        } else {
            match self.item {
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
    pub ty: Option<TyScheme>,
}

impl Define {
    pub fn with_arena<'a>(&'a self, arena: &'a Arena<TyK>) -> WithOptionalArena<'a, TyK, Self> {
        WithOptionalArena {
            arena: Some(arena),
            item: self,
        }
    }

    pub fn with_maybe_arena<'a>(
        &'a self,
        arena: Option<&'a Arena<TyK>>,
    ) -> WithOptionalArena<'a, TyK, Self> {
        WithOptionalArena { arena, item: self }
    }
}

impl fmt::Debug for Define {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.with_maybe_arena(None).fmt(f)
    }
}

impl fmt::Debug for WithOptionalArena<'_, TyK, Define> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let padding = " ".repeat(width);
            write!(
                f,
                "{padding}{{define@{}\n{}{}}}",
                self.item.span,
                if let (Some(ty), Some(arena)) = (&self.item.ty, self.arena) {
                    format!(
                        "{}{{|{}| : {:#?} {}}}",
                        " ".repeat(width + INDENTATION_WIDTH),
                        self.item.name.value,
                        ty.with_arena(arena),
                        self.item.name.span,
                    )
                } else {
                    format!(
                        "{:#width$?}",
                        self.item.name,
                        width = width + INDENTATION_WIDTH,
                    )
                },
                if let Some(e) = &self.item.expr {
                    format!(
                        "\n{:#width$?}",
                        e.with_maybe_arena(self.arena),
                        width = width + INDENTATION_WIDTH
                    )
                } else {
                    String::new()
                },
            )
        } else {
            f.debug_struct("Define")
                .field("span", &self.item.span)
                .field("name", &self.item.name)
                .field("expr", &self.item.expr)
                .finish()
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
    pub ty_hint: Option<Ty>,
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
        formal_tys: Vec<TyScheme>,
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
            ty_hint: None,
        }
    }

    #[must_use]
    pub fn into_quote(self) -> Self {
        Self {
            span: self.span,
            kind: ExprKind::Quote(Box::new(self)),
            ty_hint: None,
        }
    }

    pub fn dummy() -> Expr {
        Self {
            span: Span::dummy(),
            kind: ExprKind::Void,
            ty_hint: None,
        }
    }

    pub fn is_lambda(&self) -> bool {
        matches!(self.kind, ExprKind::Lambda { .. })
    }

    pub fn is_void(&self) -> bool {
        matches!(self.kind, ExprKind::Void)
    }

    pub fn with_arena<'a>(&'a self, arena: &'a Arena<TyK>) -> WithOptionalArena<'a, TyK, Self> {
        WithOptionalArena {
            arena: Some(arena),
            item: self,
        }
    }

    pub fn with_maybe_arena<'a>(
        &'a self,
        arena: Option<&'a Arena<TyK>>,
    ) -> WithOptionalArena<'a, TyK, Self> {
        WithOptionalArena { arena, item: self }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.with_maybe_arena(None).fmt(f)
    }
}

impl fmt::Debug for WithOptionalArena<'_, TyK, Expr> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let indentation = " ".repeat(width);
            let span = self.item.span;

            match &self.item.kind {
                ExprKind::Body(items) => write!(
                    f,
                    "{indentation}{{body {span}\n{}}}",
                    items
                        .iter()
                        .map(|e| format!(
                            "{:#width$?}",
                            e.with_maybe_arena(self.arena),
                            width = width + INDENTATION_WIDTH
                        ))
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
                        .map(|e| format!(
                            "{:#width$?}",
                            e.with_maybe_arena(self.arena),
                            width = width + INDENTATION_WIDTH
                        ))
                        .collect::<Vec<_>>()
                        .join("\n"),
                ),
                ExprKind::DottedList(l, dot) => write!(
                    f,
                    "{indentation}{{dotted-list {span}\n{}\n{}.\n{:#width$?}}}",
                    l.iter()
                        .map(|e| format!(
                            "{:#width$?}",
                            e.with_maybe_arena(self.arena),
                            width = width + INDENTATION_WIDTH
                        ))
                        .collect::<Vec<_>>()
                        .join("\n"),
                    " ".repeat(width + INDENTATION_WIDTH),
                    dot.with_maybe_arena(self.arena),
                    width = width + INDENTATION_WIDTH,
                ),
                ExprKind::Boolean(b) => {
                    write!(
                        f,
                        "{indentation}{{{}{} {span}}}",
                        if *b { "#t" } else { "#f" },
                        if let (Some(ty), Some(arena)) = (self.item.ty_hint, self.arena) {
                            format!(" : {:#?}", ty.with_arena(arena))
                        } else {
                            String::new()
                        }
                    )
                }
                ExprKind::Char(c) => {
                    write!(
                        f,
                        "{indentation}{{#\\{}{} {span}}}",
                        if c.is_alphanumeric() {
                            format!("{c}")
                        } else {
                            format!("x{:X}", u32::from(*c))
                        },
                        if let (Some(ty), Some(arena)) = (self.item.ty_hint, self.arena) {
                            format!(" : {:#?}", ty.with_arena(arena))
                        } else {
                            String::new()
                        }
                    )
                }
                ExprKind::Number(n) => {
                    write!(
                        f,
                        "{indentation}{{{}{} {span}}}",
                        match n {
                            Number::Fixnum(n) => format!("{n}"),
                        },
                        if let (Some(ty), Some(arena)) = (self.item.ty_hint, self.arena) {
                            format!(" : {:#?}", ty.with_arena(arena))
                        } else {
                            String::new()
                        }
                    )
                }
                ExprKind::Var(path) => {
                    write!(
                        f,
                        "{indentation}{{var |{}|{} {} {span}}}",
                        path.value,
                        if let (Some(ty), Some(arena)) = (self.item.ty_hint, self.arena) {
                            format!(" : {:#?}", ty.with_arena(arena))
                        } else {
                            String::new()
                        },
                        path.module.resolve(),
                    )
                }
                ExprKind::Lambda {
                    formals,
                    rest,
                    expr,
                    formal_tys,
                } => {
                    write!(
                        f,
                        "{indentation}{{λ{} {span}\n{}({})\n{}{}\n{:#width$?}}}",
                        if let (Some(ty), Some(arena)) = (self.item.ty_hint, self.arena) {
                            format!(" : {:#?}", ty.with_arena(arena))
                        } else {
                            String::new()
                        },
                        " ".repeat(width + INDENTATION_WIDTH),
                        if formal_tys.is_empty() || self.arena.is_none() {
                            formals
                                .iter()
                                .map(|f| format!("{f:#?}"))
                                .collect::<Vec<_>>()
                                .join(" ")
                        } else {
                            formals
                                .iter()
                                .zip(formal_tys.iter())
                                .map(|(f, ty)| {
                                    format!(
                                        "{{|{}| : {:#?} {}}}}}",
                                        f.value,
                                        ty.with_arena(self.arena.unwrap()),
                                        f.span,
                                    )
                                })
                                .collect::<Vec<_>>()
                                .join(" ")
                        },
                        " ".repeat(width + INDENTATION_WIDTH),
                        match rest {
                            Some(rest) => format!("{rest:#?}"),
                            None => String::from("#f"),
                        },
                        expr.with_maybe_arena(self.arena),
                        width = width + INDENTATION_WIDTH,
                    )
                }
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
                ExprKind::If(cond, r#true, r#false) => write!(
                    f,
                    "{indentation}{{if {span}\n{:#width$?}\n{:#width$?}\n{:#width$?}}}",
                    cond.with_maybe_arena(self.arena),
                    r#true.with_maybe_arena(self.arena),
                    r#false.with_maybe_arena(self.arena),
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
                .field("span", &self.item.span)
                .field("kind", &self.item.kind)
                .field("ty", &self.item.ty_hint)
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
