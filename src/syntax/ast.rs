use std::fmt;

use crate::{env::Env, expander::Binding, span::Span};

const INDENTATION_WIDTH: usize = 2;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName {
    pub paths: Vec<String>,
    pub versions: Vec<usize>,
}

impl fmt::Display for ModuleName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({} ({}))",
            self.paths.join(" "),
            self.versions
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
}

#[derive(Debug, Clone, Hash)]
pub struct ModuleInterface {
    pub span: Span,
    pub name: ModuleName,
    // TODO: bring Binding here
    pub bindings: Env<'static, String, Binding>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub span: Span,
    pub name: ModuleName,
    pub items: Vec<Item>,
    pub exports: Env<'static, String, Binding>,
    pub bindings: Env<'static, String, Binding>,
}

impl Module {
    pub fn to_interface(&self) -> ModuleInterface {
        ModuleInterface {
            span: self.span,
            name: self.name.clone(),
            bindings: self.exports.clone(),
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
                "{padding}mod {} @{}{}",
                self.name,
                self.span,
                if self.items.is_empty() {
                    String::new()
                } else {
                    format!(
                        "\n{}",
                        self.items
                            .iter()
                            .map(|i| format!("{i:#width$?}", width = width + INDENTATION_WIDTH))
                            .collect::<Vec<_>>()
                            .join("\n"),
                    )
                }
            )
        } else {
            f.debug_struct("Module")
                .field("span", &self.span)
                .field("items", &self.items)
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
    pub module: ModuleName,
    pub value: String,
}

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let padding = " ".repeat(f.width().unwrap_or_default());
            write!(
                f,
                "{padding}{{|{}| {} {}}}",
                self.value, self.module, self.span
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
    Quote(Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Lambda {
        formals: Vec<Ident>,
        rest: Option<Ident>,
        exprs: Vec<Expr>,
    },
    List(Vec<Expr>),
    Boolean(bool),
    Char(char),
    Var(Path),
    Error(Box<Item>),
    Void,
}

impl Expr {
    #[must_use]
    pub fn into_error(self) -> Self {
        Self {
            span: self.span,
            kind: ExprKind::Error(Box::new(Item::Expr(self))),
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
                        path.value, path.module, self.span
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
                    write!(f, "{indentation}{{error {} ({e:#?})}}", self.span)
                }
                ExprKind::Quote(_) => todo!(),
                ExprKind::If(cond, tru, fls) => write!(
                    f,
                    "{indentation}{{if {}\n{cond:#width$?}\n{tru:#width$?}\n{fls:#width$?}}}",
                    self.span,
                    width = width + INDENTATION_WIDTH,
                ),
                ExprKind::Void => write!(f, "{indentation}{{void {}}}", self.span),
            }
        } else {
            f.debug_struct("Expr")
                .field("span", &self.span)
                .field("kind", &self.kind)
                .finish()
        }
    }
}
