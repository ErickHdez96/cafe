use std::fmt;

use crate::span::Span;

const INDENTATION_WIDTH: usize = 2;

#[derive(Clone, PartialEq, Eq)]
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

#[derive(Clone, PartialEq, Eq)]
pub enum DefExpr {
    Expr(Expr),
}

impl fmt::Debug for DefExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self {
                DefExpr::Expr(e) => e.fmt(f),
            }
        } else {
            match self {
                DefExpr::Expr(e) => f.debug_tuple("DefExpr::Expr").field(&e).finish(),
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    Var(Ident),
    Error(Box<DefExpr>),
    Void,
}

impl Expr {
    #[must_use]
    pub fn into_error(self) -> Self {
        Self {
            span: self.span,
            kind: ExprKind::Error(Box::new(DefExpr::Expr(self))),
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
                ExprKind::Var(id) => {
                    write!(f, "{indentation}{{var |{}| {}}}", id.value, self.span)
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
