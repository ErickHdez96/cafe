use crate::{
    env::Env,
    new_expander::{syntax::Syntax, CstSource},
    new_syntax::{
        ast::{self, Item},
        cst::{CstKind, ListKind},
    },
};

use super::{syntax::SynList, Binding, Ctx, Expanded, Expander};

pub fn define_transformer(
    expander: &mut Expander,
    syn: SynList,
    env: &mut Env<String, Binding>,
) -> Item {
    let span = syn.span;
    let close_delim_char = syn.expected_close_char();
    let close_delim_span = syn.close_delim_span();
    //let (sexps, _) = syn.into_parts();
    //let mut syn_children = sexps.into_iter();
    //syn_children.next();
    let mut source = CstSource::new(syn.value);
    source.next();

    let name = match source.next() {
        Some(cst) => match &cst.kind {
            CstKind::Ident(n) => ast::Ident {
                span: cst.span,
                value: n.clone(),
            },
            _ => {
                expander.emit_error(|b| {
                    b.msg(format!(
                        "expected an identifier or a list, found {}",
                        close_delim_char
                    ))
                    .span(close_delim_span)
                });
                todo!();
            } // env should already have the binding to the variable
        },
        _ => {
            expander.emit_error(|b| {
                b.msg(format!(
                    "expected an identifier or a list, found {}",
                    close_delim_char
                ))
                .span(close_delim_span)
            });
            todo!()
        }
    };

    let expr = if let Some(e) = source.next().map(|c| {
        expander.expand_syntax(
            Syntax {
                cst: c,
                scopes: syn.scopes.clone(),
            },
            env,
            Ctx::Expr,
        )
    }) {
        match e {
            Expanded::Item(Item::Expr(e)) => Some(e),
            _ => todo!(),
        }
    } else {
        None
    };

    if let Some(c) = source.next() {
        expander.emit_error(|b| {
            b.msg(format!("expected {}, found {}", close_delim_char, c))
                .span(close_delim_span)
        });
    }

    ast::Define { span, name, expr }.into()
}

pub fn if_transformer(expander: &mut Expander, syn: SynList, env: &Env<String, Binding>) -> Item {
    assert_eq!(syn.kind, ListKind::List);
    let span = syn.span;
    let close_delim_char = syn.expected_close_char();
    let close_delim_span = syn.close_delim_span();
    //let (sexps, dot) = syn.into_parts();
    //let mut children = sexps.into_iter();
    let mut source = CstSource::new(syn.value);
    source.next();

    let cond = match source.next() {
        Some(c) => expander.expand_expr(
            Syntax {
                cst: c,
                scopes: syn.scopes.clone(),
            },
            env,
        ),
        None => {
            expander.emit_error(|b| {
                b.msg(format!("expected a condition, found `{close_delim_char}`"))
                    .span(close_delim_span)
            });
            return ast::Expr {
                span,
                kind: ast::ExprKind::If(
                    Box::new(ast::Expr::dummy()),
                    Box::new(ast::Expr::dummy()),
                    Box::new(ast::Expr::dummy()),
                ),
            }
            .into_error()
            .into();
        }
    };

    let r#true = match source.next() {
        Some(c) => expander.expand_expr(
            Syntax {
                cst: c,
                scopes: syn.scopes.clone(),
            },
            env,
        ),
        None => {
            expander.emit_error(|b| {
                b.msg(format!(
                    "expected a true branch, found `{close_delim_char}`",
                ))
                .span(close_delim_span)
            });
            return ast::Expr {
                span,
                kind: ast::ExprKind::If(
                    Box::new(cond),
                    Box::new(ast::Expr::dummy()),
                    Box::new(ast::Expr::dummy()),
                ),
            }
            .into_error()
            .into();
        }
    };

    let r#false = if let Some(c) = source.next() {
        expander.expand_expr(
            Syntax {
                cst: c,
                scopes: syn.scopes,
            },
            env,
        )
    } else {
        ast::Expr {
            span,
            kind: ast::ExprKind::Void,
        }
    };

    //if dot.is_some() {
    //    todo!()
    //}

    ast::Expr {
        span,
        kind: ast::ExprKind::If(Box::new(cond), Box::new(r#true), Box::new(r#false)),
    }
    .into()
}
