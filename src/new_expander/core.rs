use crate::{
    env::Env,
    new_expander::{syntax::Syntax, SynSource},
    new_syntax::{
        ast::{self, Item},
        cst::{CstKind, ListKind},
    },
};

use super::{syntax::SynList, Binding, Ctx, Expanded, Expander};

pub mod lambda;

pub fn define_transformer(
    expander: &mut Expander,
    syn: SynList,
    env: &mut Env<String, Binding>,
) -> Item {
    let span = syn.span();
    let close_delim_char = syn.expected_close_char();
    let close_delim_span = syn.close_delim_span();
    let mut source = SynSource::new(syn.value);
    source.next();

    let name = match source.next() {
        Some(cst) => match cst {
            Syntax::Symbol(symbol) => ast::Ident {
                span: symbol.span(),
                value: symbol.value().clone(),
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

    let expr = if let Some(e) = source
        .next()
        .map(|syn| expander.expand_syntax(syn, env, Ctx::Expr))
    {
        match e {
            Expanded::Item(Item::Expr(e)) => Some(e),
            _ => todo!(),
        }
    } else {
        None
    };

    if let Some(c) = source.next() {
        expander.emit_error(|b| {
            b.msg(format!(
                "expected {}, found {}",
                close_delim_char,
                c.source()
            ))
            .span(close_delim_span)
        });
    }

    ast::Define { span, name, expr }.into()
}

pub fn if_transformer(
    expander: &mut Expander,
    syn: SynList,
    env: &mut Env<String, Binding>,
) -> Item {
    assert!(matches!(syn.source.kind, CstKind::List(_, ListKind::List)));
    let span = syn.span();
    let close_delim_char = syn.expected_close_char();
    let close_delim_span = syn.close_delim_span();
    let mut source = SynSource::new(syn.value);
    source.next();

    let cond = match source.next() {
        Some(syn) => expander.expand_expr(syn, env),
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
        Some(syn) => expander.expand_expr(syn, env),
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

    let r#false = if let Some(syn) = source.next() {
        expander.expand_expr(syn, env)
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
