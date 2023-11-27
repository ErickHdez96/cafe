use crate::{
    diagnostics::Diagnostic,
    env::Env,
    syntax::{
        ast,
        cst::{SynExp, SynList, SynSymbol},
    },
};

use super::{expand_expr, scopes::Scope, Binding};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeSyntaxTransformer;

pub fn if_core_transformer(
    syn: &SynList,
    env: &Env<String, Binding>,
) -> (ast::Expr, Vec<Diagnostic>) {
    let mut diags = vec![];
    let mut children = syn.sexps().iter();
    children.next();

    let cond = match children.next() {
        Some(c) => {
            let (e, d) = expand_expr(c, env);
            diags.extend(d);
            e
        }
        None => {
            diags.push(
                Diagnostic::builder()
                    .msg("expected a condition")
                    .span(syn.span())
                    .finish(),
            );
            ast::Expr {
                span: syn.span(),
                kind: ast::ExprKind::Void,
            }
            .into_error()
        }
    };

    let tru = match children.next() {
        Some(c) => {
            let (e, d) = expand_expr(c, env);
            diags.extend(d);
            e
        }
        None => {
            diags.push(
                Diagnostic::builder()
                    .msg("expected a true branch")
                    .span(syn.span())
                    .finish(),
            );
            ast::Expr {
                span: syn.span(),
                kind: ast::ExprKind::Void,
            }
            .into_error()
        }
    };

    let fls = match children.next() {
        Some(c) => {
            let (e, d) = expand_expr(c, env);
            diags.extend(d);
            e
        }
        None => ast::Expr {
            span: syn.span(),
            kind: ast::ExprKind::Void,
        },
    };

    (
        ast::Expr {
            span: syn.span(),
            kind: ast::ExprKind::If(Box::new(cond), Box::new(tru), Box::new(fls)),
        },
        diags,
    )
}

pub fn lambda_core_transformer(
    syn: &SynList,
    env: &Env<String, Binding>,
) -> (ast::Expr, Vec<Diagnostic>) {
    let lambda_scope = Scope::new();
    let mut lambda_env = env.enter();
    let mut diags = vec![];
    let mut children = syn.sexps().iter();
    children.next();

    let (formal_binds, rest_bind) = children
        .next()
        .map(|c| lambda_formals(c, &mut diags))
        .unwrap_or_else(|| {
            diags.push(
                Diagnostic::builder()
                    // fixme
                    .msg("bad syntax, expected a formals list")
                    .span(syn.span())
                    .finish(),
            );
            (vec![], None)
        });

    let mut formals = vec![];
    let mut rest = None;
    for f in &formal_binds {
        let mut binding = Binding::new_var(f.value(), f.scopes().clone());
        binding.scopes_mut().add(lambda_scope);
        if let Binding::Value {
            orig_name, name, ..
        } = &binding
        {
            formals.push(ast::Ident {
                span: f.span(),
                value: name.as_ref().unwrap_or(orig_name).to_string(),
            });
        }
        lambda_env.insert(f.value().to_string(), binding);
    }

    if let Some(r) = &rest_bind {
        let mut binding = Binding::new_var(r.value(), r.scopes().clone());
        binding.scopes_mut().add(lambda_scope);
        if let Binding::Value {
            orig_name, name, ..
        } = &binding
        {
            rest = Some(ast::Ident {
                span: r.span(),
                value: name.as_ref().unwrap_or(orig_name).to_string(),
            });
        }
        lambda_env.insert(r.value().to_string(), binding);
    }

    let body = children
        .map(|c| {
            let (expr, errs) = expand_expr(&c.with_scope(lambda_scope), &lambda_env);
            diags.extend(errs);
            expr
        })
        .collect::<Vec<_>>();
    (
        ast::Expr {
            span: syn.span(),
            kind: ast::ExprKind::Lambda {
                formals,
                rest,
                exprs: body,
            },
        },
        diags,
    )
}

fn lambda_formals<'a>(
    syn: &'a SynExp,
    diags: &mut Vec<Diagnostic>,
) -> (Vec<&'a SynSymbol>, Option<&'a SynSymbol>) {
    match syn {
        SynExp::List(l) => {
            let mut formals = vec![];
            let mut dot = None;
            for f in l.sexps() {
                match f {
                    SynExp::Symbol(s) => {
                        formals.push(s);
                    }
                    SynExp::List(_) | SynExp::Boolean(_) | SynExp::Char(_) => {
                        diags.push(
                            Diagnostic::builder()
                                .msg(format!("expected a formal, found {}", f.red().green()))
                                .span(f.span())
                                .finish(),
                        );
                    }
                }
            }
            if let Some(f) = l.dot() {
                match f {
                    SynExp::Symbol(s) => {
                        dot = Some(s);
                    }
                    SynExp::List(_) | SynExp::Boolean(_) | SynExp::Char(_) => {
                        diags.push(
                            Diagnostic::builder()
                                .msg(format!("expected a formal, found {}", f.red().green()))
                                .span(f.span())
                                .finish(),
                        );
                    }
                }
            }
            (formals, dot)
        }
        SynExp::Symbol(s) => (vec![], Some(s)),
        SynExp::Char(c) => {
            diags.push(
                Diagnostic::builder()
                    .msg(format!("expected a list or an identifier, found {c}"))
                    .span(c.span())
                    .finish(),
            );
            (vec![], None)
        }
        SynExp::Boolean(b) => {
            diags.push(
                Diagnostic::builder()
                    .msg(format!("expected a list or an identifier, found {b}"))
                    .span(b.span())
                    .finish(),
            );
            (vec![], None)
        }
    }
}
