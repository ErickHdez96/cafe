use crate::{
    env::Env,
    syntax::{
        ast::{self, ModuleName},
        cst::{SynExp, SynList, SynSymbol},
    },
};

use super::{scopes::Scope, Binding, Expander};

pub fn import(expander: &mut Expander, syn: SynList, env: &mut Env<String, Binding>) {
    let mut paths = vec![];
    let versions = vec![];
    let syn_span = syn.span();
    let close_delim_char = syn.close_delim_char();
    let close_delim_span = syn.close_delim_span();
    let (sexps, _) = syn.into_parts();
    let mut sexps = sexps.into_iter();
    assert!(sexps.next().is_some());

    match sexps.next() {
        Some(SynExp::List(l)) => {
            for sy in l.sexps() {
                match sy {
                    SynExp::Symbol(s) => paths.push(String::from(s.value())),
                    SynExp::List(l) => {
                        if !l.sexps().is_empty() {
                            expander.emit_error(|b| b.msg("version must be empty").span(l.span()));
                        }
                    }
                    se => {
                        expander.emit_error(|b| {
                            b.msg(format!("expected an identifier, found {}", se))
                                .span(l.close_delim_span())
                        });
                    }
                }
            }
        }
        sexp => {
            expander.emit_error(|b| {
                b.msg(format!(
                    "expected a module name, found {}",
                    sexp.as_ref()
                        .map(ToString::to_string)
                        .unwrap_or_else(|| close_delim_char.to_string())
                ))
                .span(close_delim_span)
            });
            return;
        }
    }

    match (expander.import)(ModuleName { paths, versions }) {
        Ok(i) => {
            for (v, b) in dbg!(i.bindings.bindings()) {
                if env.has_immediate(v) {
                    expander.emit_error(|b| {
                        b.msg(format!("variable {} already bound", v))
                            .span(syn_span)
                    });
                } else {
                    env.insert(v.clone(), b.clone());
                }
            }
        }
        Err(d) => {
            expander.diagnostics.push(d);
        }
    }
}

pub fn if_core_transformer(
    expander: &mut Expander,
    syn: SynList,
    env: &Env<String, Binding>,
) -> ast::Expr {
    let span = syn.span();
    let (sexps, _) = syn.into_parts();
    let mut children = sexps.into_iter();
    children.next();

    let cond = match children.next() {
        Some(c) => expander.expand_expr(c, env),
        None => {
            expander.emit_error(|b| b.msg("expected a condition").span(span));
            ast::Expr {
                span,
                kind: ast::ExprKind::Void,
            }
            .into_error()
        }
    };

    let tru = match children.next() {
        Some(c) => expander.expand_expr(c, env),
        None => {
            expander.emit_error(|b| b.msg("expected a true branch").span(span));
            ast::Expr {
                span,
                kind: ast::ExprKind::Void,
            }
            .into_error()
        }
    };

    let fls = match children.next() {
        Some(c) => expander.expand_expr(c, env),
        None => ast::Expr {
            span,
            kind: ast::ExprKind::Void,
        },
    };

    ast::Expr {
        span,
        kind: ast::ExprKind::If(Box::new(cond), Box::new(tru), Box::new(fls)),
    }
}

pub fn lambda_core_transformer(
    expander: &mut Expander,
    syn: SynList,
    env: &Env<String, Binding>,
) -> ast::Expr {
    let lambda_scope = Scope::new();
    let mut lambda_env = env.enter();
    let mut children = syn.sexps().iter();
    children.next();

    let (formal_binds, rest_bind) = children
        .next()
        .map(|c| lambda_formals(expander, c))
        .unwrap_or_else(|| {
            expander.emit_error(|b| {
                b
                    // fixme
                    .msg("bad syntax, expected a formals list")
                    .span(syn.span())
            });
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
        .map(|c| expander.expand_expr(c.with_scope(lambda_scope), &lambda_env))
        .collect::<Vec<_>>();
    ast::Expr {
        span: syn.span(),
        kind: ast::ExprKind::Lambda {
            formals,
            rest,
            exprs: body,
        },
    }
}

fn lambda_formals<'a>(
    expander: &mut Expander,
    syn: &'a SynExp,
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
                        expander.emit_error(|b| {
                            b.msg(format!("expected a formal, found {}", f.red().green()))
                                .span(f.span())
                        });
                    }
                }
            }
            if let Some(f) = l.dot() {
                match f {
                    SynExp::Symbol(s) => {
                        dot = Some(s);
                    }
                    SynExp::List(_) | SynExp::Boolean(_) | SynExp::Char(_) => {
                        expander.emit_error(|b| {
                            b.msg(format!("expected a formal, found {}", f.red().green()))
                                .span(f.span())
                        });
                    }
                }
            }
            (formals, dot)
        }
        SynExp::Symbol(s) => (vec![], Some(s)),
        SynExp::Char(c) => {
            expander.emit_error(|b| {
                b.msg(format!("expected a list or an identifier, found {c}"))
                    .span(c.span())
            });
            (vec![], None)
        }
        SynExp::Boolean(b) => {
            expander.emit_error(|br| {
                br.msg(format!("expected a list or an identifier, found {b}"))
                    .span(b.span())
            });
            (vec![], None)
        }
    }
}
