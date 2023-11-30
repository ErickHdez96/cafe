use crate::{
    env::Env,
    syntax::{
        ast::{self, Module, ModuleName},
        cst::{SynExp, SynList, SynSymbol},
    },
};

use super::{scopes::Scope, Binding, Expander};

pub fn module(expander: &mut Expander, syn: SynList) {
    let syn_span = syn.span();
    let close_delim_char = syn.close_delim_char();
    let close_delim_span = syn.close_delim_span();
    let (sexps, _) = syn.into_parts();
    let mut sexps = sexps.into_iter();

    let name = if let Some(sexp) = sexps.nth(1) {
        if let Some(name) = parse_module_name(expander, sexp) {
            name
        } else {
            return;
        }
    } else {
        expander.emit_error(|b| {
            b.msg(format!("expected a module name, found {close_delim_char}"))
                .span(close_delim_span)
        });
        return;
    };
    expander.enter_module(name.clone());

    let mut exports = vec![];
    if let Some(SynExp::List(l)) = sexps.next() {
        let (sexps, _) = l.into_parts();
        for e in sexps {
            match e.into_symbol() {
                Ok(s) => {
                    exports.push((s.value().to_string(), s.span()));
                }
                Err(s) => {
                    expander.emit_error(|b| {
                        b.msg(format!("expected an identifier, found {s}"))
                            .span(close_delim_span)
                    });
                }
            }
        }
    } else {
        expander.emit_error(|b| {
            b.msg(format!(
                "expected a list of exported identifiers, found {close_delim_char}"
            ))
            .span(close_delim_span)
        });
    }

    let module_scope = Scope::new();
    let mut bindings = Env::new();
    let mut deferred = vec![];
    let intrinsics = (expander.import)(ModuleName {
        paths: vec![
            String::from("cafe"),
            String::from("expander"),
            String::from("intrinsics"),
        ],
        versions: vec![],
    })
    .unwrap();
    for (v, b) in intrinsics.bindings.bindings() {
        bindings.insert(v.clone(), b.clone());
    }

    for mut i in sexps {
        i.reset_scope();
        if let Some(def) = expander.expand_macro(i.with_scope(module_scope), &mut bindings) {
            deferred.push(def);
        }
    }

    let mut items = vec![];

    for d in deferred {
        if let Some(item) = expander.expand_item(d, &bindings) {
            items.push(item);
        }
    }

    let mut exported_bindings = Env::new();
    for (e, span) in exports {
        match bindings.get_immediate(&e) {
            Some(b) => {
                exported_bindings.insert(e, b.clone());
            }
            None => {
                expander.emit_error(|b| {
                    b.msg(format!("tried to export undefined variable {e}"))
                        .span(span)
                });
            }
        }
    }

    (expander.register)(
        name.clone(),
        Module {
            span: syn_span,
            name,
            items,
            exports: exported_bindings,
            bindings: Env::with_bindings(bindings.into_bindings()),
        },
    );
    expander.exit_module();
}

pub fn import(expander: &mut Expander, syn: SynList, env: &mut Env<String, Binding>) {
    let syn_span = syn.span();
    let close_delim_char = syn.close_delim_char();
    let close_delim_span = syn.close_delim_span();
    let (sexps, _) = syn.into_parts();
    let mut sexps = sexps.into_iter();
    assert!(sexps.next().is_some());

    let name = if let Some(sexp) = sexps.next() {
        if let Some(name) = parse_module_name(expander, sexp) {
            name
        } else {
            return;
        }
    } else {
        expander.emit_error(|b| {
            b.msg(format!("expected a module name, found {close_delim_char}"))
                .span(close_delim_span)
        });
        return;
    };

    match (expander.import)(name) {
        Ok(i) => {
            for (v, b) in i.bindings.bindings() {
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

fn parse_module_name(expander: &mut Expander, syn: SynExp) -> Option<ModuleName> {
    match syn {
        SynExp::List(l) => {
            let mut paths = vec![];
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
            Some(ModuleName {
                paths,
                versions: vec![],
            })
        }
        sexp => {
            expander.emit_error(|b| {
                b.msg(format!("expected a module name, found {sexp}"))
                    .span(sexp.span())
            });
            None
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
        let mut binding = Binding::new_var(
            f.value(),
            expander.current_module().clone(),
            f.scopes().clone(),
        );
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
        let mut binding = Binding::new_var(
            r.value(),
            expander.current_module().clone(),
            r.scopes().clone(),
        );
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

pub fn define_core_transformer(
    expander: &mut Expander,
    syn: SynList,
    env: &Env<String, Binding>,
) -> Option<ast::Define> {
    let span = syn.span();
    let close_delim_char = syn.close_delim_char();
    let close_delim_span = syn.close_delim_span();
    let (sexps, _) = syn.into_parts();
    let mut children = sexps.into_iter();
    children.next();

    let name = match children.next() {
        Some(SynExp::Symbol(sy)) => {
            // env should already have the binding to the variable
            ast::Ident {
                span: sy.span(),
                value: sy.value().to_string(),
            }
        }
        _ => {
            expander.emit_error(|b| {
                b.msg(format!(
                    "expected an identifier or a list, found {}",
                    close_delim_char
                ))
                .span(close_delim_span)
            });
            return None;
        }
    };

    let expr = children.next().map(|c| expander.expand_expr(c, env));

    if let Some(c) = children.next() {
        expander.emit_error(|b| {
            b.msg(format!("expected {}, found {}", close_delim_char, c))
                .span(close_delim_span)
        });
    }

    Some(ast::Define { span, name, expr })
}
