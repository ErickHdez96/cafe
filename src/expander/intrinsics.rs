use crate::{
    env::Env,
    syntax::{
        ast::{ModId, Module, ModuleName},
        cst::{SynExp, SynList},
    },
};

use super::{items_to_letrec, scopes::Scope, Binding, Expander};

/// Expands a module declaration.
///
/// ```text
/// <module> := ( module <module-name> <exports> <body> )
/// <exports> := ( <identifier>* )
/// ```
pub fn module(expander: &mut Expander, syn: SynList) {
    let syn_span = syn.source_span();
    let (found_close_token, expected_close_delim, close_delim_span) = syn.error_helpers();
    let (sexps, _) = syn.into_parts();
    let mut sexps = sexps.into_iter();

    let mid = if let Some(sexp) = sexps.nth(1) {
        if let Some(mid) = parse_module_name(expander, sexp) {
            mid
        } else {
            return;
        }
    } else {
        expander.emit_error(|b| {
            b.msg(format!(
                "expected a module name, found `{found_close_token}`"
            ))
            .show_after()
            .span(close_delim_span)
        });
        return;
    };
    expander.enter_module(mid);

    let mut exports = vec![];
    match sexps.next() {
        Some(SynExp::List(l)) => {
            let (sexps, _) = l.into_parts();
            for e in sexps {
                match e.into_symbol() {
                    Ok(s) => {
                        exports.push((s.value().to_string(), s.source_span()));
                    }
                    Err(s) => {
                        expander.emit_error(|b| {
                            b.msg(format!("expected an identifier, found `{s}`"))
                                .span(s.source_span())
                        });
                    }
                }
            }
        }
        se => {
            expander.emit_error(move |b| {
                b.msg(format!(
                    "expected a list of exported identifiers, found `{}`",
                    se.as_ref()
                        .map(ToString::to_string)
                        .unwrap_or_else(|| expected_close_delim.to_string())
                ))
                .span(
                    se.as_ref()
                        .map(SynExp::source_span)
                        .unwrap_or(close_delim_span),
                )
            });
        }
    }

    let module_scope = Scope::new();
    let mut bindings = Env::new();
    let mut deferred = vec![];
    let intrinsics = (expander.import)(ModuleName::from_strings(vec![
        "rnrs",
        "expander",
        "intrinsics",
    ]))
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
                    b.msg(format!("cannot export undefined variable `{e}`"))
                        .span(span)
                });
            }
        }
    }

    (expander.register)(
        mid,
        Module {
            span: syn_span,
            id: mid,
            dependencies: std::mem::take(&mut expander.dependencies),
            exports: exported_bindings,
            bindings: Env::with_bindings(bindings.into_bindings()),
            body: items_to_letrec(items, syn_span),
            types: None,
        },
    );
    expander.exit_module();
}

pub fn import(expander: &mut Expander, syn: SynList, env: &mut Env<String, Binding>) {
    let syn_span = syn.source_span();
    let close_delim_char = syn.expected_close_char();
    let close_delim_span = syn.close_delim_span();
    let (sexps, _) = syn.into_parts();
    let mut sexps = sexps.into_iter();
    assert!(sexps.next().is_some());

    let (mid, span) = if let Some(sexp) = sexps.next() {
        let span = sexp.source_span();
        if let Some(mid) = parse_module_name(expander, sexp) {
            (mid, span)
        } else {
            return;
        }
    } else {
        expander.emit_error(|b| {
            b.msg(format!(
                "expected a module name, found `{close_delim_char}`"
            ))
            .span(close_delim_span)
        });
        return;
    };

    match expander.import(mid) {
        Ok(i) => {
            for (v, b) in i.bindings.bindings() {
                if env.has_immediate(v) {
                    expander.emit_error(|b| {
                        b.msg(format!("variable `{}` already bound", v))
                            .span(syn_span)
                    });
                } else {
                    env.insert(v.clone(), b.clone());
                }
            }
        }
        Err(mut d) => {
            if d.span.is_dummy() {
                d.span = span;
            }
            expander.diagnostics.push(d);
        }
    }
}

pub fn parse_module_name(expander: &mut Expander, syn: SynExp) -> Option<ModId> {
    let span = syn.source_span();
    match syn {
        SynExp::List(l) => {
            let mut paths = vec![];
            for sy in l.sexps() {
                match sy {
                    SynExp::Symbol(s) => paths.push(String::from(s.value())),
                    SynExp::List(l) => {
                        if !l.sexps().is_empty() {
                            expander.emit_error(|b| {
                                b.msg("version must be empty").span(l.source_span())
                            });
                        }
                    }
                    se => {
                        expander.emit_error(|b| {
                            b.msg(format!("expected an identifier, found `{}`", se))
                                .span(se.source_span())
                        });
                    }
                }
            }
            if l.sexps().is_empty() {
                expander.emit_error(|b| {
                    b.span(span)
                        .msg(format!("expected a module name, found `{}`", l))
                });
                None
            } else {
                Some(ModuleName::from_strings(paths))
            }
        }
        sexp => {
            expander.emit_error(|b| {
                b.msg(format!("expected a module name, found `{sexp}`"))
                    .span(sexp.source_span())
            });
            None
        }
    }
}
