use crate::{
    env::Env,
    new_expander::{
        scopes::Scope,
        syntax::{SynList, SynSymbol, Syntax},
        Binding, Expander, SynSource,
    },
    new_syntax::ast::{self, Item},
};

pub fn lambda_transformer(
    expander: &mut Expander,
    syn: SynList,
    env: &mut Env<String, Binding>,
) -> Item {
    let span = syn.span();
    let lambda_scope = Scope::new();
    let close_delim_char = syn.expected_close_char();
    let close_delim_span = syn.close_delim_span();
    let mut lambda_env = env.enter();
    let mut source = SynSource::new(syn.value);
    source.next();

    let (formal_binds, rest_bind) = source
        .next()
        .map(|syn| lambda_formals(expander, syn))
        .unwrap_or_else(|| {
            expander.emit_error(|b| {
                b
                    // fixme
                    .msg(format!(
                        "expected a formals list, found `{}`",
                        close_delim_char
                    ))
                    .span(close_delim_span)
            });
            (vec![], None)
        });

    let mut formals = vec![];
    let mut rest = None;
    for f in &formal_binds {
        let mut binding = Binding::new_var(f.value(), expander.current_module(), f.scopes.clone());
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
        let mut binding = Binding::new_var(r.value(), expander.current_module(), r.scopes.clone());
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

    let body = source
        .map(|syn| expander.expand_expr(syn.with_scope(lambda_scope), &mut lambda_env))
        .collect::<Vec<_>>();

    //if body.is_empty() && syn.sexps().len() >= 2 {
    //    expander.emit_error(|b| {
    //        b.msg(format!(
    //            "expected an expression, found `{}`",
    //            close_delim_char
    //        ))
    //        .span(close_delim_span)
    //    });
    //}

    Item::Expr(ast::Expr {
        span,
        kind: ast::ExprKind::Lambda {
            formals,
            rest,
            expr: Box::new(ast::Expr {
                span,
                kind: ast::ExprKind::Let {
                    kind: ast::LetKind::LetRec,
                    defs: vec![], // TODO
                    exprs: body,
                },
            }),
        },
    })
}

fn lambda_formals(expander: &mut Expander, syn: Syntax) -> (Vec<SynSymbol>, Option<SynSymbol>) {
    match syn {
        Syntax::Raw(_) => todo!(),
        Syntax::List(l) => {
            let mut formals = vec![];
            let mut dot = None;
            let mut source = SynSource::new(l.value);
            for f in source.by_ref() {
                match f {
                    Syntax::Raw(_) => todo!("{f:?}"),
                    Syntax::List(_) => todo!("{f:?}"),
                    Syntax::Symbol(s) => {
                        formals.push(s);
                    }
                }
                //match f {
                //    SynExp::Symbol(s) => {
                //        formals.push(s);
                //    }
                //    SynExp::List(_) | SynExp::Boolean(_) | SynExp::Char(_) => {
                //        expander.emit_error(|b| {
                //            b.msg(format!("expected a formal, found `{}`", f.red().green()))
                //                .span(f.source_span())
                //        });
                //    }
                //}
            }
            if let Some(f) = source.dot().next() {
                match f {
                    Syntax::Raw(_) => todo!("{f:?}"),
                    Syntax::List(_) => todo!("{f:?}"),
                    Syntax::Symbol(s) => {
                        dot = Some(s);
                    }
                }
                //match f {
                //    SynExp::Symbol(s) => {
                //        dot = Some(s);
                //    }
                //    SynExp::List(_) | SynExp::Boolean(_) | SynExp::Char(_) => {
                //        expander.emit_error(|b| {
                //            b.msg(format!("expected a formal, found `{}`", f.red().green()))
                //                .span(f.source_span())
                //        });
                //    }
                //}
            }
            (formals, dot)
        }
        Syntax::Symbol(s) => (vec![], Some(s)),
    }
    //match syn {
    //    SynExp::List(l) => {
    //        let mut formals = vec![];
    //        let mut dot = None;
    //        for f in l.sexps() {
    //            match f {
    //                SynExp::Symbol(s) => {
    //                    formals.push(s);
    //                }
    //                SynExp::List(_) | SynExp::Boolean(_) | SynExp::Char(_) => {
    //                    expander.emit_error(|b| {
    //                        b.msg(format!("expected a formal, found `{}`", f.red().green()))
    //                            .span(f.source_span())
    //                    });
    //                }
    //            }
    //        }
    //        if let Some(f) = l.dot() {
    //            match f {
    //                SynExp::Symbol(s) => {
    //                    dot = Some(s);
    //                }
    //                SynExp::List(_) | SynExp::Boolean(_) | SynExp::Char(_) => {
    //                    expander.emit_error(|b| {
    //                        b.msg(format!("expected a formal, found `{}`", f.red().green()))
    //                            .span(f.source_span())
    //                    });
    //                }
    //            }
    //        }
    //        (formals, dot)
    //    }
    //    SynExp::Symbol(s) => (vec![], Some(s)),
    //    SynExp::Char(c) => {
    //        expander.emit_error(|b| {
    //            b.msg(format!("expected a list or an identifier, found `{c}`"))
    //                .span(c.source_span())
    //        });
    //        (vec![], None)
    //    }
    //    SynExp::Boolean(b) => {
    //        expander.emit_error(|br| {
    //            br.msg(format!("expected a list or an identifier, found `{b}`"))
    //                .span(b.source_span())
    //        });
    //        (vec![], None)
    //    }
    //}
}
