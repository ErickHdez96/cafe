use std::rc::Rc;

use crate::{
    diagnostics::Diagnostic,
    env::Env,
    file::FileId,
    span::Span,
    syntax::{
        ast,
        cst::{GreenTree, RedTree, SynExp, SynList, SynSymbol},
        SyntaxKind,
    },
};

use super::{
    macros::compile_transformer,
    scopes::{Scope, Scopes},
    Binding, Expander,
};

pub fn if_transformer(
    expander: &mut Expander,
    syn: SynList,
    env: &Env<String, Binding>,
) -> ast::Expr {
    let span = syn.source_span();
    let close_delim_char = syn.expected_close_char();
    let close_delim_span = syn.close_delim_span();
    let (sexps, _) = syn.into_parts();
    let mut children = sexps.into_iter();
    children.next();

    let cond = match children.next() {
        Some(c) => expander.expand_expr(c, env),
        None => {
            expander.emit_error(|b| {
                b.msg(format!(
                    "expected a condition, found `{}`",
                    close_delim_char
                ))
                .span(close_delim_span)
            });
            return ast::Expr {
                span,
                kind: ast::ExprKind::If(
                    Box::new(ast::Expr {
                        span,
                        kind: ast::ExprKind::Void,
                    }),
                    Box::new(ast::Expr {
                        span,
                        kind: ast::ExprKind::Void,
                    }),
                    Box::new(ast::Expr {
                        span,
                        kind: ast::ExprKind::Void,
                    }),
                ),
            }
            .into_error();
        }
    };

    let tru = match children.next() {
        Some(c) => expander.expand_expr(c, env),
        None => {
            expander.emit_error(|b| {
                b.msg(format!(
                    "expected a true branch, found `{}`",
                    close_delim_char
                ))
                .span(close_delim_span)
            });
            return ast::Expr {
                span,
                kind: ast::ExprKind::If(
                    Box::new(cond),
                    Box::new(ast::Expr {
                        span,
                        kind: ast::ExprKind::Void,
                    }),
                    Box::new(ast::Expr {
                        span,
                        kind: ast::ExprKind::Void,
                    }),
                ),
            }
            .into_error();
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

pub fn lambda_transformer(
    expander: &mut Expander,
    syn: SynList,
    env: &Env<String, Binding>,
) -> ast::Expr {
    let lambda_scope = Scope::new();
    let close_delim_char = &syn.expected_close_char();
    let close_delim_span = syn.close_delim_span();
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
        let mut binding =
            Binding::new_var(f.value(), expander.current_module(), f.scopes().clone());
        binding.scopes_mut().add(lambda_scope);
        if let Binding::Value {
            orig_name, name, ..
        } = &binding
        {
            formals.push(ast::Ident {
                span: f.source_span(),
                value: name.as_ref().unwrap_or(orig_name).to_string(),
            });
        }
        lambda_env.insert(f.value().to_string(), binding);
    }

    if let Some(r) = &rest_bind {
        let mut binding =
            Binding::new_var(r.value(), expander.current_module(), r.scopes().clone());
        binding.scopes_mut().add(lambda_scope);
        if let Binding::Value {
            orig_name, name, ..
        } = &binding
        {
            rest = Some(ast::Ident {
                span: r.source_span(),
                value: name.as_ref().unwrap_or(orig_name).to_string(),
            });
        }
        lambda_env.insert(r.value().to_string(), binding);
    }

    let body = children
        .map(|c| expander.expand_expr(c.with_scope(lambda_scope), &lambda_env))
        .collect::<Vec<_>>();

    if body.is_empty() && syn.sexps().len() >= 2 {
        expander.emit_error(|b| {
            b.msg(format!(
                "expected an expression, found `{}`",
                close_delim_char
            ))
            .span(close_delim_span)
        });
    }

    ast::Expr {
        span: syn.source_span(),
        kind: ast::ExprKind::Lambda {
            formals,
            rest,
            expr: Box::new(ast::Expr {
                span: syn.source_span(),
                kind: ast::ExprKind::LetRec {
                    defs: vec![], // TODO
                    exprs: body,
                },
            }),
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
                            b.msg(format!("expected a formal, found `{}`", f.red().green()))
                                .span(f.source_span())
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
                            b.msg(format!("expected a formal, found `{}`", f.red().green()))
                                .span(f.source_span())
                        });
                    }
                }
            }
            (formals, dot)
        }
        SynExp::Symbol(s) => (vec![], Some(s)),
        SynExp::Char(c) => {
            expander.emit_error(|b| {
                b.msg(format!("expected a list or an identifier, found `{c}`"))
                    .span(c.source_span())
            });
            (vec![], None)
        }
        SynExp::Boolean(b) => {
            expander.emit_error(|br| {
                br.msg(format!("expected a list or an identifier, found `{b}`"))
                    .span(b.source_span())
            });
            (vec![], None)
        }
    }
}

pub fn define_transformer(
    expander: &mut Expander,
    syn: SynList,
    env: &Env<String, Binding>,
) -> Option<ast::Define> {
    let span = syn.source_span();
    let close_delim_char = syn.expected_close_char();
    let close_delim_span = syn.close_delim_span();
    let (sexps, _) = syn.into_parts();
    let mut children = sexps.into_iter();
    children.next();

    let name = match children.next() {
        Some(SynExp::Symbol(sy)) => {
            // env should already have the binding to the variable
            ast::Ident {
                span: sy.source_span(),
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

pub fn define_syntax_transformer(
    expander: &mut Expander,
    syn: SynList,
    env: &mut Env<String, Binding>,
) {
    let close_delim_char = syn.expected_close_char();
    let close_delim_span = syn.close_delim_span();
    let (sexps, _) = syn.into_parts();
    let mut children = sexps.into_iter();
    children.next();

    let (name, scopes) = match children.next() {
        Some(SynExp::Symbol(sy)) => (sy.value().to_string(), sy.scopes().clone()),
        _ => {
            expander.emit_error(|b| {
                b.msg(format!("expected an identifier, found {close_delim_char}",))
                    .span(close_delim_span)
            });
            return;
        }
    };

    let (transformer, diags) = match children.next() {
        Some(e) => compile_transformer(&e, env),
        None => {
            expander.emit_error(|b| {
                b.msg(format!("expected a transformer, found {close_delim_char}"))
                    .span(close_delim_span)
            });
            return;
        }
    };
    expander.diagnostics.extend(diags);
    env.insert(
        name.clone(),
        Binding::NativeSyntaxTransformer {
            scopes,
            name,
            transformer: transformer.into(),
        },
    );

    if let Some(c) = children.next() {
        expander.emit_error(|b| {
            b.msg(format!("expected {}, found {}", close_delim_char, c))
                .span(close_delim_span)
        });
    }
}

pub fn quote_transformer(
    expander: &mut Expander,
    syn: SynList,
    _: &Env<String, Binding>,
) -> ast::Expr {
    let span = syn.source_span();
    let red = Rc::clone(syn.red());
    let (sexps, _) = syn.into_parts();
    let mut children = sexps.into_iter();
    children.next();

    let expr = match children.next() {
        Some(e) => quote_expr(expander, e),
        None => quote_missing_expr(expander, &red, span),
    };

    if let Some(r) = children.next() {
        expander.emit_error(|b| {
            b.msg(format!("expected close identifier, found {}", r))
                .span(r.source_span())
        });
    };

    let mut expr = expr.into_quote();
    expr.span = span;
    expr
}

fn quote_expr(expander: &Expander, syn: SynExp) -> ast::Expr {
    let span = syn.source_span();
    match syn {
        SynExp::List(l) => ast::Expr {
            span,
            kind: {
                let (sexps, dot) = l.into_parts();
                let sexps = sexps.into_iter().map(|s| quote_expr(expander, s)).collect();
                if let Some(dot) = dot {
                    ast::ExprKind::DottedList(sexps, Box::new(quote_expr(expander, *dot)))
                } else {
                    ast::ExprKind::List(sexps)
                }
            },
        },
        SynExp::Symbol(s) => ast::Expr {
            span,
            kind: ast::ExprKind::Var(ast::Path {
                span,
                module: expander.current_module(),
                value: s.value().to_string(),
            }),
        },
        SynExp::Boolean(b) => ast::Expr {
            span,
            kind: ast::ExprKind::Boolean(b.value()),
        },
        SynExp::Char(c) => ast::Expr {
            span,
            kind: ast::ExprKind::Char(c.value()),
        },
    }
}

fn quote_missing_expr(expander: &mut Expander, red: &Rc<RedTree>, span: Span) -> ast::Expr {
    let file_id = span.file_id();
    let mut children = RedTree::children(red).into_iter();
    let tok = children
        .find(|c| c.kind().is_abbrev() || c.kind().is_open_delim())
        .unwrap_or_else(|| panic!("expected an abbreviation or a list, found {}", red));

    if tok.kind().is_open_delim() {
        let last_repr = children
            .clone()
            // find `quote`
            .skip_while(|c| c.kind().is_trivia())
            // and skip it
            .skip(1)
            .find(|c| !c.kind().is_trivia());
        let last_span = children
            .reduce(|acc, cur| if cur.kind().is_trivia() { acc } else { cur })
            .expect("expected at least quote");

        expander.emit_error(|b| {
            b.msg(format!(
                "expected an expression, found {}",
                last_repr
                    .as_ref()
                    .map(ToString::to_string)
                    .unwrap_or_else(|| String::from("<eof>"))
            ))
            .span(Span::new(
                file_id,
                last_span.offset().try_into().unwrap(),
                last_span.text_length().try_into().unwrap(),
            ))
            .show_after_select(last_repr.is_none())
        });
    } else if !children.any(|c| !c.kind().is_trivia()) {
        // If the abbreviation is followed by an invalid token, then a diagnostic must have
        // been generated for it already. Therefore, the only other explanation, is an <eof>
        expander.emit_error(|b| {
            b.msg("expected an expression to quote")
                .span(Span::new(
                    file_id,
                    tok.offset().try_into().unwrap(),
                    tok.text_length().try_into().unwrap(),
                ))
                .show_after()
        });
    }

    ast::Expr {
        span,
        kind: ast::ExprKind::Void,
    }
    .into_error()
}

pub fn library_transformer(syn: SynList) -> Result<SynExp, Vec<Diagnostic>> {
    let mut diags = vec![];
    let file_id = syn.file_id();
    let close_delim_char = syn.expected_close_char();
    let close_delim_span = syn.close_delim_span();
    let (sexps, _) = syn.into_parts();
    let mut children = sexps.into_iter();
    children.next();

    let mname = if let Some(sexp) = children.next() {
        sexp
    } else {
        diags.push(
            Diagnostic::builder()
                .msg(format!(
                    "expected a module name, found `{close_delim_char}`"
                ))
                .show_after()
                .span(close_delim_span)
                .finish(),
        );
        return Err(diags);
    };
    let exports = match children.next() {
        Some(syn) => match library_export(&mut diags, syn) {
            Some(syn) => syn,
            None => return Err(diags),
        },
        None => {
            diags.push(
                Diagnostic::builder()
                    .msg(format!(
                        "expected list of exported identifiers, found `{close_delim_char}`"
                    ))
                    .show_after()
                    .span(close_delim_span)
                    .finish(),
            );
            return Err(diags);
        }
    };

    // Intern token
    let module_ident_green = Rc::new(GreenTree::token(
        SyntaxKind::Identifier,
        String::from("module"),
    ));
    let module_ident_green_syn = SynExp::Symbol(SynSymbol::raw(
        &RedTree::new(&module_ident_green),
        Scopes::core(),
        file_id,
        None,
    ));
    let ws = Rc::new(GreenTree::token(SyntaxKind::Whitespace, String::from(" ")));
    let lopen = Rc::new(GreenTree::token(SyntaxKind::OpenDelim, String::from("(")));
    let lclose = Rc::new(GreenTree::token(SyntaxKind::CloseDelim, String::from(")")));
    let mut elist = vec![Rc::clone(&lopen)];
    for (i, e) in exports.iter().enumerate() {
        elist.push(Rc::clone(e.red().green()));
        if i + 1 < exports.len() {
            elist.push(Rc::clone(&ws));
        }
    }
    elist.push(Rc::clone(&lclose));
    let export_list = Rc::new(GreenTree::node(SyntaxKind::List, elist));
    let mut mlist = vec![lopen, module_ident_green, Rc::clone(&ws)];
    mlist.push(Rc::clone(mname.red().green()));
    mlist.push(Rc::clone(&ws));
    mlist.push(Rc::clone(&export_list));
    mlist.push(Rc::clone(&ws));

    let mut sexps = vec![
        module_ident_green_syn,
        mname,
        SynExp::List(SynList::raw(
            &RedTree::new(&export_list),
            exports,
            None,
            file_id,
            None,
        )),
    ];
    for c in children {
        mlist.push(Rc::clone(c.red().green()));
        mlist.push(Rc::clone(&ws));
        sexps.push(c);
    }

    mlist.push(lclose);
    let module_green = Rc::new(GreenTree::node(SyntaxKind::List, mlist));

    if diags.is_empty() {
        let syn = SynExp::List(SynList::raw(
            &RedTree::new(&module_green),
            sexps,
            None,
            FileId::default(),
            None,
        ));
        eprintln!("{syn}");
        Ok(syn)
    } else {
        Err(diags)
    }
}

fn library_export(diags: &mut Vec<Diagnostic>, syn: SynExp) -> Option<Vec<SynExp>> {
    let syn = match syn.into_list() {
        Ok(l) => l,
        Err(syn) => {
            diags.push(
                Diagnostic::builder()
                    .msg(format!(
                        "expected list of exported identifiers, found {}",
                        syn
                    ))
                    .span(syn.source_span())
                    .finish(),
            );
            return None;
        }
    };

    let close_delim_char = syn.expected_close_char();
    let close_delim_span = syn.close_delim_span();
    let (sexps, _) = syn.into_parts();
    let mut children = sexps.into_iter().peekable();

    match children.peek() {
        Some(SynExp::Symbol(s)) if s.value() == "export" => {
            children.next();
        }
        Some(syn) => {
            diags.push(
                Diagnostic::builder()
                    .msg(format!("expected `export`, found {syn}"))
                    .span(syn.source_span())
                    .finish(),
            );
        }
        None => {
            diags.push(
                Diagnostic::builder()
                    .msg(format!("expected `export`, found {close_delim_char}"))
                    .span(close_delim_span)
                    .finish(),
            );
            return None;
        }
    }

    Some(children.collect())
}

#[cfg(test)]
mod tests {
    use super::super::tests::modules::check;

    mod library {
        use expect_test::expect;

        use super::*;

        #[test]
        fn simple() {
            check(
                "(library
                   (my-lib)
                   (export id)
                   (import (rnrs expander core))
                   (define id (lambda (x) x)))
                 (import (my-lib))
                 (id #t)",
                expect![[r#"
                    mod (#script ()) @0:0..223
                      {letrec 0:0..223
                        ()
                        {list 0:216..7
                          {var |id| (my-lib ()) 0:217..2}
                          {#t 0:220..2}}}
                "#]],
            );
        }
    }
}
