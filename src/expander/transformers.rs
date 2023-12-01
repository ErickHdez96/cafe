use std::rc::Rc;

use crate::{
    env::Env,
    span::Span,
    syntax::{
        ast::{self, ModuleName},
        cst::{GreenTree, RedTree, SynExp, SynList, SynSymbol},
        SyntaxKind,
    },
};

use super::{scopes::Scope, Binding, Expander};

pub fn if_core_transformer(
    expander: &mut Expander,
    syn: SynList,
    env: &Env<String, Binding>,
) -> ast::Expr {
    let span = syn.span();
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

pub fn lambda_core_transformer(
    expander: &mut Expander,
    syn: SynList,
    env: &Env<String, Binding>,
) -> ast::Expr {
    let lambda_scope = Scope::new();
    let close_delim_char = dbg!(&syn).expected_close_char();
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
                            b.msg(format!("expected a formal, found `{}`", f.red().green()))
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
                            b.msg(format!("expected a formal, found `{}`", f.red().green()))
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
                b.msg(format!("expected a list or an identifier, found `{c}`"))
                    .span(c.span())
            });
            (vec![], None)
        }
        SynExp::Boolean(b) => {
            expander.emit_error(|br| {
                br.msg(format!("expected a list or an identifier, found `{b}`"))
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
    let close_delim_char = syn.expected_close_char();
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

pub fn quote_core_transformer(
    expander: &mut Expander,
    syn: SynList,
    _: &Env<String, Binding>,
) -> ast::Expr {
    let span = syn.span();
    let red = Rc::clone(syn.red());
    let file_id = syn.file_id();
    let (sexps, _) = syn.into_parts();
    let mut children = sexps.into_iter();
    children.next();

    let expr = match children.next() {
        Some(e) => quote_expr(e),
        None => {
            let mut children = RedTree::children(&red).into_iter();
            while let Some(c) = children.next() {
                if let GreenTree::Token(tok) = c.green().as_ref() {
                    if tok.kind().is_abbrev() {
                        match children.find(|c| !c.kind().is_trivia()) {
                            Some(_) => {
                                // c can only be an error token, diagnostic should be generated by
                                // the parser
                            }
                            None => {
                                expander.emit_error(|b| {
                                    b.msg("expected an expression to quote")
                                        .span(Span::new(
                                            file_id,
                                            c.offset().try_into().unwrap(),
                                            c.text_length().try_into().unwrap(),
                                        ))
                                        .show_after()
                                });
                            }
                        }
                        break;
                    } else if matches!(
                        tok.kind(),
                        SyntaxKind::OpenDelim | SyntaxKind::SpecialOpenDelim
                    ) {
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
                        break;
                    }
                }
            }
            ast::Expr {
                span,
                kind: ast::ExprKind::Void,
            }
            .into_error()
        }
    };

    if let Some(r) = children.next() {
        expander.emit_error(|b| {
            b.msg(format!("expected close identifier, found {}", r))
                .span(r.span())
        });
    };

    let mut expr = expr.into_quote();
    expr.span = span;
    expr
}

fn quote_expr(syn: SynExp) -> ast::Expr {
    let span = syn.span();
    match syn {
        SynExp::List(l) => ast::Expr {
            span,
            kind: {
                let (sexps, dot) = l.into_parts();
                let sexps = sexps.into_iter().map(quote_expr).collect();
                if let Some(dot) = dot {
                    ast::ExprKind::DottedList(sexps, Box::new(quote_expr(*dot)))
                } else {
                    ast::ExprKind::List(sexps)
                }
            },
        },
        SynExp::Symbol(s) => ast::Expr {
            span,
            kind: ast::ExprKind::Var(ast::Path {
                span,
                module: ModuleName {
                    paths: vec![],
                    versions: vec![],
                },
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
