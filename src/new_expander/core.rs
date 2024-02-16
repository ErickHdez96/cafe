use std::rc::Rc;

use crate::{
    env::Env,
    new_syntax::{ast::Item, cst::Cst},
};

use super::{Binding, Ctx, Expander};

//pub fn define_transformer(
//    expander: &mut Expander,
//    syn: Cst<'_>,
//    env: &mut Env<String, Binding>,
//) -> Item {
//    let span = syn.source_span();
//    let green = Rc::clone(syn.red().green());
//    let close_delim_char = syn.expected_close_char();
//    let close_delim_span = syn.close_delim_span();
//    let (sexps, _) = syn.into_parts();
//    let mut syn_children = sexps.into_iter();
//    syn_children.next();
//    let mut children = vec![];
//
//    match syn_children.next() {
//        Some(SynExp::Symbol(sy)) => {
//            // env should already have the binding to the variable
//            children.push(
//                Atom::new_var(
//                    sy.source_span(),
//                    Rc::clone(sy.red().green()),
//                    expander.current_module(),
//                    sy.value().to_string(),
//                )
//                .into(),
//            );
//        }
//        _ => {
//            expander.emit_error(|b| {
//                b.msg(format!(
//                    "expected an identifier or a list, found {}",
//                    close_delim_char
//                ))
//                .span(close_delim_span)
//            });
//            todo!()
//            //return Some(
//            //    Define {
//            //        span,
//            //        green,
//            //        name: None,
//            //        expr: None,
//            //    }
//            //    .into_item(),
//            //);
//        }
//    }
//
//    if let Some(e) = syn_children
//        .next()
//        .map(|c| expander.expand_syntax(c, env, Ctx::Expr))
//    {
//        match e {
//            super::Expanded::Syn(_) => todo!(),
//            super::Expanded::Binding(_, _) => todo!(),
//            super::Expanded::Item(_) => todo!(),
//        }
//        children.push(e);
//    }
//
//    if let Some(c) = syn_children.next() {
//        expander.emit_error(|b| {
//            b.msg(format!("expected {}, found {}", close_delim_char, c))
//                .span(close_delim_span)
//        });
//    }
//
//    List::new_define(span, green, children).into()
//}

//pub fn if_transformer(expander: &mut Expander, syn: SynList, env: &Env<String, Binding>) -> Item {
//    let span = syn.source_span();
//    let green = Rc::clone(syn.red().green());
//    let close_delim_char = syn.expected_close_char();
//    let close_delim_span = syn.close_delim_span();
//    let (sexps, dot) = syn.into_parts();
//    let mut children = sexps.into_iter();
//    let mut exprs = vec![];
//
//    match children.nth(1) {
//        Some(c) => {
//            exprs.push(expander.expand_expr(c, env));
//        }
//        None => {
//            expander.emit_error(|b| {
//                b.msg(format!("expected a condition, found `{close_delim_char}`"))
//                    .span(close_delim_span)
//            });
//            return Expr::new_if(span, &green, vec![]).into_error().into_item();
//        }
//    }
//
//    match children.next() {
//        Some(c) => {
//            exprs.push(expander.expand_expr(c, env));
//        }
//        None => {
//            expander.emit_error(|b| {
//                b.msg(format!(
//                    "expected a true branch, found `{close_delim_char}`",
//                ))
//                .span(close_delim_span)
//            });
//            return Expr::new_if(span, &green, exprs).into_error().into_item();
//        }
//    }
//
//    if let Some(c) = children.next() {
//        exprs.push(expander.expand_expr(c, env));
//    }
//
//    if dot.is_some() {
//        todo!()
//    }
//
//    Expr::new_if(span, &green, exprs).into_item()
//}
