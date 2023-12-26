use std::{fmt, rc::Rc};

use crate::{
    diagnostics::Diagnostic,
    env::Env,
    syntax::cst::{SynExp, SynList, SynSymbol},
};

use self::{
    patterns::{compile_pattern, Pattern},
    templates::{compile_template, Template},
};

use super::{Binding, Expander};

pub mod patterns;
pub mod templates;

const INDENTATION_WIDTH: usize = 2;

pub fn compile_transformer(
    syn: &SynExp,
    env: &Env<String, Binding>,
) -> (NativeSyntaxTransformer, Vec<Diagnostic>) {
    match syn.list() {
        // TODO: Use syntax-rules from the environment
        Some(l)
            if l.sexps()
                .first()
                .and_then(SynExp::symbol)
                .map(SynSymbol::value)
                == Some("syntax-rules") =>
        {
            compile_syntax_rules(l, env)
        }
        s => {
            let mut db = Diagnostic::builder().msg("expected a transformer");
            if let Some(s) = s {
                db = db.span(s.source_span());
            }
            (
                NativeSyntaxTransformer {
                    // TODO: return an ID transformer (syntax-rules () [(_ e) e])
                    rules: vec![],
                },
                vec![db.finish()],
            )
        }
    }
}

pub fn compile_syntax_rules(
    syn: &SynList,
    env: &Env<String, Binding>,
) -> (NativeSyntaxTransformer, Vec<Diagnostic>) {
    let mut syns = syn.sexps().iter();
    let mut diags = vec![];
    syns.next().expect("expected syntax-rules");

    let _kws: Vec<()> = if let Some(s) = syns.next() {
        match s {
            SynExp::List(l) if l.sexps().is_empty() => vec![],
            SynExp::List(_) => todo!(),
            SynExp::Symbol(_) | SynExp::Boolean(_) | SynExp::Char(_) => {
                diags.push(
                    Diagnostic::builder()
                        .msg(format!("expected a keyword list, found {}", s.red()))
                        .span(syn.close_delim_span())
                        .finish(),
                );
                vec![]
            }
        }
    } else {
        diags.push(
            Diagnostic::builder()
                .msg("expected a keyword list")
                .span(syn.close_delim_span())
                .finish(),
        );
        vec![]
    };

    let mut rules = vec![];
    for s in syns {
        let (rule, ds) = compile_syntax_rule(s, env);
        diags.extend(ds);
        if let Some(rule) = rule {
            rules.push(rule);
        }
    }

    (NativeSyntaxTransformer { rules }, diags)
}

fn compile_syntax_rule(
    syn: &SynExp,
    env: &Env<String, Binding>,
) -> (Option<(Pattern, Template)>, Vec<Diagnostic>) {
    let mut diags = vec![];
    let (srpattern_syn, template_syn) = match syn {
        SynExp::List(l) => {
            let mut sexps = l.sexps().iter();
            match (sexps.next(), sexps.next()) {
                (None, _) => {
                    return (
                        None,
                        vec![Diagnostic::builder()
                            .msg("expected a pattern and template")
                            .span(l.close_delim_span())
                            .finish()],
                    );
                }
                (Some(_), None) => {
                    return (
                        None,
                        vec![Diagnostic::builder()
                            .msg("expected a template")
                            .span(l.close_delim_span())
                            .finish()],
                    );
                }
                (Some(p), Some(t)) => {
                    if let Some(n) = sexps.next() {
                        diags.push(
                            Diagnostic::builder()
                                .msg(format!("expected {}, found {}", l.expected_close_char(), n))
                                .span(l.close_delim_span())
                                .finish(),
                        );
                    }
                    (p, t)
                }
            }
        }
        SynExp::Symbol(_) | SynExp::Boolean(_) | SynExp::Char(_) => {
            return (
                None,
                vec![Diagnostic::builder()
                    .msg(format!("expected a syntax rule, found {}", syn.red()))
                    .span(syn.source_span())
                    .finish()],
            );
        }
    };

    // TODO: Fix (syntax-rules () [#t #t])
    let (pattern, ds) = compile_pattern(srpattern_syn);
    diags.extend(ds);
    let (template, ds) = compile_template(template_syn, &pattern, env);
    diags.extend(ds);

    (Some((pattern, template)), diags)
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct NativeSyntaxTransformer {
    rules: Vec<(Pattern, Template)>,
}

impl NativeSyntaxTransformer {
    pub fn expand(
        &self,
        _expander: &mut Expander,
        mut syn: SynList,
        _env: &Env<String, Binding>,
    ) -> Option<SynExp> {
        for (p, t) in &self.rules {
            let pl = p.list().expect("a list pattern");
            let red = Rc::clone(syn.red());
            let file_id = syn.file_id();
            match Pattern::match_list(pl, syn) {
                Ok(vars) => return Some(t.instantiate(&vars, red, file_id)),
                Err(s) => {
                    syn = s;
                }
            };
        }

        None
    }
}

impl fmt::Debug for NativeSyntaxTransformer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let padding = " ".repeat(width);
            let padding2 = " ".repeat(width + INDENTATION_WIDTH);
            for (pat, template) in &self.rules {
                writeln!(f, "{padding}rules:")?;
                writeln!(f, "{padding}- pattern:")?;
                writeln!(
                    f,
                    "{pat:#width$?}",
                    width = width + INDENTATION_WIDTH + INDENTATION_WIDTH
                )?;
                writeln!(f, "{padding2}template:")?;
                write!(
                    f,
                    "{template:#width$?}",
                    width = width + INDENTATION_WIDTH + INDENTATION_WIDTH
                )?;
            }
            Ok(())
        } else {
            f.debug_struct("NativeSyntaxTransformer")
                .field("rules", &self.rules)
                .finish()
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::{
        expander::{core, scopes::Scopes},
        file::FileId,
        syntax::{ast::ModuleName, cst::SynRoot, parser::parse_str},
    };

    use super::*;

    pub fn root_env() -> Env<'static, String, Binding> {
        let mut core = Env::new();
        core.insert(
            String::from("lambda"),
            Binding::CoreExprTransformer {
                scopes: Scopes::core(),
                name: String::from("lambda"),
                transformer: core::lambda_transformer,
            },
        );
        core.insert(
            String::from("if"),
            Binding::CoreExprTransformer {
                scopes: Scopes::core(),
                name: String::from("if"),
                transformer: core::if_transformer,
            },
        );
        core.insert(
            String::from("cons"),
            Binding::Value {
                scopes: Scopes::core(),
                orig_module: ModuleName::script(),
                orig_name: String::from("cons"),
                name: None,
            },
        );
        core.enter_consume()
    }

    mod syntax_rules {
        use super::*;

        fn check(input: &str, expected: Expect) {
            let res = parse_str(input);
            assert_eq!(res.diagnostics, vec![]);
            let red = SynRoot::new(&res.tree, FileId::default());
            let mut children = red.syn_children();
            let syn = children
                .next()
                .expect("expected an item")
                .into_list()
                .unwrap()
                .clone();
            let (pattern, errs) = compile_syntax_rules(&syn, &root_env());
            assert_eq!(errs, vec![]);
            expected.assert_debug_eq(&pattern);
        }

        #[test]
        fn no_variables() {
            check(
                "(syntax-rules () [(or) #t])",
                expect![[r#"
                    rules:
                    - pattern:
                        list@0:18..4
                          or@0:19..2
                      template:
                        constant - #t @0:23..2
                "#]],
            );
        }

        #[test]
        fn simple_variable() {
            check(
                "(syntax-rules () [(or e) e])",
                expect![[r#"
                    rules:
                    - pattern:
                        list@0:18..6
                          or@0:19..2
                          e@0:22..1
                      template:
                        variable - e @0:25..1
                "#]],
            );
        }

        #[test]
        fn simple_list() {
            check(
                "(syntax-rules () [(null) (quote ())])",
                expect![[r#"
                    rules:
                    - pattern:
                        list@0:18..6
                          null@0:19..4
                      template:
                        list@0:25..10
                          constant - quote @0:26..5
                          list@0:32..2
                "#]],
            );
        }

        #[test]
        fn simple_repeat() {
            check(
                "(syntax-rules ()
                   [(when cond body0 body ...)
                     (if cond (begin body0 body ...))])",
                expect![[r#"
                    rules:
                    - pattern:
                        list@0:37..26
                          when@0:38..4
                          cond@0:43..4
                          body0@0:48..5
                          repeat@0:54..8
                            body@0:54..4
                      template:
                        list@0:85..32
                          captured-binding - if @0:86..2
                          variable - cond @0:89..4
                          list@0:94..22
                            constant - begin @0:95..5
                            variable - body0 @0:101..5
                            splice@0:107..8
                              variable - body @0:107..4
                "#]],
            );
        }

        #[test]
        fn double_repeat() {
            check(
                "(syntax-rules () [(flat (a ...) ...) (list a ... ...)])",
                expect![[r#"
                    rules:
                    - pattern:
                        list@0:18..18
                          flat@0:19..4
                          repeat@0:24..11
                            list@0:24..7
                              repeat@0:25..5
                                a@0:25..1
                      template:
                        list@0:37..16
                          constant - list @0:38..4
                          splice@0:43..9
                            splice@0:43..5
                              variable - a @0:43..1
                "#]],
            );
        }

        #[test]
        fn let_() {
            check(
                "(syntax-rules ()
                   [(let ((v e) ...) body0 body ...)
                     ((lambda (v ...)
                       body0
                       body ...)
                      e ...)])",
                expect![[r#"
                    rules:
                    - pattern:
                        list@0:37..32
                          let@0:38..3
                          list@0:42..11
                            repeat@0:43..9
                              list@0:43..5
                                v@0:44..1
                                e@0:46..1
                          body0@0:54..5
                          repeat@0:60..8
                            body@0:60..4
                      template:
                        list@0:91..107
                          list@0:92..77
                            captured-binding - lambda @0:93..6
                            list@0:100..7
                              splice@0:101..5
                                variable - v @0:101..1
                            variable - body0 @0:131..5
                            splice@0:160..8
                              variable - body @0:160..4
                          splice@0:192..5
                            variable - e @0:192..1
                "#]],
            );
        }
    }

    mod errors {
        use crate::span::Span;

        use super::*;

        fn check_errors(input: &str, errors: Vec<Diagnostic>) {
            let res = parse_str(input);
            assert_eq!(res.diagnostics, vec![]);
            let red = SynRoot::new(&res.tree, FileId::default());
            let mut children = red.syn_children();
            let syn = children
                .next()
                .expect("expected an item")
                .into_list()
                .unwrap()
                .clone();
            let (_, errs) = compile_syntax_rules(&syn, &root_env());
            assert_eq!(errs, errors);
        }

        #[test]
        fn splice_no_repeating_variable() {
            check_errors(
                "(syntax-rules () [(_ e) (e ...)])",
                vec![Diagnostic::builder()
                    .msg("variable e not repeating at this point")
                    .span(Span::new(FileId::default(), 25, 1))
                    .finish()],
            );
        }

        #[test]
        fn splice_list_no_repeating_variable() {
            check_errors(
                "(syntax-rules () [(_ e) ((e) ...)])",
                vec![Diagnostic::builder()
                    .msg("no variable is repeating at this point")
                    .span(Span::new(FileId::default(), 25, 3))
                    .finish()],
            );
        }
    }
}
