use std::{collections::HashMap, fmt, rc::Rc};

use crate::{
    diagnostics::Diagnostic,
    env::Env,
    expander::{macros::INDENTATION_WIDTH, resolve, Binding},
    file::FileId,
    span::Span,
    syntax::cst::{RedTree, SynExp, SynList, SynSymbol},
};

use super::patterns::{MatchVariable, Pattern};

pub fn compile_template(
    syn: &SynExp,
    pat: &Pattern,
    env: &Env<String, Binding>,
) -> (Template, Vec<Diagnostic>) {
    let mut diags = vec![];
    let (vars, repeated_vars) = pat.variables();
    for (v, span) in repeated_vars {
        diags.push(
            Diagnostic::builder()
                .msg(format!("variable repeated in pattern {v}"))
                .span(span)
                .finish(),
        );
    }
    let t = do_compile_template(syn, &vars, &mut diags, 0, env);
    (t, diags)
}

fn do_compile_template(
    syn: &SynExp,
    vars: &HashMap<String, usize>,
    diags: &mut Vec<Diagnostic>,
    unrepeat: usize,
    env: &Env<String, Binding>,
) -> Template {
    match syn {
        SynExp::List(l) if l.sexps.is_empty() => Template::new(
            syn.red(),
            syn.file_id(),
            TemplateKind::List(vec![], l.source_span()),
        ),
        // TODO: dot
        SynExp::List(l) => {
            let mut templates = vec![];
            let mut syns = l.sexps().iter().peekable();

            while let Some(s) = syns.next() {
                let mut new_unrepeat = unrepeat;
                let mut spans = vec![];

                while let Some(ellipsis @ SynExp::Symbol(sy)) = syns.peek() {
                    if sy.value() == "..." {
                        new_unrepeat += 1;
                        spans.push(ellipsis.source_span());
                        syns.next();
                        continue;
                    }
                    break;
                }

                if new_unrepeat == unrepeat {
                    templates.push(do_compile_template(s, vars, diags, unrepeat, env));
                } else {
                    let mut t = do_compile_template(s, vars, diags, new_unrepeat, env);
                    for span in spans {
                        t = Template::new(
                            syn.red(),
                            syn.file_id(),
                            TemplateKind::Splice(Box::new(t), s.source_span().extend(span)),
                        );
                        new_unrepeat -= 1;
                    }
                    validate_splice(&t, vars, diags, unrepeat, false);
                    templates.push(t);
                }
            }

            Template::new(
                syn.red(),
                syn.file_id(),
                TemplateKind::List(templates, syn.source_span()),
            )
        }
        SynExp::Symbol(v) => {
            match vars.get(v.value()) {
                Some(&repeat) if repeat <= unrepeat => Template::new(
                    syn.red(),
                    syn.file_id(),
                    TemplateKind::Variable(v.value().to_string(), v.source_span()),
                ),
                Some(_) => {
                    diags.push(
                        Diagnostic::builder()
                            .msg(format!(
                                "variable {} still repeating at this level",
                                v.value()
                            ))
                            .span(v.source_span())
                            .finish(),
                    );
                    Template::new(
                        syn.red(),
                        syn.file_id(),
                        TemplateKind::Error(syn.clone(), v.source_span()),
                    )
                }
                // TODO: Validate in environment
                None => {
                    let binding = resolve(v, env);
                    if let Some(b) = binding {
                        Template::new(
                            syn.red(),
                            syn.file_id(),
                            TemplateKind::Binding(b.clone(), v.source_span()),
                        )
                    } else {
                        Template::new(
                            syn.red(),
                            syn.file_id(),
                            TemplateKind::Constant(syn.clone()),
                        )
                    }
                }
            }
        }
        SynExp::Boolean(_) | SynExp::Char(_) => Template::new(
            syn.red(),
            syn.file_id(),
            TemplateKind::Constant(syn.clone()),
        ),
    }
}

/// Emits diagnostics for all the invalid splicing `...` operators.
///
/// ```text
/// (syntax-rules () [(_ e) (e ...)])
/// (syntax-rules () [(_ e) ((e) ...)])
/// (syntax-rules () [(_ e0 e ...) (e0 (e ...) ...)])
/// ```
///
/// The return value is for internal purposes. Returns true if there is a variable where its
/// repetition count and the splicing count match.
fn validate_splice(
    template: &Template,
    vars: &HashMap<String, usize>,
    diags: &mut Vec<Diagnostic>,
    unrepeat: usize,
    // `true` if we're directly iterating over elements of a list. `false` if we're at the root of
    // the template or under a splice operator.
    in_list: bool,
) -> bool {
    match &template.kind {
        // direct splice operator over variables `e ...`
        TemplateKind::Variable(v, span) if !in_list => {
            if let Some(&repeat) = vars.get(v) {
                // Variables that are still repeating are handled above
                if repeat < unrepeat {
                    diags.push(
                        Diagnostic::builder()
                            .msg(format!("variable {v} not repeating at this point"))
                            .span(*span)
                            .finish(),
                    );
                }
                repeat >= unrepeat
            } else {
                false
            }
        }
        TemplateKind::Variable(v, _) => {
            if let Some(&repeat) = vars.get(v) {
                // Variables that are still repeating are handled above
                repeat >= unrepeat
            } else {
                false
            }
        }
        TemplateKind::List(elems, span) => {
            let res = elems
                .iter()
                .all(|cur| validate_splice(cur, vars, diags, unrepeat, true));
            match (in_list, res) {
                (false, false) => {
                    // We are either at the root or under a splice operator and didn't use up all
                    // the splice operations
                    diags.push(
                        Diagnostic::builder()
                            .msg("no variable is repeating at this point")
                            .span(*span)
                            .finish(),
                    );
                    false
                }
                (_, res) => res,
            }
        }
        TemplateKind::Splice(t, _) => validate_splice(t, vars, diags, unrepeat + 1, false),
        TemplateKind::Binding(_, _) => false,
        TemplateKind::Constant(_) => false,
        TemplateKind::Error(_, _) => false,
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Template {
    red: Rc<RedTree>,
    file_id: FileId,
    kind: TemplateKind,
    // maybe: add source_span for macro generated macros
}

impl Template {
    pub fn new(red: &Rc<RedTree>, file_id: FileId, kind: TemplateKind) -> Self {
        Self {
            red: Rc::clone(red),
            file_id,
            kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TemplateKind {
    /// `#t` `#\a`
    Constant(SynExp),
    /// `e`
    Variable(String, Span),
    Binding(Binding, Span),
    /// `( a b c )`
    List(Vec<Template>, Span),
    /// `a ...` `(a) ...`
    Splice(Box<Template>, Span),
    Error(SynExp, Span),
}

impl Template {
    pub fn instantiate(
        &self,
        vars: &HashMap<String, MatchVariable>,
        source_red: Rc<RedTree>,
        file_id: FileId,
    ) -> SynExp {
        self.do_instantiate(vars, source_red, file_id, &[]).unwrap()
    }

    fn do_instantiate(
        &self,
        vars: &HashMap<String, MatchVariable>,
        source_red: Rc<RedTree>,
        file_id: FileId,
        repeat: &[usize],
    ) -> Option<SynExp> {
        match &self.kind {
            TemplateKind::Constant(c) => Some(c.clone().with_source_span(Span::new(
                file_id,
                source_red.offset().try_into().unwrap(),
                source_red.text_length().try_into().unwrap(),
            ))),
            TemplateKind::Variable(v, _) => get_variable(vars, v, repeat),
            TemplateKind::Binding(b, _) => Some(SynExp::Symbol(SynSymbol::raw(
                &self.red,
                b.scopes().clone(),
                self.file_id,
                Some(Span::new(
                    file_id,
                    source_red.offset().try_into().unwrap(),
                    source_red.text_length().try_into().unwrap(),
                )),
            ))),
            TemplateKind::List(l, _) => {
                let mut sink = Vec::new();
                for t in l {
                    t.instantiate_into_list(
                        vars,
                        Rc::clone(&source_red),
                        file_id,
                        &mut sink,
                        repeat,
                    )?;
                }

                Some(SynExp::List(SynList::raw(
                    &self.red,
                    sink,
                    None,
                    self.file_id,
                    Some(Span::new(
                        file_id,
                        source_red.offset().try_into().unwrap(),
                        source_red.text_length().try_into().unwrap(),
                    )),
                )))
            }
            TemplateKind::Splice(_, _) => panic!("should be inside a list"),
            TemplateKind::Error(_, _) => todo!(),
        }
    }

    fn instantiate_into_list(
        &self,
        vars: &HashMap<String, MatchVariable>,
        source_red: Rc<RedTree>,
        file_id: FileId,
        sink: &mut Vec<SynExp>,
        repeat: &[usize],
    ) -> Option<()> {
        match &self.kind {
            TemplateKind::Constant(_)
            | TemplateKind::Variable(_, _)
            | TemplateKind::Binding(_, _)
            | TemplateKind::List(_, _) => {
                sink.push(self.do_instantiate(vars, source_red, file_id, repeat)?);
                Some(())
            }
            TemplateKind::Splice(s, _) => {
                // TODO: emit diagnostics when variable repeat don't match
                match &s.kind {
                    TemplateKind::Variable(e, _) => match vars
                        .get(e)
                        .unwrap_or_else(|| panic!("expected variable {e}"))
                    {
                        // reach into the repeat iteration
                        MatchVariable::Variable(_) => panic!("expected a repeat variable {e}"),
                        MatchVariable::List(l) => {
                            for e in l {
                                sink.push(e.variable().cloned().expect("a variable"));
                            }
                            Some(())
                        }
                    },
                    TemplateKind::List(_, _) => {
                        let mut repeat = repeat.to_owned();
                        repeat.push(0);
                        loop {
                            match s.do_instantiate(vars, Rc::clone(&source_red), file_id, &repeat) {
                                Some(s) => sink.push(s),
                                None => break,
                            }
                            *repeat.last_mut().unwrap() += 1;
                        }
                        if repeat.last().copied().unwrap_or_default() > 0 {
                            Some(())
                        } else {
                            None
                        }
                    }
                    TemplateKind::Splice(_, _) => todo!(),
                    TemplateKind::Error(e, _) => {
                        sink.push(e.clone());
                        Some(())
                    }
                    _ => panic!("invalid splice argument"),
                }
            }
            TemplateKind::Error(_, _) => todo!(),
        }
    }
}

fn get_variable(
    vars: &HashMap<String, MatchVariable>,
    v: &str,
    repeat: &[usize],
) -> Option<SynExp> {
    match vars.get(v).expect("unbound variable") {
        MatchVariable::Variable(v) => Some(v.clone()),
        MatchVariable::List(l) => resolve_repeat_variable(l, v, repeat),
    }
}

fn resolve_repeat_variable(var: &[MatchVariable], v: &str, repeat: &[usize]) -> Option<SynExp> {
    if repeat.is_empty() {
        panic!("variable {v} not repeating anymore at this level");
    }

    match &var.get(repeat[0])? {
        MatchVariable::Variable(v) => Some(v.clone()),
        MatchVariable::List(l) => resolve_repeat_variable(l, v, &repeat[1..]),
    }
}

impl fmt::Debug for Template {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let padding = " ".repeat(width);
            match &self.kind {
                TemplateKind::Constant(c) => {
                    write!(f, "{padding}constant - {c} @{}", c.source_span())
                }
                TemplateKind::Variable(v, span) => write!(f, "{padding}variable - {v} @{span}"),
                TemplateKind::Binding(b, span) => {
                    write!(f, "{padding}captured-binding - {} @{span}", b.name())
                }
                TemplateKind::List(templs, span) => write!(
                    f,
                    "{padding}list@{span}{}",
                    if templs.is_empty() {
                        String::new()
                    } else {
                        format!(
                            "\n{}",
                            templs
                                .iter()
                                .map(|t| format!("{t:#width$?}", width = width + INDENTATION_WIDTH))
                                .collect::<Vec<_>>()
                                .join("\n")
                        )
                    }
                ),
                TemplateKind::Splice(t, span) => write!(
                    f,
                    "{padding}splice@{span}\n{t:#width$?}",
                    width = width + INDENTATION_WIDTH
                ),
                TemplateKind::Error(source, span) => write!(f, "{padding}error - {source}@{span}",),
            }
        } else {
            match &self.kind {
                TemplateKind::Constant(c) => f.debug_tuple("Template::Constant").field(&c).finish(),
                TemplateKind::Variable(v, span) => f
                    .debug_tuple("Template::Variable")
                    .field(&v)
                    .field(&span)
                    .finish(),
                TemplateKind::Binding(b, span) => f
                    .debug_tuple("Template::Binding")
                    .field(&b)
                    .field(&span)
                    .finish(),
                TemplateKind::List(elems, span) => f
                    .debug_tuple("Template::List")
                    .field(&elems)
                    .field(&span)
                    .finish(),
                TemplateKind::Splice(t, span) => f
                    .debug_tuple("Template::Splice")
                    .field(&t)
                    .field(&span)
                    .finish(),
                TemplateKind::Error(source, span) => f
                    .debug_tuple("Template::Error")
                    .field(&source)
                    .field(&span)
                    .finish(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::{
        expander::{
            macros::{compile_syntax_rules, tests::root_env},
            Expander,
        },
        file::FileId,
        syntax::{ast::ModuleName, cst::SynRoot, parser::parse_str},
    };

    use super::*;

    fn check(macro_: &str, input: &str, expected: Expect) {
        let res = parse_str(macro_);
        assert_eq!(res.diagnostics, vec![]);
        let red = SynRoot::new(&res.tree, FileId::default());
        let mut children = red.syn_children();
        let syn = children
            .next()
            .expect("expected an item")
            .into_list()
            .unwrap()
            .clone();
        let (transformer, errs) = compile_syntax_rules(&syn, &root_env());
        assert_eq!(errs, vec![]);
        let res_input = parse_str(input);
        assert_eq!(res_input.diagnostics, vec![]);
        let red = SynRoot::new(&res_input.tree, FileId::default());
        let mut children = red.syn_children();
        let syn = children
            .next()
            .expect("expected an item")
            .into_list()
            .unwrap()
            .clone();
        let mut expander = Expander {
            import: &|_| panic!(),
            register: &|_, _| panic!(),
            module: ModuleName::script(),
            module_stack: vec![],
            diagnostics: vec![],
        };
        let env = Env::new();
        expected.assert_eq(
            &transformer
                .expand(&mut expander, syn, &env)
                .unwrap()
                .syn_string(),
        );
    }

    #[test]
    fn constants() {
        check("(syntax-rules () [(_) #t])", "(or)", expect!["#t"]);
    }

    #[test]
    fn variables() {
        check("(syntax-rules () [(_ e) e])", r"(or #\a)", expect![r"#\a"]);
    }

    #[test]
    fn list() {
        check(
            "(syntax-rules () [(_ (e1 e2)) (cons e1 e2)])",
            r"(kons (#t #\a))",
            expect![r"(cons #t #\a)"],
        );
    }

    #[test]
    fn splice_variable() {
        check(
            "(syntax-rules () [(_ e ...) (e ...)])",
            r"(splice #t #\a #f #\b)",
            expect![r"(#t #\a #f #\b)"],
        );
    }

    #[test]
    fn splice_list() {
        check(
            "(syntax-rules () [(_ (e) ...) ((e) ...)])",
            r"(splice (#t) (#f))",
            expect![r"((#t) (#f))"],
        );
    }

    #[test]
    fn splice_nested_list() {
        check(
            "(syntax-rules () [(_ ((e) ...) ...) (((e) ...) ...)])",
            r"(splice ((#t) (#f)) ((#\a)))",
            expect![r"(((#t) (#f)) ((#\a)))"],
        );
    }

    #[test]
    fn let_macro() {
        check(
            "(syntax-rules () [(_ ((v e) ...) body0 body ...) ((lambda (v ...) body0 body ...) e ...)])",
            r"(let ([x #\a] [y #\b]) x y)",
            expect![[r#"((lambda (x y) x y) #\a #\b)"#]],
        );
    }
}
