use std::{collections::HashMap, fmt};

use crate::{
    diagnostics::Diagnostic,
    env::Env,
    span::Span,
    syntax::cst::{SynBoolean, SynChar, SynExp, SynList, SynSymbol},
};

use super::{resolve, Binding, Expander};

const INDENTATION_WIDTH: usize = 2;

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
                                .msg(format!("expected {}, found {}", l.close_delim_char(), n))
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
                    .span(syn.span())
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

pub fn compile_pattern(syn: &SynExp) -> (Pattern, Vec<Diagnostic>) {
    let mut diags = vec![];
    let p = do_compile_pattern(syn.clone(), &mut diags);
    (p, diags)
}

fn do_compile_pattern(syn: SynExp, _diags: &mut Vec<Diagnostic>) -> Pattern {
    match syn {
        SynExp::List(l) if l.sexps.is_empty() => Pattern::List(vec![], l.span()),
        SynExp::List(ps) => {
            let mut patterns: Vec<Pattern> = vec![];
            let span = ps.span();
            let (sexps, dot) = ps.into_parts();
            for p in sexps {
                match &p {
                    SynExp::Symbol(s) if s.value() == "..." => match patterns.pop() {
                        Some(p) => {
                            let pspan = p.span();
                            patterns.push(Pattern::Repeat(Box::new(p), pspan.extend(s.span())));
                        }
                        None => todo!(),
                    },
                    _ => patterns.push(do_compile_pattern(p, _diags)),
                }
            }
            if let Some(dot) = dot {
                todo!("dot pattern: {dot:?}");
            }
            Pattern::List(patterns, span)
        }
        SynExp::Symbol(s) => match s.value() {
            "_" => Pattern::Discard(s),
            "..." => todo!("ellipsis without preceding pattern {s:?}"),
            _ => Pattern::Variable(s),
        },
        SynExp::Boolean(b) => Pattern::Constant(PatternConstant::Boolean(b)),
        SynExp::Char(c) => Pattern::Constant(PatternConstant::Char(c)),
    }
}

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
        SynExp::List(l) if l.sexps.is_empty() => Template::List(vec![], l.span()),
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
                        spans.push(ellipsis.span());
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
                        t = Template::Splice(Box::new(t), s.span().extend(span));
                        new_unrepeat -= 1;
                    }
                    validate_splice(&t, vars, diags, unrepeat, false);
                    templates.push(t);
                }
            }

            Template::List(templates, syn.span())
        }
        SynExp::Symbol(v) => {
            match vars.get(v.value()) {
                Some(&repeat) if repeat <= unrepeat => {
                    Template::Variable(v.value().to_string(), v.span())
                }
                Some(_) => {
                    diags.push(
                        Diagnostic::builder()
                            .msg(format!(
                                "variable {} still repeating at this level",
                                v.value()
                            ))
                            .span(v.span())
                            .finish(),
                    );
                    Template::Error(syn.clone(), v.span())
                }
                // TODO: Validate in environment
                None => {
                    let binding = resolve(v, env);
                    if let Some(b) = binding {
                        Template::Binding(b.clone(), v.span())
                    } else {
                        Template::Constant(syn.clone())
                    }
                }
            }
        }
        SynExp::Boolean(_) | SynExp::Char(_) => Template::Constant(syn.clone()),
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
    match &template {
        // direct splice operator over variables `e ...`
        Template::Variable(v, span) if !in_list => {
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
        Template::Variable(v, _) => {
            if let Some(&repeat) = vars.get(v) {
                // Variables that are still repeating are handled above
                repeat >= unrepeat
            } else {
                false
            }
        }
        Template::List(elems, span) => {
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
        Template::Splice(t, _) => validate_splice(t, vars, diags, unrepeat + 1, false),
        Template::Binding(_, _) => false,
        Template::Constant(_) => false,
        Template::Error(_, _) => false,
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct NativeSyntaxTransformer {
    rules: Vec<(Pattern, Template)>,
}

impl NativeSyntaxTransformer {
    pub fn expand(
        &self,
        _expander: &mut Expander,
        _syn: SynList,
        _env: &Env<String, Binding>,
    ) -> SynExp {
        todo!()
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CapturedVariable {
    Simple(SynExp),
    Repeat(Box<CapturedVariable>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PatternConstant {
    Boolean(SynBoolean),
    Char(SynChar),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    /// _
    Discard(SynSymbol),
    /// `e`
    Variable(SynSymbol),
    /// `#t | 3`
    Constant(PatternConstant),
    /// `( a b c )`
    List(Vec<Pattern>, Span),
    /// `e ... | (e) ...`
    Repeat(Box<Pattern>, Span),
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Discard(s) => s.span(),
            Pattern::Variable(v) => v.span(),
            Pattern::Constant(c) => match c {
                PatternConstant::Boolean(b) => b.span(),
                PatternConstant::Char(c) => c.span(),
            },
            Pattern::List(_, span) | Pattern::Repeat(_, span) => *span,
        }
    }

    pub fn variables(&self) -> (HashMap<String, usize>, Vec<(String, Span)>) {
        fn do_variables(
            p: &Pattern,
            vars: &mut HashMap<String, usize>,
            repeat: &mut Vec<(String, Span)>,
            depth: usize,
        ) {
            match p {
                Pattern::Variable(var) => match vars.get(var.value()) {
                    Some(_) => {
                        repeat.push((var.value().to_string(), var.span()));
                    }
                    None => {
                        vars.insert(var.value().to_string(), depth);
                    }
                },
                Pattern::List(l, _) => {
                    for e in l {
                        do_variables(e, vars, repeat, depth);
                    }
                }
                Pattern::Repeat(p, _) => do_variables(p, vars, repeat, depth + 1),
                _ => {}
            }
        }

        let mut vars = HashMap::default();
        let mut repeat = vec![];

        do_variables(self, &mut vars, &mut repeat, 0);
        (vars, repeat)
    }
}

impl fmt::Debug for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let padding = " ".repeat(width);
            match self {
                Pattern::Discard(s) => {
                    write!(f, "{padding}_@{}", s.span())
                }
                Pattern::Variable(v) => {
                    write!(f, "{padding}{}@{}", v, v.span())
                }
                Pattern::Constant(c) => match c {
                    PatternConstant::Boolean(b) => {
                        write!(f, "{padding}{}@{}", b, b.span())
                    }
                    PatternConstant::Char(c) => {
                        write!(f, "{padding}{}@{}", c, c.span())
                    }
                },
                Pattern::List(c, span) => write!(
                    f,
                    "{padding}list@{span}{}",
                    if c.is_empty() {
                        String::new()
                    } else {
                        format!(
                            "\n{}",
                            c.iter()
                                .map(|p| format!("{p:#width$?}", width = width + INDENTATION_WIDTH))
                                .collect::<Vec<_>>()
                                .join("\n")
                        )
                    }
                ),
                Pattern::Repeat(p, span) => write!(
                    f,
                    "{padding}repeat@{span}\n{p:#width$?}",
                    width = width + INDENTATION_WIDTH
                ),
            }
        } else {
            match self {
                Pattern::Discard(s) => f.debug_tuple("Pattern::Discard").field(&s).finish(),
                Pattern::Variable(v) => f.debug_tuple("Pattern::Variable").field(&v).finish(),
                Pattern::Constant(c) => f.debug_tuple("Pattern::Constant").field(&c).finish(),
                Pattern::List(c, span) => f
                    .debug_tuple("Pattern::Compound")
                    .field(&c)
                    .field(&span)
                    .finish(),
                Pattern::Repeat(p, span) => f
                    .debug_tuple("Pattern::Repeat")
                    .field(&p)
                    .field(&span)
                    .finish(),
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Template {
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

impl fmt::Debug for Template {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let padding = " ".repeat(width);
            match self {
                Template::Constant(c) => write!(f, "{padding}constant - {c} @{}", c.span()),
                Template::Variable(v, span) => write!(f, "{padding}variable - {v} @{span}"),
                Template::Binding(b, span) => {
                    write!(f, "{padding}captured-binding - {} @{span}", b.name())
                }
                Template::List(templs, span) => write!(
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
                Template::Splice(t, span) => write!(
                    f,
                    "{padding}splice@{span}\n{t:#width$?}",
                    width = width + INDENTATION_WIDTH
                ),
                Template::Error(source, span) => write!(f, "{padding}error - {source}@{span}",),
            }
        } else {
            match self {
                Template::Constant(c) => f.debug_tuple("Template::Constant").field(&c).finish(),
                Template::Variable(v, span) => f
                    .debug_tuple("Template::Variable")
                    .field(&v)
                    .field(&span)
                    .finish(),
                Template::Binding(b, span) => f
                    .debug_tuple("Template::Binding")
                    .field(&b)
                    .field(&span)
                    .finish(),
                Template::List(elems, span) => f
                    .debug_tuple("Template::List")
                    .field(&elems)
                    .field(&span)
                    .finish(),
                Template::Splice(t, span) => f
                    .debug_tuple("Template::Splice")
                    .field(&t)
                    .field(&span)
                    .finish(),
                Template::Error(source, span) => f
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
            scopes::Scopes,
            transformers::{if_core_transformer, lambda_core_transformer},
        },
        file::FileId,
        syntax::{cst::SynRoot, parser::parse_str},
    };

    use super::*;

    fn root_env() -> Env<'static, String, Binding> {
        let mut core = Env::new();
        core.insert(
            String::from("lambda"),
            Binding::CoreExprTransformer {
                scopes: Scopes::core(),
                name: String::from("lambda"),
                transformer: lambda_core_transformer,
            },
        );
        core.insert(
            String::from("if"),
            Binding::CoreExprTransformer {
                scopes: Scopes::core(),
                name: String::from("if"),
                transformer: if_core_transformer,
            },
        );
        core.insert(
            String::from("cons"),
            Binding::Value {
                scopes: Scopes::core(),
                orig_name: String::from("cons"),
                name: None,
            },
        );
        core.enter_consume()
    }

    mod patterns {
        use super::*;

        fn check(input: &str, expected: Expect) {
            let res = parse_str(input);
            assert_eq!(res.diagnostics, vec![]);
            let red = SynRoot::new(&res.tree, FileId::default());
            let mut children = red.syn_children();
            let (pattern, errs) = compile_pattern(&children.next().expect("expected an item"));
            assert_eq!(errs, vec![]);
            expected.assert_debug_eq(&pattern);
        }

        #[test]
        fn wildcard() {
            check(
                "_",
                expect![[r#"
                    _@0:0..1
                "#]],
            );
        }

        #[test]
        fn identifier() {
            check(
                "e",
                expect![[r#"
                    e@0:0..1
                "#]],
            );
            check(
                "e",
                expect![[r#"
                    e@0:0..1
                "#]],
            );
        }

        #[test]
        fn char() {
            check(
                r"#\a",
                expect![[r#"
                    #\a@0:0..3
                "#]],
            );
            check(
                r"#\x3bb",
                expect![[r#"
                    #\x3bb@0:0..6
                "#]],
            );
            check(
                r"#\λ",
                expect![[r#"
                    #\λ@0:0..4
                "#]],
            );
            check(
                r"#\space",
                expect![[r#"
                    #\space@0:0..7
                "#]],
            );
        }

        #[test]
        fn boolean() {
            check(
                r"#t",
                expect![[r#"
                    #t@0:0..2
                "#]],
            );
            check(
                r"#f",
                expect![[r#"
                    #f@0:0..2
                "#]],
            );
        }

        #[test]
        fn list() {
            check(
                r"()",
                expect![[r#"
                    list@0:0..2
                "#]],
            );
            check(
                r"(#t)",
                expect![[r#"
                    list@0:0..4
                      #t@0:1..2
                "#]],
            );
            check(
                r"(let)",
                expect![[r#"
                    list@0:0..5
                      let@0:1..3
                "#]],
            );
            check(
                r"((x) (y))",
                expect![[r#"
                    list@0:0..9
                      list@0:1..3
                        x@0:2..1
                      list@0:5..3
                        y@0:6..1
                "#]],
            );
        }

        #[test]
        fn repeat() {
            check(
                r"(e ...)",
                expect![[r#"
                    list@0:0..7
                      repeat@0:1..5
                        e@0:1..1
                "#]],
            );
            check(
                r"(let ((v e) ...) body0 body ...)",
                expect![[r#"
                    list@0:0..32
                      let@0:1..3
                      list@0:5..11
                        repeat@0:6..9
                          list@0:6..5
                            v@0:7..1
                            e@0:9..1
                      body0@0:17..5
                      repeat@0:23..8
                        body@0:23..4
                "#]],
            );
            check(
                r"((a ...) ...)",
                expect![[r#"
                    list@0:0..13
                      repeat@0:1..11
                        list@0:1..7
                          repeat@0:2..5
                            a@0:2..1
                "#]],
            );
        }
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
