use std::{
    collections::{hash_map::Entry, HashMap},
    fmt,
    iter::Peekable,
    slice::Iter,
};

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
                    // TODO: Only allow one repeat in a pattern list
                    SynExp::Symbol(s) if s.value() == "..." => match patterns.pop() {
                        Some(p) => {
                            let pspan = p.span();
                            patterns.push(Pattern::Repeat(Box::new(p), pspan.extend(s.span())));
                        }
                        None => todo!(),
                    },
                    // TODO: Emit diagnostic for repeated variable
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

    pub fn match_(&self, syn: &SynExp) -> Option<HashMap<String, MatchVariable>> {
        match &self {
            // _ matches everything
            Pattern::Discard(_) => Some(HashMap::new()),
            // a variable matches everything and stores it in a variable
            Pattern::Variable(v) => Some(HashMap::from([(
                v.value().to_string(),
                MatchVariable::Variable(syn.clone()),
            )])),
            // a constant only matches itself
            Pattern::Constant(c) => match (c, syn) {
                (PatternConstant::Boolean(bl), SynExp::Boolean(br)) if bl.value() == br.value() => {
                    Some(HashMap::new())
                }
                (PatternConstant::Char(cl), SynExp::Char(cr)) if cl.value() == cr.value() => {
                    Some(HashMap::new())
                }
                _ => None,
            },
            Pattern::List(pl, _) => match syn {
                SynExp::List(sl) => Self::match_list(pl, sl),
                _ => None,
            },
            Pattern::Repeat(_, _) => panic!("must be inside a list"),
        }
    }

    fn match_list(pl: &[Pattern], sl: &SynList) -> Option<HashMap<String, MatchVariable>> {
        let mut vars = HashMap::new();
        let mut pats = pl.iter().peekable();
        // TODO: handle dot
        let mut syns = sl.sexps().iter().peekable();
        while let Some(pat) = pats.next() {
            if let Pattern::Repeat(pat, _) = pat {
                let p = pats.peek();
                match pat.match_repeat(&mut syns, |s| match p {
                    Some(Pattern::List(_, _)) => s.list().is_some(),
                    Some(Pattern::Constant(PatternConstant::Boolean(_))) => s.boolean().is_some(),
                    Some(Pattern::Constant(PatternConstant::Char(_))) => s.char().is_some(),
                    _ => false,
                }) {
                    Some(v) => {
                        vars.extend(v);
                    }
                    None => return None,
                }
            } else {
                match syns.next() {
                    Some(syn) => match pat.match_(syn) {
                        Some(v) => vars.extend(v),
                        None => return None,
                    },
                    // reached the end of input
                    None => return None,
                }
            }
        }

        if syns.next().is_none() {
            Some(vars)
        } else {
            None
        }
    }

    fn match_repeat(
        &self,
        syns: &mut Peekable<Iter<SynExp>>,
        stop: impl Fn(&SynExp) -> bool,
    ) -> Option<HashMap<String, MatchVariable>> {
        if let Some(syn) = syns.peek() {
            if stop(syn) {
                return None;
            }
        }

        match self {
            Pattern::Discard(_) => {
                while let Some(syn) = syns.peek() {
                    if stop(syn) {
                        return None;
                    }
                    syns.next();
                }
                Some(HashMap::new())
            }
            Pattern::Variable(v) => {
                let mut repeat = vec![];
                loop {
                    match syns.peek() {
                        Some(s) if stop(s) => break,
                        Some(_) => {}
                        None => break,
                    }

                    let syn = syns.next().unwrap();
                    repeat.push(MatchVariable::Variable(syn.clone()));
                }
                Some(HashMap::from([(
                    v.value().to_string(),
                    MatchVariable::List(repeat),
                )]))
            }
            Pattern::Constant(_) => todo!(),
            Pattern::List(pl, _) => {
                let mut vars = HashMap::new();
                loop {
                    match syns.peek() {
                        Some(s) if stop(s) => break,
                        Some(SynExp::List(sl)) => match Self::match_list(pl, sl) {
                            Some(res) => {
                                syns.next();
                                for (rk, rv) in res {
                                    match vars.entry(rk) {
                                        Entry::Occupied(mut o) => match o.get_mut() {
                                            MatchVariable::Variable(v) => {
                                                panic!("expected a list {v:?}")
                                            }
                                            MatchVariable::List(l) => l.push(rv),
                                        },
                                        Entry::Vacant(v) => {
                                            v.insert(MatchVariable::List(vec![rv]));
                                        }
                                    }
                                }
                            }
                            None => break,
                        },
                        _ => break,
                    }
                }
                Some(vars)
            }
            Pattern::Repeat(_, _) => todo!(),
        }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MatchVariable {
    Variable(SynExp),
    List(Vec<MatchVariable>),
}

impl MatchVariable {
    fn variable(&self) -> Option<&SynExp> {
        match self {
            MatchVariable::Variable(v) => Some(v),
            MatchVariable::List(_) => None,
        }
    }

    fn list(&self) -> Option<&[MatchVariable]> {
        match self {
            MatchVariable::Variable(_) => None,
            MatchVariable::List(l) => Some(l.as_ref()),
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
        expander::{core, scopes::Scopes},
        file::FileId,
        syntax::{ast::ModuleName, cst::SynRoot, parser::parse_str},
    };

    use super::*;

    fn root_env() -> Env<'static, String, Binding> {
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

        mod matcher {
            use super::*;

            fn check(
                pat: &str,
                input: &str,
                pred: impl Fn(HashMap<String, MatchVariable>) -> bool,
            ) {
                let pat_res = parse_str(pat);
                assert_eq!(pat_res.diagnostics, vec![]);
                let red = SynRoot::new(&pat_res.tree, FileId::default());
                let mut children = red.syn_children();
                let syn = children.next().expect("expected an item");
                let (pattern, errs) = compile_pattern(&syn);
                assert_eq!(errs, vec![]);

                let res = parse_str(input);
                assert_eq!(res.diagnostics, vec![]);
                let red = SynRoot::new(&res.tree, FileId::default());
                let mut children = red.syn_children();
                let syn = children.next().expect("expected an item");
                assert!(pred(
                    pattern.match_(&syn).expect("should have matched input")
                ));
            }

            #[test]
            fn wildcard() {
                check("_", "#t", |vars| vars.is_empty());
                check("_", "x", |vars| vars.is_empty());
                check("_", "()", |vars| vars.is_empty());
                check("_", "((x . y))", |vars| vars.is_empty());
            }

            #[test]
            fn constant() {
                check("#t", "#t", |vars| vars.is_empty());
                check("#f", "#F", |vars| vars.is_empty());
                check(r"#\a", r"#\a", |vars| vars.is_empty());
                check(r"#\λ", r"#\λ", |vars| vars.is_empty());
                check(r"#\λ", r"#\x3bb", |vars| vars.is_empty());
                check(r"#\x3bb", r"#\λ", |vars| vars.is_empty());
            }

            #[test]
            fn variable() {
                check("x", "#t", |vars| {
                    vars.len() == 1
                        && vars
                            .get("x")
                            .and_then(MatchVariable::variable)
                            .and_then(SynExp::boolean)
                            .map(SynBoolean::value)
                            .unwrap()
                });
                check("hello-world", r"#\space", |vars| {
                    vars.len() == 1
                        && vars
                            .get("hello-world")
                            .and_then(MatchVariable::variable)
                            .and_then(SynExp::char)
                            .map(SynChar::value)
                            .unwrap()
                            == ' '
                });
                check("x", "()", |vars| {
                    vars.len() == 1
                        && vars
                            .get("x")
                            .and_then(MatchVariable::variable)
                            .and_then(SynExp::list)
                            .map(|l| l.sexps().is_empty() && l.dot.is_none())
                            .unwrap()
                });
            }

            #[test]
            fn list() {
                check("(_)", "(#t)", |vars| vars.is_empty());
                check("(_)", r"(#\a)", |vars| vars.is_empty());
                check("(#t)", "(#t)", |vars| vars.is_empty());
                check("(#F)", "(#f)", |vars| vars.is_empty());
                check(r"(#\space)", r"(#\ )", |vars| vars.is_empty());
                check("(x)", "(#t)", |vars| {
                    vars.len() == 1
                        && vars
                            .get("x")
                            .and_then(MatchVariable::variable)
                            .and_then(SynExp::boolean)
                            .is_some()
                });
                check("(x y)", r"(#t #\a)", |vars| {
                    vars.len() == 2
                        && vars
                            .get("x")
                            .and_then(MatchVariable::variable)
                            .and_then(SynExp::boolean)
                            .map(SynBoolean::value)
                            .unwrap()
                        && vars
                            .get("y")
                            .and_then(MatchVariable::variable)
                            .and_then(SynExp::char)
                            .map(SynChar::value)
                            .unwrap()
                            == 'a'
                });
                check("(x (y) z)", r"(#t (#\a) e)", |vars| {
                    vars.len() == 3
                        && vars
                            .get("x")
                            .and_then(MatchVariable::variable)
                            .and_then(SynExp::boolean)
                            .map(SynBoolean::value)
                            .unwrap()
                        && vars
                            .get("y")
                            .and_then(MatchVariable::variable)
                            .and_then(SynExp::char)
                            .map(SynChar::value)
                            .unwrap()
                            == 'a'
                        && vars
                            .get("z")
                            .and_then(MatchVariable::variable)
                            .and_then(SynExp::symbol)
                            .map(SynSymbol::value)
                            .unwrap()
                            == "e"
                });
            }

            #[test]
            fn simple_repeat() {
                check("(e ...)", "()", |vars| {
                    vars.len() == 1
                        && vars.get("e").and_then(MatchVariable::list).unwrap().len() == 0
                });
                check("(e ...)", "(#t)", |vars| {
                    assert!(vars.len() == 1);
                    let e = vars.get("e").and_then(MatchVariable::list).unwrap();
                    assert!(e.len() == 1);
                    assert!(e[0].variable().and_then(SynExp::boolean).unwrap().value());
                    true
                });
                check("(e ...)", r"(#f (#\a x))", |vars| {
                    assert!(vars.len() == 1);
                    let e = vars.get("e").and_then(MatchVariable::list).unwrap();
                    assert!(e.len() == 2);
                    assert!(e[0].variable().and_then(SynExp::boolean).unwrap().value() == false);
                    let e1 = e[1].variable().and_then(SynExp::list).unwrap();
                    assert!(e1.dot().is_none());
                    let sexps = e1.sexps();
                    assert!(sexps.len() == 2);
                    assert!(sexps[0].char().unwrap().value() == 'a');
                    assert!(sexps[1].symbol().unwrap().value() == "x");
                    true
                });
            }

            #[test]
            fn list_repeat() {
                check("((e) ...)", "((#t) (#f))", |vars| {
                    assert_eq!(vars.len(), 1);
                    let el = vars.get("e").and_then(MatchVariable::list).unwrap();
                    assert_eq!(el.len(), 2);
                    assert_eq!(
                        el[0]
                            .variable()
                            .and_then(SynExp::boolean)
                            .map(SynBoolean::value)
                            .unwrap(),
                        true
                    );
                    assert_eq!(
                        el[1]
                            .variable()
                            .and_then(SynExp::boolean)
                            .map(SynBoolean::value)
                            .unwrap(),
                        false
                    );
                    true
                });
            }

            #[test]
            fn recursive_repeat() {
                check(
                    "((e ...) ...)",
                    r"((#\1 #\2) (#\3 #\4 #\5) () ((#t)))",
                    |vars| {
                        assert_eq!(vars.len(), 1);

                        let el = vars.get("e").and_then(MatchVariable::list).unwrap();
                        assert_eq!(el.len(), 4);

                        // (#\1 #\2)
                        let el0l = &el[0].list().unwrap();
                        assert_eq!(el0l.len(), 2);
                        assert_eq!(
                            el0l[0]
                                .variable()
                                .and_then(SynExp::char)
                                .map(SynChar::value),
                            Some('1'),
                        );
                        assert_eq!(
                            el0l[1]
                                .variable()
                                .and_then(SynExp::char)
                                .map(SynChar::value),
                            Some('2'),
                        );

                        let el1l = &el[1].list().unwrap();
                        assert_eq!(el1l.len(), 3);
                        assert_eq!(
                            el1l[0]
                                .variable()
                                .and_then(SynExp::char)
                                .map(SynChar::value),
                            Some('3'),
                        );
                        assert_eq!(
                            el1l[1]
                                .variable()
                                .and_then(SynExp::char)
                                .map(SynChar::value),
                            Some('4'),
                        );
                        assert_eq!(
                            el1l[2]
                                .variable()
                                .and_then(SynExp::char)
                                .map(SynChar::value),
                            Some('5'),
                        );

                        // ()
                        let el2l = &el[2].list().unwrap();
                        assert_eq!(el2l.len(), 0);

                        // (#t)
                        let el3l = &el[3].list().unwrap();
                        assert_eq!(el3l.len(), 1);
                        assert_eq!(
                            el3l[0]
                                .variable()
                                .and_then(SynExp::list)
                                .and_then(|l| {
                                    assert_eq!(l.dot(), None);
                                    assert_eq!(l.sexps().len(), 1);
                                    l.sexps()[0].boolean()
                                })
                                .map(SynBoolean::value),
                            Some(true)
                        );

                        true
                    },
                );
            }

            #[test]
            fn simple_let() {
                check(
                    "(let ((v e) ...) body0 body ...)",
                    r"(let ([x #t] [y #\a]) (display y) x)",
                    |vars| {
                        assert_eq!(vars.len(), 5);
                        assert_eq!(
                            vars.get("let")
                                .and_then(MatchVariable::variable)
                                .and_then(SynExp::symbol)
                                .map(SynSymbol::value),
                            Some("let")
                        );
                        let vl = vars.get("v").and_then(MatchVariable::list).unwrap();
                        assert_eq!(vl.len(), 2);

                        assert_eq!(
                            vl[0]
                                .variable()
                                .and_then(SynExp::symbol)
                                .map(SynSymbol::value),
                            Some("x")
                        );
                        assert_eq!(
                            vl[1]
                                .variable()
                                .and_then(SynExp::symbol)
                                .map(SynSymbol::value),
                            Some("y")
                        );

                        let el = vars.get("e").and_then(MatchVariable::list).unwrap();
                        assert_eq!(el.len(), 2);

                        assert_eq!(
                            el[0]
                                .variable()
                                .and_then(SynExp::boolean)
                                .map(SynBoolean::value),
                            Some(true),
                        );
                        assert_eq!(
                            el[1].variable().and_then(SynExp::char).map(SynChar::value),
                            Some('a')
                        );

                        assert_eq!(
                            vars.get("body0")
                                .and_then(MatchVariable::variable)
                                .and_then(SynExp::list)
                                .map(|l| l.to_string())
                                .as_deref(),
                            Some("(display y)")
                        );

                        let bodies = vars.get("body").and_then(MatchVariable::list).unwrap();
                        assert_eq!(bodies.len(), 1);

                        assert_eq!(
                            bodies[0]
                                .variable()
                                .and_then(SynExp::symbol)
                                .map(SynSymbol::value),
                            Some("x"),
                        );

                        true
                    },
                );
            }
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

        mod expand {
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
                let (pattern, errs) = compile_syntax_rules(&syn, &root_env());
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
                expected.assert_debug_eq(&pattern.expand(&mut expander, syn, &env));
            }

            #[test]
            #[ignore]
            fn simple_macro() {
                check("(syntax-rules () [(_) #t])", "(or)", expect![[""]]);
            }
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
