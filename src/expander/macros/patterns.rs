use std::{
    collections::{hash_map::Entry, HashMap},
    fmt,
    iter::Peekable,
    vec::IntoIter,
};

use crate::{
    diagnostics::Diagnostic,
    expander::macros::INDENTATION_WIDTH,
    span::Span,
    syntax::cst::{SynBoolean, SynChar, SynExp, SynList, SynSymbol},
};

pub fn compile_pattern(syn: &SynExp) -> (Pattern, Vec<Diagnostic>) {
    let mut diags = vec![];
    let p = do_compile_pattern(syn.clone(), &mut diags);
    (p, diags)
}

fn do_compile_pattern(syn: SynExp, _diags: &mut Vec<Diagnostic>) -> Pattern {
    match syn {
        SynExp::List(l) if l.sexps.is_empty() => Pattern::List(vec![], l.source_span()),
        SynExp::List(ps) => {
            let mut patterns: Vec<Pattern> = vec![];
            let span = ps.source_span();
            let (sexps, dot) = ps.into_parts();
            for p in sexps {
                match &p {
                    // TODO: Only allow one repeat in a pattern list
                    SynExp::Symbol(s) if s.value() == "..." => match patterns.pop() {
                        Some(p) => {
                            let pspan = p.span();
                            patterns
                                .push(Pattern::Repeat(Box::new(p), pspan.extend(s.source_span())));
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PatternConstant {
    Boolean(SynBoolean),
    Char(SynChar),
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Discard(s) => s.source_span(),
            Pattern::Variable(v) => v.source_span(),
            Pattern::Constant(c) => match c {
                PatternConstant::Boolean(b) => b.source_span(),
                PatternConstant::Char(c) => c.source_span(),
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
                        repeat.push((var.value().to_string(), var.source_span()));
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

    pub fn list(&self) -> Option<&[Pattern]> {
        match self {
            Pattern::List(l, _) => Some(l),
            _ => None,
        }
    }

    pub fn match_(&self, syn: SynExp) -> Result<HashMap<String, MatchVariable>, SynExp> {
        match &self {
            // _ matches everything
            Pattern::Discard(_) => Ok(HashMap::new()),
            // a variable matches everything and stores it in a variable
            Pattern::Variable(v) => Ok(HashMap::from([(
                v.value().to_string(),
                MatchVariable::Variable(syn.clone()),
            )])),
            // a constant only matches itself
            Pattern::Constant(c) => match (c, syn) {
                (PatternConstant::Boolean(bl), SynExp::Boolean(br)) if bl.value() == br.value() => {
                    Ok(HashMap::new())
                }
                (PatternConstant::Char(cl), SynExp::Char(cr)) if cl.value() == cr.value() => {
                    Ok(HashMap::new())
                }
                (_, syn) => Err(syn),
            },
            Pattern::List(pl, _) => match syn {
                SynExp::List(sl) => Self::match_list(pl, sl).map_err(SynExp::List),
                _ => Err(syn),
            },
            Pattern::Repeat(_, _) => panic!("must be inside a list"),
        }
    }

    pub fn match_list(
        pl: &[Pattern],
        sl: SynList,
    ) -> Result<HashMap<String, MatchVariable>, SynList> {
        let mut vars = HashMap::new();
        let mut pats = pl.iter().peekable();
        // TODO: Don't clone the whole thing
        let error = sl.clone();
        // TODO: handle dot
        let (syns, _) = sl.into_parts();
        let mut syns = syns.into_iter().peekable();
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
                    None => return Err(error),
                }
            } else {
                match syns.next() {
                    Some(syn) => match pat.match_(syn) {
                        Ok(v) => vars.extend(v),
                        _ => return Err(error),
                    },
                    // reached the end of input
                    None => return Err(error),
                }
            }
        }

        if syns.next().is_none() {
            Ok(vars)
        } else {
            Err(error)
        }
    }

    fn match_repeat(
        &self,
        syns: &mut Peekable<IntoIter<SynExp>>,
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
                        Some(SynExp::List(sl)) => {
                            let Ok(res) = Self::match_list(pl, sl.clone()) else {
                                break;
                            };

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
                        _ => break,
                    }
                }
                Some(vars)
            }
            Pattern::Repeat(_, _) => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MatchVariable {
    Variable(SynExp),
    List(Vec<MatchVariable>),
}

impl MatchVariable {
    pub fn variable(&self) -> Option<&SynExp> {
        match self {
            MatchVariable::Variable(v) => Some(v),
            MatchVariable::List(_) => None,
        }
    }

    pub fn list(&self) -> Option<&[MatchVariable]> {
        match self {
            MatchVariable::Variable(_) => None,
            MatchVariable::List(l) => Some(l.as_ref()),
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
                    write!(f, "{padding}_@{}", s.source_span())
                }
                Pattern::Variable(v) => {
                    write!(f, "{padding}{}@{}", v, v.source_span())
                }
                Pattern::Constant(c) => match c {
                    PatternConstant::Boolean(b) => {
                        write!(f, "{padding}{}@{}", b, b.source_span())
                    }
                    PatternConstant::Char(c) => {
                        write!(f, "{padding}{}@{}", c, c.source_span())
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

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::{
        file::FileId,
        syntax::{cst::SynRoot, parser::parse_str},
    };

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

        fn check(pat: &str, input: &str, pred: impl Fn(HashMap<String, MatchVariable>) -> bool) {
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
                pattern.match_(syn).expect("should have matched input")
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
                vars.len() == 1 && vars.get("e").and_then(MatchVariable::list).unwrap().len() == 0
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
