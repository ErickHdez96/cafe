use std::{collections::{hash_map::Entry, HashMap}, fmt, iter::Peekable, vec::IntoIter};

use crate::{new_expander::{macros::INDENTATION_WIDTH, syntax::{SynList, SynSymbol, Syntax}, SynSource}, new_syntax::cst::CstKind, span::Span};

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    /// _
    Discard(SynSymbol),
    /// `e`
    Variable(SynSymbol),
    /// `#t | 3`
    Constant(Syntax),
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
            Pattern::Constant(s) => s.span(),
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

    pub fn list(&self) -> Option<&[Pattern]> {
        match self {
            Pattern::List(l, _) => Some(l),
            _ => None,
        }
    }

    pub fn match_(&self, syn: Syntax) -> Result<HashMap<String, MatchVariable>, Syntax> {
        match &self {
            // _ matches everything
            Pattern::Discard(_) => Ok(HashMap::new()),
            // a variable matches everything and stores it in a variable
            Pattern::Variable(v) => Ok(HashMap::from([(
                v.value().to_string(),
                MatchVariable::Variable(syn.clone()),
            )])),
            // a constant only matches itself
            Pattern::Constant(c) => match (c.kind(), syn.kind()) {
                (CstKind::True, CstKind::True) => { Ok(HashMap::new()) }
                (CstKind::False, CstKind::False) => { Ok(HashMap::new()) }
                (CstKind::Char(cl), CstKind::Char(cr)) if cl == cr => {
                    Ok(HashMap::new())
                }
                (_, _) => Err(syn),
            },
            Pattern::List(pl, _) => match syn {
                Syntax::List(sl) => Self::match_list(pl, sl).map_err(Syntax::List),
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
        let source = SynSource::new(sl.value);
        //let (syns, _) = sl.into_parts();
        //let mut syns = syns.into_iter().peekable();
        while let Some(pat) = pats.next() {
            if let Pattern::Repeat(pat, _) = pat {
                let p = pats.peek();
                match pat.match_repeat(&mut source, |s| match p {
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
        syns: &mut SynSource,
        stop: impl Fn(&Syntax) -> bool,
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
                        Some(Syntax::List(sl)) => {
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
    Variable(Syntax),
    List(Vec<MatchVariable>),
}

impl MatchVariable {
    pub fn variable(&self) -> Option<&Syntax> {
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
                    write!(f, "{padding}_@{}", s.span())
                }
                Pattern::Variable(v) => {
                    write!(f, "{padding}{}@{}", v.source, v.span())
                }
                Pattern::Constant(s) => write!(f, "{padding}{}@{}", s.source(), s.span()),
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
