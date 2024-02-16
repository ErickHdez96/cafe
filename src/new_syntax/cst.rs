use std::fmt;

use crate::{span::Span, utils::Atom};

use super::scanner::Token;

const INDENTATION_WIDTH: usize = 2;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Cst {
    pub span: Span,
    pub kind: CstKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CstKind {
    List(Vec<Cst>, ListKind),
    Abbreviation(Box<Cst>, Box<Cst>),
    Prefix(Prefix),

    Delim(Delim),

    Dot,
    True,
    False,
    HashSemicolon,

    Ident(Atom),
    Char(Atom),
    Number(Atom),
    String(Atom),

    SingleComment(Atom),
    MultiComment(Atom),
    DatumComment(Vec<Cst>),
    Whitespace(Atom),
    Eof,
}

impl Cst {
    pub fn is_eof(&self) -> bool {
        matches!(self.kind, CstKind::Eof)
    }

    pub fn text_len(&self) -> usize {
        match &self.kind {
            CstKind::List(l, _) => l.iter().fold(0, |acc, a| acc + a.text_len()),
            CstKind::Abbreviation(prefix, atom) => prefix.text_len() + atom.text_len(),
            CstKind::Prefix(p) => p.text_len(),
            CstKind::Delim(d) => match d {
                Delim::Open(o) => match o {
                    OpenDelim::HashParen => 2,
                    OpenDelim::BytevectorParen => 5,
                    OpenDelim::Paren | OpenDelim::Bracket | OpenDelim::Brace => 1,
                },
                Delim::Close(_) => 1,
            },
            CstKind::Dot => 1,
            CstKind::True => 2,
            CstKind::False => 2,
            CstKind::HashSemicolon => 2,
            CstKind::DatumComment(l) => l.iter().fold(0, |acc, a| acc + a.text_len()),
            CstKind::Eof => 0,
            CstKind::Whitespace(s)
            | CstKind::Ident(s)
            | CstKind::Char(s)
            | CstKind::String(s)
            | CstKind::Number(s)
            | CstKind::SingleComment(s)
            | CstKind::MultiComment(s) => s.len(),
        }
    }

    pub fn is_trivia(&self) -> bool {
        matches!(self.kind, CstKind::Whitespace(_))
    }

    pub fn is_open_delim(&self) -> bool {
        matches!(self.kind, CstKind::Delim(Delim::Open(_)))
    }
}

impl From<Token<'_>> for CstKind {
    fn from(value: Token<'_>) -> Self {
        use super::SyntaxKind as SK;
        match value.kind {
            SK::OpenDelim | SK::SpecialOpenDelim => {
                CstKind::Delim(Delim::Open(match value.source {
                    "#(" => OpenDelim::HashParen,
                    "#vu8(" => OpenDelim::BytevectorParen,
                    "(" => OpenDelim::Paren,
                    "[" => OpenDelim::Bracket,
                    "{" => OpenDelim::Brace,
                    c => unreachable!("OpenDelim({c})"),
                }))
            }
            SK::CloseDelim => CstKind::Delim(Delim::Close(match value.source {
                ")" => CloseDelim::Paren,
                "]" => CloseDelim::Bracket,
                "}" => CloseDelim::Brace,
                c => unreachable!("CloseDelim({c})"),
            })),
            SK::Quote => CstKind::Prefix(Prefix::Quote),
            SK::Backtick => CstKind::Prefix(Prefix::Backtick),
            SK::Comma => CstKind::Prefix(Prefix::Comma),
            SK::CommaAt => CstKind::Prefix(Prefix::CommaAt),
            SK::Dot => CstKind::Dot,
            SK::HashQuote => CstKind::Prefix(Prefix::HashQuote),
            SK::HashBacktick => CstKind::Prefix(Prefix::HashBacktick),
            SK::HashComma => CstKind::Prefix(Prefix::HashComma),
            SK::HashCommaAt => CstKind::Prefix(Prefix::HashCommaAt),
            SK::True => CstKind::True,
            SK::False => CstKind::False,
            SK::Shebang => todo!(),
            SK::Number => CstKind::Number(value.source.into()),
            SK::Char => CstKind::Char(value.source.into()),
            SK::Identifier => CstKind::Ident(value.source.into()),
            SK::String => CstKind::String(value.source.into()),
            SK::SimpleComment => CstKind::SingleComment(value.source.into()),
            SK::MultiComment => CstKind::MultiComment(value.source.into()),
            SK::HashSemicolon => CstKind::HashSemicolon,
            SK::Whitespace => CstKind::Whitespace(value.source.into()),
            SK::Error => todo!(),
            SK::Eof => CstKind::Eof,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ListKind {
    List,
    Vector,
    Bytevector,
}

impl fmt::Display for ListKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ListKind::List => "List",
                ListKind::Vector => "Vector",
                ListKind::Bytevector => "Bytevector",
            }
        )
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Prefix {
    Quote,
    Backtick,
    Comma,
    CommaAt,
    HashQuote,
    HashBacktick,
    HashComma,
    HashCommaAt,
}

impl Prefix {
    pub fn text_len(&self) -> usize {
        match self {
            Prefix::Quote | Prefix::Backtick | Prefix::Comma => 1,
            Prefix::CommaAt | Prefix::HashQuote | Prefix::HashBacktick | Prefix::HashComma => 2,
            Prefix::HashCommaAt => 3,
        }
    }
}

impl fmt::Debug for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Prefix::Quote => "Quote",
                Prefix::Backtick => "Backtick",
                Prefix::Comma => "Comma",
                Prefix::CommaAt => "CommaAt",
                Prefix::HashQuote => "HashQuote",
                Prefix::HashBacktick => "HashBacktick",
                Prefix::HashComma => "HashComma",
                Prefix::HashCommaAt => "HashCommaAt",
            }
        )
    }
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Prefix::Quote => '\''.fmt(f),
            Prefix::Backtick => '`'.fmt(f),
            Prefix::Comma => ','.fmt(f),
            Prefix::CommaAt => ",@".fmt(f),
            Prefix::HashQuote => "#'".fmt(f),
            Prefix::HashBacktick => "#`".fmt(f),
            Prefix::HashComma => "#,".fmt(f),
            Prefix::HashCommaAt => "#,@".fmt(f),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Delim {
    Open(OpenDelim),
    Close(CloseDelim),
}

impl Delim {
    pub fn is_brace(self) -> bool {
        matches!(
            self,
            Delim::Open(OpenDelim::Brace) | Delim::Close(CloseDelim::Brace)
        )
    }

    pub fn close(self) -> Self {
        match self {
            Delim::Open(o) => match o {
                OpenDelim::HashParen => Self::Close(CloseDelim::Paren),
                OpenDelim::BytevectorParen => Self::Close(CloseDelim::Paren),
                OpenDelim::Paren => Self::Close(CloseDelim::Paren),
                OpenDelim::Bracket => Self::Close(CloseDelim::Bracket),
                OpenDelim::Brace => Self::Close(CloseDelim::Brace),
            },
            Delim::Close(_) => self,
        }
    }
}

impl From<&str> for Delim {
    fn from(value: &str) -> Self {
        match value {
            "(" => Delim::Open(OpenDelim::Paren),
            ")" => Delim::Close(CloseDelim::Paren),
            "[" => Delim::Open(OpenDelim::Bracket),
            "]" => Delim::Close(CloseDelim::Bracket),
            "{" => Delim::Open(OpenDelim::Brace),
            "}" => Delim::Close(CloseDelim::Brace),
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Delim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Delim::Open(o) => match o {
                OpenDelim::HashParen => "#(".fmt(f),
                OpenDelim::BytevectorParen => "#vu8(".fmt(f),
                OpenDelim::Paren => '('.fmt(f),
                OpenDelim::Bracket => '['.fmt(f),
                OpenDelim::Brace => '{'.fmt(f),
            },
            Delim::Close(c) => match c {
                CloseDelim::Paren => ')'.fmt(f),
                CloseDelim::Bracket => ']'.fmt(f),
                CloseDelim::Brace => '}'.fmt(f),
            },
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum OpenDelim {
    HashParen,
    BytevectorParen,
    Paren,
    Bracket,
    Brace,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum CloseDelim {
    Paren,
    Bracket,
    Brace,
}

impl fmt::Debug for Cst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let indentation = " ".repeat(width);
            let span = self.span;

            match &self.kind {
                CstKind::List(l, k) => write!(
                    f,
                    "{indentation}{k}@{span}\n{}",
                    l.iter()
                        .map(|a| format!("{a:#width$?}", width = width + INDENTATION_WIDTH))
                        .collect::<Vec<_>>()
                        .join("\n"),
                ),
                CstKind::Abbreviation(prefix, atom) => write!(
                    f,
                    "{indentation}Abbreviation@{span}\n{prefix:#width$?}\n{atom:#width$?}",
                    width = width + INDENTATION_WIDTH
                ),
                CstKind::Prefix(p) => write!(f, r#"{indentation}{p:?}@{span} "{p}""#,),
                CstKind::Delim(d) => write!(
                    f,
                    r#"{indentation}{}@{span} "{d}""#,
                    match d {
                        Delim::Open(_) => "OpenDelim",
                        Delim::Close(_) => "CloseDelim",
                    }
                ),
                CstKind::Dot => write!(f, "{indentation}Dot@{span}"),
                CstKind::True => write!(f, "{indentation}True@{span}"),
                CstKind::False => write!(f, "{indentation}False@{span}"),
                CstKind::HashSemicolon => write!(f, "{indentation}HashSemicolon@{span}"),
                CstKind::Ident(i) => write!(f, r#"{indentation}Identifier@{span} "{i}""#),
                CstKind::Char(c) => write!(f, r#"{indentation}Char@{span} "{c}""#),
                CstKind::String(s) => write!(f, "{indentation}String@{span} {s}"),
                CstKind::Number(n) => write!(f, r#"{indentation}Number@{span} "{n}""#),
                CstKind::SingleComment(c) => {
                    write!(f, r#"{indentation}SingleComment@{span} "{c}""#)
                }
                CstKind::MultiComment(c) => {
                    write!(f, r#"{indentation}MultiComment@{span} "{c}""#)
                }
                CstKind::DatumComment(l) => write!(
                    f,
                    "{indentation}DatumComment@{span}\n{}",
                    l.iter()
                        .map(|a| format!("{a:#width$?}", width = width + INDENTATION_WIDTH))
                        .collect::<Vec<_>>()
                        .join("\n"),
                ),
                CstKind::Whitespace(w) => write!(f, r#"{indentation}Whitespace@{span} "{w}""#),
                CstKind::Eof => todo!(),
            }
        } else {
            f.debug_struct("Cst")
                .field("span", &self.span)
                .field("kind", &self.kind)
                .finish()
        }
    }
}
