use std::rc::Rc;

use crate::{
    config::ParserConfig,
    diagnostics::{Diagnostic, DiagnosticBuilder},
    file::{FileId, SourceFile},
    span::Span,
};

use super::{
    cst::{Cst, CstKind, Delim, ListKind, OpenDelim, Prefix},
    scanner::{tokenize_str, Token},
    TokenKind as TK,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseResult {
    pub file_id: FileId,
    pub root: Vec<Rc<Cst>>,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn parse_str(input: &str) -> ParseResult {
    parse_str_with_config(input, FileId::default(), ParserConfig::default())
}

pub fn parse_source_file(file: &SourceFile, config: ParserConfig) -> ParseResult {
    parse_str_with_config(&file.contents, file.id, config)
}

pub fn parse_str_with_config(input: &str, file_id: FileId, config: ParserConfig) -> ParseResult {
    let mut p = Parser::new(input, config);
    p.file_id = file_id;
    p.parse();
    ParseResult {
        file_id,
        root: p.builder.finish(),
        diagnostics: std::mem::take(&mut p.diags),
    }
}

#[derive(Default)]
struct Parser<'input> {
    tokens: Vec<Token<'input>>,
    offset: usize,
    text_offset: usize,
    builder: CstBuilder,
    diags: Vec<Diagnostic>,
    delim_stack: Vec<(Delim, Span)>,
    file_id: FileId,
    config: ParserConfig,
    last_seen_token_span: Span,
}

impl<'input> Parser<'input> {
    fn new(input: &'input str, config: ParserConfig) -> Self {
        let tokens = tokenize_str(input);
        Self {
            tokens,
            config,
            ..Default::default()
        }
    }

    fn parse(&mut self) {
        loop {
            self.skip_trivia();
            if self.at_eof() {
                break;
            }

            self.datum(true);
        }
    }

    fn datum(&mut self, bump_close_delim: bool) {
        match &self.peek().kind {
            TK::OpenDelim => self.list(),
            TK::CloseDelim => {
                self.err_span("unexpected closing delimiter");
                if bump_close_delim {
                    self.bump();
                }
            }
            TK::SpecialOpenDelim => self.special_list(),
            TK::Quote
            | TK::Backtick
            | TK::Comma
            | TK::CommaAt
            | TK::HashQuote
            | TK::HashBacktick
            | TK::HashComma
            | TK::HashCommaAt => self.abbreviation(),
            TK::Dot => {
                self.err_span("expected a datum, found .");
                self.bump();
            }
            TK::True | TK::False | TK::Number | TK::Char | TK::Identifier | TK::String => {
                self.atom()
            }
            TK::Error => {
                let msg = format!("expected a datum, found {}", self.peek().source);
                self.err_span(msg);
                self.bump();
            }
            TK::HashSemicolon => self.datum_comment(),
            TK::Eof => {}
            _ => unreachable!(),
        }
    }

    fn atom(&mut self) {
        assert!(matches!(
            self.peek().kind,
            TK::True | TK::False | TK::Number | TK::Char | TK::Identifier | TK::String
        ));
        let file_id = self.file_id;
        let text_offset = self.text_offset;
        let t = self.peek_raw();
        match &t.kind {
            TK::Number => {}
            TK::Char => {
                if let Some(b) = validate_char(t.source) {
                    self.diags.push(b.span(self.peek_raw_span()).finish());
                }
            }
            TK::Identifier => {
                for b in validate_identifier(t.source, file_id, text_offset.try_into().unwrap()) {
                    self.diags.push(b.finish());
                }
            }
            TK::String => {}
            _ => {}
        }
        self.bump();
    }

    fn list(&mut self) {
        assert_eq!(self.peek().kind, TK::OpenDelim);
        self.open_delimiter();

        if self.parse_until(|tk| matches!(tk.kind, TK::CloseDelim | TK::Dot))
            && self.peek().kind == TK::Dot
        {
            self.dot_list_rest();
        }

        self.expect_close_delimiter();
    }

    fn special_list(&mut self) {
        assert_eq!(self.peek().kind, TK::SpecialOpenDelim);
        self.open_delimiter();
        self.parse_until(|tk| matches!(tk.kind, TK::CloseDelim | TK::Dot));
        // TODO: Recover from . in list
        self.expect_close_delimiter();
    }

    fn dot_list_rest(&mut self) {
        assert_eq!(self.peek().kind, TK::Dot);
        self.bump();
        self.datum(false);
        if self.peek().kind == TK::CloseDelim {
            return;
        }

        let msg = format!(
            "expected {}, found {}",
            self.delim_stack.last().unwrap().0.close(),
            self.peek().source
        );
        self.err_span(msg);

        loop {
            if self.peek().kind == TK::CloseDelim {
                return;
            }
            self.datum(false);
        }
    }

    fn abbreviation(&mut self) {
        let node = self.next_raw();
        let Cst {
            span,
            kind: CstKind::Prefix(p),
        } = node
        else {
            panic!("expected a prefix");
        };
        self.builder.start_abbrev(p, span);
        self.datum(false);
        self.builder.end_abbrev();
    }

    fn expect_close_delimiter(&mut self) {
        let (open_delim, open_delim_span) = self.delim_stack.pop().unwrap();
        let close_delim = {
            let t = self.peek();
            if t.kind == TK::CloseDelim {
                Delim::from(t.source)
            } else {
                let msg = format!("expected {}, found {}", open_delim.close(), t);
                self.emit_error(|b| {
                    b.msg(msg)
                        .show_after()
                        .related(vec![Diagnostic::with_builder(|b| {
                            b.hint().msg("unclosed delimiter").span(open_delim_span)
                        })])
                });
                self.bump();
                self.builder.end_unterminated_list();
                return;
            }
        };

        if open_delim.close() != close_delim {
            let msg = format!(
                "expected {}, found {}",
                open_delim.close(),
                close_delim.close()
            );
            self.err_sources(
                msg,
                vec![Diagnostic::builder()
                    .hint()
                    .msg("delimiter open here")
                    .span(open_delim_span)
                    .finish()],
            );
        }
        let span = self.next_raw().span;
        self.builder.end_list(close_delim, span);
    }

    fn parse_until(&mut self, pred: impl Fn(&Token) -> bool) -> bool {
        let mut parsed = false;
        while !self.at_eof() && !pred(self.peek()) {
            self.datum(false);
            parsed = true;
        }
        parsed
    }

    /// Starts a new list depending on the type of the open delimiter at the current position.
    fn open_delimiter(&mut self) {
        let lo = self.text_offset.try_into().unwrap();
        let node = self.next_raw();
        let len = node.text_len();
        let Cst {
            kind: CstKind::Delim(d),
            ..
        } = node
        else {
            panic!("expected an open delim")
        };
        if d.is_brace() && !self.config.braces {
            self.err_sources_consumed(
                "braces not supported",
                vec![Diagnostic::builder()
                    .hint()
                    .msg("enable braces with --extended-syntax / --allow-braces")
                    .finish()],
            );
        }
        let span = Span::new(self.file_id, lo, len.try_into().unwrap());
        self.delim_stack.push((d, span));
        self.builder.start_list(d, span);
    }

    fn datum_comment(&mut self) {
        assert_eq!(self.peek().kind, TK::HashSemicolon);
        let t = self.next_raw();
        self.builder.start_compound(t);
        self.datum(true);
        self.builder.end_datum_comment();
    }

    fn at_eof(&self) -> bool {
        (self.offset + 1) >= self.tokens.len()
    }

    fn peek_raw(&self) -> &Token {
        match self.tokens.get(self.offset) {
            Some(t) => t,
            _ => self.tokens.last().unwrap(),
        }
    }

    fn peek(&mut self) -> &Token {
        self.skip_trivia();
        self.peek_raw()
    }

    fn peek_raw_span(&self) -> Span {
        if self.at_eof() {
            self.last_seen_token_span
        } else {
            Span::new(
                self.file_id,
                self.text_offset.try_into().unwrap(),
                std::cmp::max(self.peek_raw().source.len(), 1)
                    .try_into()
                    .unwrap(),
            )
        }
    }

    fn next_raw(&mut self) -> Cst {
        match self.tokens.get_mut(self.offset) {
            Some(t) => {
                let t = std::mem::take(t);
                let start = self.text_offset;

                if !t.is_trivia() {
                    self.last_seen_token_span = Span::new(
                        self.file_id,
                        self.text_offset.try_into().unwrap(),
                        t.source.len().try_into().unwrap(),
                    );
                }
                self.offset += 1;
                self.text_offset += t.source.len();
                Cst {
                    span: Span::new(
                        self.file_id,
                        start.try_into().unwrap(),
                        t.source.len().try_into().unwrap(),
                    ),
                    kind: t.into(),
                }
            }
            _ => Cst {
                span: self.last_seen_token_span,
                kind: CstKind::Eof,
            },
        }
    }

    fn bump(&mut self) {
        let t = self.next_raw();
        if t.is_eof() {
            return;
        }
        self.builder.node(t);
    }

    fn skip_trivia(&mut self) {
        while self.peek_raw().is_trivia() {
            self.bump();
        }
    }

    fn emit_error(&mut self, builder: impl FnOnce(DiagnosticBuilder) -> DiagnosticBuilder) {
        self.diags.push(
            builder(Diagnostic::builder())
                .span(self.peek_raw_span())
                .finish(),
        );
    }

    fn err_span(&mut self, msg: impl Into<String>) {
        self.diags.push(
            Diagnostic::builder()
                .error()
                .msg(msg)
                .span(self.peek_raw_span())
                .finish(),
        );
    }

    fn err_sources(&mut self, msg: impl Into<String>, sources: Vec<Diagnostic>) {
        self.diags.push(
            Diagnostic::builder()
                .error()
                .msg(msg)
                .span(self.peek_raw_span())
                .related(sources)
                .finish(),
        );
    }

    fn err_sources_consumed(&mut self, msg: impl Into<String>, sources: Vec<Diagnostic>) {
        self.diags.push(
            Diagnostic::builder()
                .error()
                .msg(msg)
                .span(self.last_seen_token_span)
                .related(sources)
                .finish(),
        );
    }
}

#[derive(Default)]
struct CstBuilder {
    current: Vec<Rc<Cst>>,
    stack: Vec<Vec<Rc<Cst>>>,
}

impl CstBuilder {
    fn finish(self) -> Vec<Rc<Cst>> {
        self.current
    }

    fn start_compound(&mut self, cst: Cst) {
        self.stack.push(std::mem::take(&mut self.current));
        self.current.push(Rc::new(cst));
    }

    fn start_list(&mut self, delim: Delim, span: Span) {
        self.start_compound(Cst {
            span,
            kind: CstKind::Delim(delim),
        });
    }

    fn end_list(&mut self, delim: Delim, span: Span) {
        let mut list = std::mem::replace(
            &mut self.current,
            self.stack.pop().expect("must have pushed a list"),
        );
        list.push(Rc::new(Cst {
            span,
            kind: CstKind::Delim(delim),
        }));
        let list_kind = match list.first().unwrap().kind {
            CstKind::Delim(Delim::Open(OpenDelim::HashParen)) => ListKind::Vector,
            CstKind::Delim(Delim::Open(OpenDelim::BytevectorParen)) => ListKind::Bytevector,
            CstKind::Delim(Delim::Open(OpenDelim::Paren))
            | CstKind::Delim(Delim::Open(OpenDelim::Bracket))
            | CstKind::Delim(Delim::Open(OpenDelim::Brace)) => ListKind::List,
            _ => unreachable!(),
        };
        self.node(Cst {
            span: list.first().unwrap().span.extend(list.last().unwrap().span),
            kind: CstKind::List(list, list_kind),
        });
    }

    fn end_unterminated_list(&mut self) {
        let list = std::mem::replace(
            &mut self.current,
            self.stack.pop().expect("must have pushed a list"),
        );
        let list_kind = match list.first().unwrap().kind {
            CstKind::Delim(Delim::Open(OpenDelim::HashParen)) => ListKind::Vector,
            CstKind::Delim(Delim::Open(OpenDelim::BytevectorParen)) => ListKind::Bytevector,
            CstKind::Delim(Delim::Open(OpenDelim::Paren))
            | CstKind::Delim(Delim::Open(OpenDelim::Bracket))
            | CstKind::Delim(Delim::Open(OpenDelim::Brace)) => ListKind::List,
            _ => unreachable!(),
        };
        self.node(Cst {
            span: list.first().unwrap().span.extend(list.last().unwrap().span),
            kind: CstKind::List(list, list_kind),
        });
    }

    fn end_datum_comment(&mut self) {
        let list = std::mem::replace(
            &mut self.current,
            self.stack.pop().expect("must have pushed a list"),
        );
        self.node(Cst {
            span: list.first().unwrap().span.extend(list.last().unwrap().span),
            kind: CstKind::DatumComment(list),
        });
    }

    fn start_abbrev(&mut self, prefix: Prefix, span: Span) {
        self.stack.push(std::mem::take(&mut self.current));
        self.current.push(Rc::new(Cst {
            span,
            kind: CstKind::Prefix(prefix),
        }));
    }

    fn end_abbrev(&mut self) {
        let mut list = std::mem::replace(
            &mut self.current,
            self.stack.pop().expect("must have pushed a list"),
        );
        assert_eq!(list.len(), 2);
        let prefix = list.remove(0);
        let atom = list.remove(0);
        self.node(Cst {
            span: prefix.span.extend(atom.span),
            kind: CstKind::Abbreviation(prefix, atom),
        });
    }

    fn node(&mut self, node: Cst) {
        self.current.push(Rc::new(node));
    }
}

pub fn unicode_hex_escape_sequence(s: &str) -> Option<DiagnosticBuilder> {
    match u32::from_str_radix(s, 16) {
        Ok(n) => match char::from_u32(n) {
            Some(_) => None,
            None => Some(Diagnostic::builder().msg("invalid unicode hex escape sequence")),
        },
        Err(_) => Some(Diagnostic::builder().msg("invalid hex scalar value")),
    }
}

pub fn validate_identifier(s: &str, file_id: FileId, offset: u32) -> Vec<DiagnosticBuilder> {
    let mut diags = vec![];
    match s.chars().next() {
        c @ Some('+' | '.' | '-') if s.chars().nth(1) != Some('>') => match s {
            "+" | "..." | "-" => {}
            _ => {
                diags.push(
                    Diagnostic::builder()
                        .msg(format!(
                            "identifiers cannot begin with {}",
                            c.unwrap_or_default()
                        ))
                        .span(Span::new(file_id, offset, s.len().try_into().unwrap())),
                );
            }
        },
        _ => {}
    }

    let mut start = -1i64;
    let mut chars = s.chars();
    let mut current = 0;
    while let Some(c) = chars.next() {
        match c {
            '\\' => match chars.next().unwrap_or_default() {
                'x' => {
                    if start != -1 {
                        let start: u32 = start.try_into().unwrap();
                        diags.push(
                            Diagnostic::builder()
                                .msg("unfinished hex escape sequence")
                                .span(Span::new(
                                    file_id,
                                    offset + start,
                                    (current - start).try_into().unwrap(),
                                )),
                        );
                    }
                    start = i64::from(current);
                    current += 2;
                }
                c => {
                    diags.push(
                        Diagnostic::builder()
                            .msg("invalid character")
                            .span(Span::new(file_id, current, 1)),
                    );
                    current += c.len_utf8() as u32 + 1;
                }
            },
            ';' => {
                let lo: usize = start.try_into().unwrap();
                let new_current = current + c.len_utf8() as u32;
                if let Some(b) = unicode_hex_escape_sequence(&s[lo + 2..current as usize]) {
                    diags.push(b.span(Span::new(
                        file_id,
                        offset + lo as u32,
                        (new_current as usize - lo).try_into().unwrap(),
                    )));
                }
                current = new_current;
                start = -1;
            }
            c => {
                current += c.len_utf8() as u32;
            }
        }
    }

    if start != -1 {
        let lo: usize = start.try_into().unwrap();
        let offset = offset as usize + lo;
        diags.push(
            Diagnostic::builder()
                .msg("unterminated hex escape sequence")
                .span(Span::new(
                    file_id,
                    offset.try_into().unwrap(),
                    (s.len() - lo).try_into().unwrap(),
                )),
        );
    }

    diags
}

pub fn validate_char(s: &str) -> Option<DiagnosticBuilder> {
    assert!(s.starts_with("#\\"));
    let s = &s[2..];

    if s.chars().nth(1).is_none() {
        // Simple case, one character
        match s.chars().next() {
            Some(_) => None,
            None => Some(Diagnostic::builder().msg("expected a character, found <eof>")),
        }
    } else if let Some(stripped) = s.strip_prefix('x') {
        // Unicode characters
        unicode_hex_escape_sequence(stripped)
    } else {
        // Named character
        match s {
            "nul" | "alarm" | "backspace" | "tab" | "linefeed" | "newline" | "vtab" | "page"
            | "return" | "esc" | "space" | "delete" => None,
            _ => Some(Diagnostic::builder().msg("invalid character name")),
        }
    }
}

pub fn parse_unicode_hex_escape_sequence(s: &str) -> char {
    u32::from_str_radix(s, 16)
        .ok()
        .and_then(char::from_u32)
        .unwrap_or('\u{FFFD}')
}

pub fn parse_char(s: &str) -> char {
    if s.len() <= 2 {
        return '\0';
    }
    let s = &s[2..];

    if s.chars().nth(1).is_none() {
        // Simple case, one character
        s.chars().next().unwrap_or_default()
    } else if let Some(stripped) = s.strip_prefix('x') {
        // Unicode characters
        parse_unicode_hex_escape_sequence(stripped)
    } else {
        // Named character
        match s {
            "nul" => '\0',
            "alarm" => '\u{7}',
            "backspace" => '\u{8}',
            "tab" => '\u{9}',
            "linefeed" | "newline" => '\u{A}',
            "vtab" => '\u{B}',
            "page" => '\u{C}',
            "return" => '\u{D}',
            "esc" => '\u{1b}',
            "space" => '\u{20}',
            "delete" => '\u{7f}',
            _ => '\u{FFFD}',
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Number {
    Fixnum(usize),
}

impl Default for Number {
    fn default() -> Self {
        Self::Fixnum(0)
    }
}

pub fn parse_number(s: &str, span: Span) -> Result<Number, Diagnostic> {
    // TODO: everything
    match s.parse::<usize>() {
        Ok(n) => Ok(Number::Fixnum(n)),
        Err(_) => Err(Diagnostic::builder()
            .msg("invalid number")
            .span(span)
            .finish()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use expect_test::{expect, Expect};

    fn check(input: &str, expected: Expect) {
        let result = parse_str(input);
        assert_eq!(
            Vec::<Diagnostic>::default(),
            result.diagnostics,
            "{}",
            input
        );
        expected.assert_eq(
            &result
                .root
                .into_iter()
                .map(|c| format!("{c:#?}"))
                .collect::<Vec<_>>()
                .join("\n"),
        );
    }

    fn check_error(input: &str, errors: Vec<Diagnostic>, expected: Expect) {
        let result = parse_str(input);
        assert_eq!(errors, result.diagnostics, "{}", input);
        expected.assert_eq(
            &result
                .root
                .into_iter()
                .map(|c| format!("{c:#?}"))
                .collect::<Vec<_>>()
                .join("\n"),
        );
    }

    #[test]
    fn boolean() {
        check("#t", expect!["True@0:0..2"]);
        check("#T", expect!["True@0:0..2"]);
        check("#f", expect!["False@0:0..2"]);
        check("#F", expect!["False@0:0..2"]);
    }

    #[test]
    fn char() {
        check(r"#\a", expect![[r##"Char@0:0..3 "#\a""##]]);
        check(r"#\ ", expect![[r##"Char@0:0..3 "#\ ""##]]);
        check(r"#\x", expect![[r##"Char@0:0..3 "#\x""##]]);
        check(r"#\n", expect![[r##"Char@0:0..3 "#\n""##]]);
        check(r"#\x3bb", expect![[r##"Char@0:0..6 "#\x3bb""##]]);
        check(r"#\nul", expect![[r##"Char@0:0..5 "#\nul""##]]);
        check(r"#\alarm", expect![[r##"Char@0:0..7 "#\alarm""##]]);
        check(r"#\backspace", expect![[r##"Char@0:0..11 "#\backspace""##]]);
        check(r"#\tab", expect![[r##"Char@0:0..5 "#\tab""##]]);
        check(r"#\linefeed", expect![[r##"Char@0:0..10 "#\linefeed""##]]);
        check(r"#\newline", expect![[r##"Char@0:0..9 "#\newline""##]]);
        check(r"#\vtab", expect![[r##"Char@0:0..6 "#\vtab""##]]);
        check(r"#\page", expect![[r##"Char@0:0..6 "#\page""##]]);
        check(r"#\return", expect![[r##"Char@0:0..8 "#\return""##]]);
        check(r"#\esc", expect![[r##"Char@0:0..5 "#\esc""##]]);
        check(r"#\space", expect![[r##"Char@0:0..7 "#\space""##]]);
        check(r"#\delete", expect![[r##"Char@0:0..8 "#\delete""##]]);
    }

    #[test]
    fn number() {
        check(r"1", expect![[r#"Number@0:0..1 "1""#]]);
        check(r"#b0", expect![[r##"Number@0:0..3 "#b0""##]]);
        check(r"#B1", expect![[r##"Number@0:0..3 "#B1""##]]);
        check(r"#o0", expect![[r##"Number@0:0..3 "#o0""##]]);
        check(r"#O7", expect![[r##"Number@0:0..3 "#O7""##]]);
        check(r"#d0", expect![[r##"Number@0:0..3 "#d0""##]]);
        check(r"#D9", expect![[r##"Number@0:0..3 "#D9""##]]);
        check(r"#x0", expect![[r##"Number@0:0..3 "#x0""##]]);
        check(r"#XF", expect![[r##"Number@0:0..3 "#XF""##]]);
    }

    #[test]
    fn string() {
        check(r#""hello""#, expect![[r#"String@0:0..7 "hello""#]]);
        check(
            r#""\a \b \t \n \v \f \r \" \\ \x3bb;""#,
            expect![[r#"String@0:0..35 "\a \b \t \n \v \f \r \" \\ \x3bb;""#]],
        );
        check(
            "\"\\ \n \"",
            expect![[r#"
                String@0:0..6 "\ 
                 ""#]],
        );
    }

    #[test]
    fn identifier() {
        check(
            "hello-world",
            expect![[r#"Identifier@0:0..11 "hello-world""#]],
        );
        check("+", expect![[r#"Identifier@0:0..1 "+""#]]);
        check("-", expect![[r#"Identifier@0:0..1 "-""#]]);
        check("...", expect![[r#"Identifier@0:0..3 "...""#]]);
        check("->", expect![[r#"Identifier@0:0..2 "->""#]]);
        check(r"\x3bb;", expect![[r#"Identifier@0:0..6 "\x3bb;""#]]);
    }

    #[test]
    fn abbreviation() {
        check(
            "'a",
            expect![[r#"
                Abbreviation@0:0..2
                  Quote@0:0..1 "'"
                  Identifier@0:1..1 "a""#]],
        );
        check(
            "`#t",
            expect![[r#"
                Abbreviation@0:0..3
                  Backtick@0:0..1 "`"
                  True@0:1..2"#]],
        );
        check(
            r",#\a",
            expect![[r##"
                Abbreviation@0:0..4
                  Comma@0:0..1 ","
                  Char@0:1..3 "#\a""##]],
        );
        check(
            ",@#f",
            expect![[r#"
                Abbreviation@0:0..4
                  CommaAt@0:0..2 ",@"
                  False@0:2..2"#]],
        );
        check(
            r#"#'"Hi""#,
            expect![[r##"
                Abbreviation@0:0..6
                  HashQuote@0:0..2 "#'"
                  String@0:2..4 "Hi""##]],
        );
        check(
            "#`()",
            expect![[r##"
                Abbreviation@0:0..4
                  HashBacktick@0:0..2 "#`"
                  List@0:2..2
                    OpenDelim@0:2..1 "("
                    CloseDelim@0:3..1 ")""##]],
        );
        check(
            "#,[]",
            expect![[r##"
                Abbreviation@0:0..4
                  HashComma@0:0..2 "#,"
                  List@0:2..2
                    OpenDelim@0:2..1 "["
                    CloseDelim@0:3..1 "]""##]],
        );
        check(
            "#,@(a)",
            expect![[r##"
                Abbreviation@0:0..6
                  HashCommaAt@0:0..3 "#,@"
                  List@0:3..3
                    OpenDelim@0:3..1 "("
                    Identifier@0:4..1 "a"
                    CloseDelim@0:5..1 ")""##]],
        );
    }

    #[test]
    fn list() {
        check(
            "()",
            expect![[r#"
                List@0:0..2
                  OpenDelim@0:0..1 "("
                  CloseDelim@0:1..1 ")""#]],
        );
        check(
            "[]",
            expect![[r#"
                List@0:0..2
                  OpenDelim@0:0..1 "["
                  CloseDelim@0:1..1 "]""#]],
        );
        check(
            "(#t)",
            expect![[r#"
                List@0:0..4
                  OpenDelim@0:0..1 "("
                  True@0:1..2
                  CloseDelim@0:3..1 ")""#]],
        );
        check(
            "(#t . #f)",
            expect![[r#"
                List@0:0..9
                  OpenDelim@0:0..1 "("
                  True@0:1..2
                  Whitespace@0:3..1 " "
                  Dot@0:4..1
                  Whitespace@0:5..1 " "
                  False@0:6..2
                  CloseDelim@0:8..1 ")""#]],
        );
        check(
            r"(() (#t (#f) [] [#\a ]))",
            expect![[r##"
                List@0:0..24
                  OpenDelim@0:0..1 "("
                  List@0:1..2
                    OpenDelim@0:1..1 "("
                    CloseDelim@0:2..1 ")"
                  Whitespace@0:3..1 " "
                  List@0:4..19
                    OpenDelim@0:4..1 "("
                    True@0:5..2
                    Whitespace@0:7..1 " "
                    List@0:8..4
                      OpenDelim@0:8..1 "("
                      False@0:9..2
                      CloseDelim@0:11..1 ")"
                    Whitespace@0:12..1 " "
                    List@0:13..2
                      OpenDelim@0:13..1 "["
                      CloseDelim@0:14..1 "]"
                    Whitespace@0:15..1 " "
                    List@0:16..6
                      OpenDelim@0:16..1 "["
                      Char@0:17..3 "#\a"
                      Whitespace@0:20..1 " "
                      CloseDelim@0:21..1 "]"
                    CloseDelim@0:22..1 ")"
                  CloseDelim@0:23..1 ")""##]],
        );
    }

    #[test]
    fn vector() {
        check(
            "#()",
            expect![[r##"
                Vector@0:0..3
                  OpenDelim@0:0..2 "#("
                  CloseDelim@0:2..1 ")""##]],
        );
        check(
            "#vu8()",
            expect![[r##"
                Bytevector@0:0..6
                  OpenDelim@0:0..5 "#vu8("
                  CloseDelim@0:5..1 ")""##]],
        );
        check(
            r"#(#\a #() #(#t))",
            expect![[r##"
                Vector@0:0..16
                  OpenDelim@0:0..2 "#("
                  Char@0:2..3 "#\a"
                  Whitespace@0:5..1 " "
                  Vector@0:6..3
                    OpenDelim@0:6..2 "#("
                    CloseDelim@0:8..1 ")"
                  Whitespace@0:9..1 " "
                  Vector@0:10..5
                    OpenDelim@0:10..2 "#("
                    True@0:12..2
                    CloseDelim@0:14..1 ")"
                  CloseDelim@0:15..1 ")""##]],
        );
    }

    mod error_recovery {
        use super::*;

        #[test]
        fn delimiters() {
            check_error(
                ")",
                vec![Diagnostic::builder()
                    .msg("unexpected closing delimiter")
                    .span(Span::new(FileId::default(), 0, 1))
                    .finish()],
                expect![[r#"CloseDelim@0:0..1 ")""#]],
            );

            check_error(
                "(",
                vec![Diagnostic::builder()
                    .msg("expected ), found <eof>")
                    .span(Span::new(FileId::default(), 0, 1))
                    .show_after()
                    .related(vec![Diagnostic::builder()
                        .msg("unclosed delimiter")
                        .span(Span::new(FileId::default(), 0, 1))
                        .hint()
                        .finish()])
                    .finish()],
                expect![[r#"
                    List@0:0..1
                      OpenDelim@0:0..1 "(""#]],
            );
        }

        #[test]
        fn braces() {
            check_error(
                "{}",
                vec![Diagnostic::builder()
                    .error()
                    .msg("braces not supported")
                    .span(Span::new(FileId::default(), 0, 1))
                    .related(vec![Diagnostic::builder()
                        .hint()
                        .msg("enable braces with --extended-syntax / --allow-braces")
                        .finish()])
                    .finish()],
                expect![[r#"
                    List@0:0..2
                      OpenDelim@0:0..1 "{"
                      CloseDelim@0:1..1 "}""#]],
            );
        }

        #[test]
        fn characters() {
            check_error(
                r"#\",
                vec![Diagnostic::builder()
                    .error()
                    .msg("expected a character, found <eof>")
                    .span(Span::new(FileId::default(), 0, 2))
                    .finish()],
                expect![[r##"Char@0:0..2 "#\""##]],
            );

            check_error(
                r"#\Space",
                vec![Diagnostic::builder()
                    .error()
                    .msg("invalid character name")
                    .span(Span::new(FileId::default(), 0, 7))
                    .finish()],
                expect![[r##"Char@0:0..7 "#\Space""##]],
            );

            check_error(
                r"#\INVALID-CHAR-NAME",
                vec![Diagnostic::builder()
                    .error()
                    .msg("invalid character name")
                    .span(Span::new(FileId::default(), 0, 19))
                    .finish()],
                expect![[r##"Char@0:0..19 "#\INVALID-CHAR-NAME""##]],
            );

            check_error(
                r"#\xd800",
                vec![Diagnostic::builder()
                    .error()
                    .msg("invalid unicode hex escape sequence")
                    .span(Span::new(FileId::default(), 0, 7))
                    .finish()],
                expect![[r##"Char@0:0..7 "#\xd800""##]],
            );

            check_error(
                r"#\xdfff",
                vec![Diagnostic::builder()
                    .error()
                    .msg("invalid unicode hex escape sequence")
                    .span(Span::new(FileId::default(), 0, 7))
                    .finish()],
                expect![[r##"Char@0:0..7 "#\xdfff""##]],
            );

            check_error(
                r"#\x110000",
                vec![Diagnostic::builder()
                    .error()
                    .msg("invalid unicode hex escape sequence")
                    .span(Span::new(FileId::default(), 0, 9))
                    .finish()],
                expect![[r##"Char@0:0..9 "#\x110000""##]],
            );

            check_error(
                r"#\xQWERTY",
                vec![Diagnostic::builder()
                    .error()
                    .msg("invalid hex scalar value")
                    .span(Span::new(FileId::default(), 0, 9))
                    .finish()],
                expect![[r##"Char@0:0..9 "#\xQWERTY""##]],
            );
        }

        #[test]
        fn identifiers() {
            check_error(
                "+a",
                vec![Diagnostic::builder()
                    .error()
                    .msg("identifiers cannot begin with +")
                    .span(Span::new(FileId::default(), 0, 2))
                    .finish()],
                expect![[r##"Identifier@0:0..2 "+a""##]],
            );

            check_error(
                "-a",
                vec![Diagnostic::builder()
                    .error()
                    .msg("identifiers cannot begin with -")
                    .span(Span::new(FileId::default(), 0, 2))
                    .finish()],
                expect![[r##"Identifier@0:0..2 "-a""##]],
            );

            check_error(
                "..",
                vec![Diagnostic::builder()
                    .error()
                    .msg("identifiers cannot begin with .")
                    .span(Span::new(FileId::default(), 0, 2))
                    .finish()],
                expect![[r##"Identifier@0:0..2 "..""##]],
            );

            check_error(
                r"h\xq65;llo",
                vec![Diagnostic::builder()
                    .error()
                    .msg("invalid hex scalar value")
                    .span(Span::new(FileId::default(), 1, 6))
                    .finish()],
                expect![[r##"Identifier@0:0..10 "h\xq65;llo""##]],
            );

            check_error(
                r"h\xp;l\xq;lo",
                vec![
                    Diagnostic::builder()
                        .error()
                        .msg("invalid hex scalar value")
                        .span(Span::new(FileId::default(), 1, 4))
                        .finish(),
                    Diagnostic::builder()
                        .error()
                        .msg("invalid hex scalar value")
                        .span(Span::new(FileId::default(), 6, 4))
                        .finish(),
                ],
                expect![[r##"Identifier@0:0..12 "h\xp;l\xq;lo""##]],
            );

            check_error(
                r"\x3bb",
                vec![Diagnostic::builder()
                    .error()
                    .msg("unterminated hex escape sequence")
                    .span(Span::new(FileId::default(), 0, 5))
                    .finish()],
                expect![[r##"Identifier@0:0..5 "\x3bb""##]],
            );
        }

        //#[test]
        //fn strings() {
        //    check_error(
        //        r#""\q""#,
        //        vec![Diagnostic::builder()
        //            .error()
        //            .msg("invalid escape character")
        //            .span(Span::new(FileId::default(), 1, 2))
        //            .finish()],
        //        expect![[r##"Identifier@0..4 ""\q"""##]],
        //    );
        //}
    }

    mod extended_syntax {
        use super::*;

        fn check(input: &str, expected: Expect) {
            let result = parse_str_with_config(input, FileId::default(), ParserConfig::extended());
            assert_eq!(Vec::<Diagnostic>::default(), result.diagnostics);
            expected.assert_eq(
                &result
                    .root
                    .into_iter()
                    .map(|c| format!("{c:#?}"))
                    .collect::<Vec<_>>()
                    .join("\n"),
            );
        }

        #[test]
        fn braces() {
            check(
                "{}",
                expect![[r#"
                    List@0:0..2
                      OpenDelim@0:0..1 "{"
                      CloseDelim@0:1..1 "}""#]],
            );
        }
    }

    mod comments {
        use super::*;

        #[test]
        fn single_line() {
            check(
                ";hello world",
                expect![[r#"SingleComment@0:0..12 ";hello world""#]],
            );
            check(
                ";hi\n3",
                expect![[r#"
                    SingleComment@0:0..4 ";hi
                    "
                    Number@0:4..1 "3""#]],
            );
            check(
                ";hi\n;hello",
                expect![[r#"
                    SingleComment@0:0..4 ";hi
                    "
                    SingleComment@0:4..6 ";hello""#]],
            );
        }

        #[test]
        fn multi_line() {
            check(
                "#| hello |#",
                expect![[r##"MultiComment@0:0..11 "#| hello |#""##]],
            );
            check(
                "#| hello #| there |# general #| kenobi |#|#",
                expect![[
                    r##"MultiComment@0:0..43 "#| hello #| there |# general #| kenobi |#|#""##
                ]],
            );
            check(
                "(3 #| hi |# 4)",
                expect![[r##"
                    List@0:0..14
                      OpenDelim@0:0..1 "("
                      Number@0:1..1 "3"
                      Whitespace@0:2..1 " "
                      MultiComment@0:3..8 "#| hi |#"
                      Whitespace@0:11..1 " "
                      Number@0:12..1 "4"
                      CloseDelim@0:13..1 ")""##]],
            );
        }

        #[test]
        fn datum() {
            check(
                "#;3",
                expect![[r#"
                    DatumComment@0:0..3
                      HashSemicolon@0:0..2
                      Number@0:2..1 "3""#]],
            );
            check(
                r"#;3 #\a",
                expect![[r##"
                    DatumComment@0:0..3
                      HashSemicolon@0:0..2
                      Number@0:2..1 "3"
                    Whitespace@0:3..1 " "
                    Char@0:4..3 "#\a""##]],
            );
            check(
                "#;() 3",
                expect![[r#"
                    DatumComment@0:0..4
                      HashSemicolon@0:0..2
                      List@0:2..2
                        OpenDelim@0:2..1 "("
                        CloseDelim@0:3..1 ")"
                    Whitespace@0:4..1 " "
                    Number@0:5..1 "3""#]],
            );
            check(
                "#;(3 #;4) 3",
                expect![[r#"
                    DatumComment@0:0..9
                      HashSemicolon@0:0..2
                      List@0:2..7
                        OpenDelim@0:2..1 "("
                        Number@0:3..1 "3"
                        Whitespace@0:4..1 " "
                        DatumComment@0:5..3
                          HashSemicolon@0:5..2
                          Number@0:7..1 "4"
                        CloseDelim@0:8..1 ")"
                    Whitespace@0:9..1 " "
                    Number@0:10..1 "3""#]],
            );
            check(
                "#;#(3) 3",
                expect![[r##"
                    DatumComment@0:0..6
                      HashSemicolon@0:0..2
                      Vector@0:2..4
                        OpenDelim@0:2..2 "#("
                        Number@0:4..1 "3"
                        CloseDelim@0:5..1 ")"
                    Whitespace@0:6..1 " "
                    Number@0:7..1 "3""##]],
            );
        }

        #[test]
        fn combination() {
            check(
                "#;(3 #|hi|#)",
                expect![[r##"
                    DatumComment@0:0..12
                      HashSemicolon@0:0..2
                      List@0:2..10
                        OpenDelim@0:2..1 "("
                        Number@0:3..1 "3"
                        Whitespace@0:4..1 " "
                        MultiComment@0:5..6 "#|hi|#"
                        CloseDelim@0:11..1 ")""##]],
            );
        }
    }
}
