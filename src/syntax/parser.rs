use std::rc::Rc;

use crate::{
    config::ParserConfig,
    diagnostics::{Diagnostic, DiagnosticBuilder},
    file::SourceFile,
    parse_tree::{GreenNodeBuilder, GreenTree},
    span::Span,
};

use super::{
    scanner::{tokenize_str, Token},
    SyntaxKind,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseResult {
    pub tree: Rc<GreenTree>,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn parse_str(input: &str) -> ParseResult {
    parse_str_with_config(input, 0, ParserConfig::default())
}

pub fn parse_source_file(file: &SourceFile, config: ParserConfig) -> ParseResult {
    parse_str_with_config(&file.contents, file.id.value(), config)
}

pub fn parse_str_with_config(input: &str, file_id: u16, config: ParserConfig) -> ParseResult {
    let mut p = Parser::new(input, config);
    p.file_id = file_id;
    p.parse();
    ParseResult {
        tree: p.builder.finish(),
        diagnostics: std::mem::take(&mut p.diags),
    }
}

#[derive(Default)]
struct Parser<'input> {
    tokens: Vec<Token<'input>>,
    offset: usize,
    text_offset: usize,
    builder: GreenNodeBuilder,
    diags: Vec<Diagnostic>,
    delim_stack: Vec<(Delim, Span)>,
    file_id: u16,
    config: ParserConfig,
    last_seen_token_span: Span,
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Delim {
    Paren,
    Bracket,
    Brace,
}

impl Delim {
    pub fn close(self) -> char {
        match self {
            Delim::Paren => ')',
            Delim::Bracket => ']',
            Delim::Brace => '}',
        }
    }
}

impl From<&str> for Delim {
    fn from(value: &str) -> Self {
        match value.chars().last().unwrap() {
            '(' | ')' => Self::Paren,
            '[' | ']' => Self::Bracket,
            '{' | '}' => Self::Brace,
            _ => unreachable!(),
        }
    }
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
        self.builder.start_node(SyntaxKind::Root);

        loop {
            self.skip_trivia();
            if self.at_eof() {
                break;
            }

            self.datum();
        }
    }

    fn datum(&mut self) {
        match &self.peek().kind {
            SyntaxKind::OpenDelim => self.list(),
            SyntaxKind::CloseDelim => {
                self.err_span("unexpected closing delimiter");
                self.bump();
            }
            SyntaxKind::SpecialOpenDelim => self.special_list(),
            SyntaxKind::Quote
            | SyntaxKind::Backtick
            | SyntaxKind::Comma
            | SyntaxKind::CommaAt
            | SyntaxKind::HashQuote
            | SyntaxKind::HashBacktick
            | SyntaxKind::HashComma
            | SyntaxKind::HashCommaAt => self.abbreviation(),
            SyntaxKind::Dot => {
                self.err_span("expected a datum, found .");
                self.bump();
            }
            SyntaxKind::True
            | SyntaxKind::False
            | SyntaxKind::Number
            | SyntaxKind::Char
            | SyntaxKind::Identifier
            | SyntaxKind::String => self.atom(),
            SyntaxKind::Error => {
                let msg = format!("expected a datum, found {}", self.peek().source);
                self.err_span(msg);
                self.bump();
            }
            SyntaxKind::Eof => {}
            _ => unreachable!(),
        }
    }

    fn atom(&mut self) {
        self.builder.start_node(SyntaxKind::Atom);
        self.peek();
        let file_id = self.file_id;
        let text_offset = self.text_offset;
        let t = self.peek_raw();
        match &t.kind {
            SyntaxKind::Number => {}
            SyntaxKind::Char => {
                if let Some(b) = validate_char(t.source) {
                    self.diags.push(b.span(self.peek_raw_span()).finish());
                }
            }
            SyntaxKind::Identifier => {
                for b in validate_identifier(t.source, file_id, text_offset.try_into().unwrap()) {
                    self.diags.push(b.finish());
                }
            }
            SyntaxKind::String => {}
            _ => {}
        }
        self.bump();
        self.builder.finish_node();
    }

    fn list(&mut self) {
        self.push_delimiter();
        self.builder.start_node(SyntaxKind::List);
        self.bump();

        if self.parse_until(|tk| matches!(tk.kind, SyntaxKind::CloseDelim | SyntaxKind::Dot))
            && self.peek().kind == SyntaxKind::Dot
        {
            self.dot_list();
        }

        self.expect_close_delimiter();
        self.builder.finish_node();
    }

    fn dot_list(&mut self) {
        assert_eq!(self.peek().kind, SyntaxKind::Dot);
        self.bump();
        self.datum();
        if self.peek().kind == SyntaxKind::CloseDelim {
            return;
        }

        let msg = format!(
            "expected {}, found {}",
            self.delim_stack.last().unwrap().0.close(),
            self.peek().source
        );
        self.err_span(msg);

        loop {
            if self.peek().kind == SyntaxKind::CloseDelim {
                return;
            }
            self.datum();
        }
    }

    fn abbreviation(&mut self) {
        assert!(self.peek().kind.is_abbrev());
        self.builder.start_node(SyntaxKind::Abbreviation);
        self.bump();
        self.datum();
        self.builder.finish_node();
    }

    fn special_list(&mut self) {
        self.push_delimiter();

        let token = self.peek_raw();
        if token.source.to_ascii_lowercase().starts_with("#vu8") {
            if token.source != "#vu8(" {
                self.err_span("invalid bytevector prefix, expected #vu8(");
            }
            self.builder.start_node(SyntaxKind::Bytevector);
        } else {
            if token.source != "#(" {
                self.err_span("invalid vector prefix, expected #(");
            }
            self.builder.start_node(SyntaxKind::Vector);
        }
        self.bump();

        self.parse_until(|tk| matches!(tk.kind, SyntaxKind::CloseDelim | SyntaxKind::Dot));

        self.expect_close_delimiter();
        self.builder.finish_node();
    }

    fn expect_close_delimiter(&mut self) {
        let (open_delim, open_delim_span) = self.delim_stack.pop().unwrap();
        let close_delim = {
            let t = self.peek();
            match &t.kind {
                SyntaxKind::CloseDelim => Delim::from(t.source),
                _ => {
                    let msg = format!("expected {}, found {}", open_delim.close(), t);
                    self.err_sources(
                        msg,
                        vec![Diagnostic::builder()
                            .hint()
                            .msg("unclosed delimiter")
                            .span(open_delim_span)
                            .finish()],
                    );
                    self.bump();
                    return;
                }
            }
        };

        if open_delim != close_delim {
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
        self.bump();
    }

    fn parse_until(&mut self, pred: impl Fn(&Token) -> bool) -> bool {
        let mut parsed = false;
        while !self.at_eof() && !pred(self.peek()) {
            self.datum();
            parsed = true;
        }
        parsed
    }

    fn push_delimiter(&mut self) {
        let token = self.peek_raw();
        assert!(matches!(
            token.kind,
            SyntaxKind::OpenDelim | SyntaxKind::SpecialOpenDelim
        ));
        let delim = Delim::from(token.source);
        let len = token.source.len().try_into().unwrap();
        if delim == Delim::Brace && !self.config.braces {
            self.err_sources(
                "braces not supported",
                vec![Diagnostic::builder()
                    .hint()
                    .msg("enable braces with --extended-syntax / --allow-braces")
                    .finish()],
            );
        }
        self.delim_stack.push((
            delim,
            Span::new(self.file_id, self.text_offset.try_into().unwrap(), len),
        ));
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

    fn next_raw(&mut self) -> Token {
        match self.tokens.get_mut(self.offset) {
            Some(t) => {
                let t = std::mem::take(t);
                self.offset += 1;
                self.text_offset += t.source.len();
                t
            }
            _ => Token {
                kind: SyntaxKind::Eof,
                ..Default::default()
            },
        }
    }

    fn bump(&mut self) {
        let lo = self.text_offset;
        let t = self.next_raw();
        let kind = t.kind;
        if kind.is_eof() {
            return;
        }
        let text = t.source.to_string();

        if !kind.is_trivia() && !kind.is_eof() {
            self.last_seen_token_span = Span::new(
                self.file_id,
                lo.try_into().unwrap(),
                text.len().try_into().unwrap(),
            );
        }

        self.builder.push_token(kind, text);
    }

    fn skip_trivia(&mut self) {
        while self.peek_raw().is_trivia() {
            self.bump();
        }
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
                .sources(sources)
                .finish(),
        );
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

pub fn validate_identifier(s: &str, file_id: u16, offset: u32) -> Vec<DiagnosticBuilder> {
    let mut diags = vec![];
    match s.chars().next() {
        c @ Some('+') | c @ Some('.') | c @ Some('-') if s.chars().nth(1) != Some('>') => match s {
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
                    start = current as i64;
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
            "nul" => None,
            "alarm" => None,
            "backspace" => None,
            "tab" => None,
            "linefeed" => None,
            "newline" => None,
            "vtab" => None,
            "page" => None,
            "return" => None,
            "esc" => None,
            "space" => None,
            "delete" => None,
            _ => Some(Diagnostic::builder().msg("invalid character name")),
        }
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
        expected.assert_debug_eq(&result.tree);
    }

    fn check_error(input: &str, errors: Vec<Diagnostic>, expected: Expect) {
        let result = parse_str(input);
        assert_eq!(errors, result.diagnostics, "{}", input);
        expected.assert_debug_eq(&result.tree);
    }

    #[test]
    fn boolean() {
        check(
            "#t",
            expect![[r##"
                Root@0..2
                  Atom@0..2
                    True@0..2 "#t"
            "##]],
        );
        check(
            "#T",
            expect![[r##"
                Root@0..2
                  Atom@0..2
                    True@0..2 "#T"
            "##]],
        );
        check(
            "#f",
            expect![[r##"
                Root@0..2
                  Atom@0..2
                    False@0..2 "#f"
            "##]],
        );
        check(
            "#F",
            expect![[r##"
                Root@0..2
                  Atom@0..2
                    False@0..2 "#F"
            "##]],
        );
    }

    #[test]
    fn char() {
        check(
            r"#\a",
            expect![[r##"
                Root@0..3
                  Atom@0..3
                    Char@0..3 "#\a"
            "##]],
        );
        check(
            r"#\ ",
            expect![[r##"
                Root@0..3
                  Atom@0..3
                    Char@0..3 "#\ "
            "##]],
        );
        check(
            r"#\x",
            expect![[r##"
                Root@0..3
                  Atom@0..3
                    Char@0..3 "#\x"
            "##]],
        );
        check(
            r"#\n",
            expect![[r##"
                Root@0..3
                  Atom@0..3
                    Char@0..3 "#\n"
            "##]],
        );
        check(
            r"#\x3bb",
            expect![[r##"
                Root@0..6
                  Atom@0..6
                    Char@0..6 "#\x3bb"
            "##]],
        );
        check(
            r"#\nul",
            expect![[r##"
                Root@0..5
                  Atom@0..5
                    Char@0..5 "#\nul"
            "##]],
        );
        check(
            r"#\alarm",
            expect![[r##"
                Root@0..7
                  Atom@0..7
                    Char@0..7 "#\alarm"
            "##]],
        );
        check(
            r"#\backspace",
            expect![[r##"
                Root@0..11
                  Atom@0..11
                    Char@0..11 "#\backspace"
            "##]],
        );
        check(
            r"#\tab",
            expect![[r##"
                Root@0..5
                  Atom@0..5
                    Char@0..5 "#\tab"
            "##]],
        );
        check(
            r"#\linefeed",
            expect![[r##"
                Root@0..10
                  Atom@0..10
                    Char@0..10 "#\linefeed"
            "##]],
        );
        check(
            r"#\newline",
            expect![[r##"
                Root@0..9
                  Atom@0..9
                    Char@0..9 "#\newline"
            "##]],
        );
        check(
            r"#\vtab",
            expect![[r##"
                Root@0..6
                  Atom@0..6
                    Char@0..6 "#\vtab"
            "##]],
        );
        check(
            r"#\page",
            expect![[r##"
                Root@0..6
                  Atom@0..6
                    Char@0..6 "#\page"
            "##]],
        );
        check(
            r"#\return",
            expect![[r##"
                Root@0..8
                  Atom@0..8
                    Char@0..8 "#\return"
            "##]],
        );
        check(
            r"#\esc",
            expect![[r##"
                Root@0..5
                  Atom@0..5
                    Char@0..5 "#\esc"
            "##]],
        );
        check(
            r"#\space",
            expect![[r##"
                Root@0..7
                  Atom@0..7
                    Char@0..7 "#\space"
            "##]],
        );
        check(
            r"#\delete",
            expect![[r##"
                Root@0..8
                  Atom@0..8
                    Char@0..8 "#\delete"
            "##]],
        );
    }

    #[test]
    fn number() {
        check(
            r"1",
            expect![[r#"
                Root@0..1
                  Atom@0..1
                    Number@0..1 "1"
            "#]],
        );
        check(
            r"#b0",
            expect![[r##"
                Root@0..3
                  Atom@0..3
                    Number@0..3 "#b0"
            "##]],
        );
        check(
            r"#B1",
            expect![[r##"
                Root@0..3
                  Atom@0..3
                    Number@0..3 "#B1"
            "##]],
        );
        check(
            r"#o0",
            expect![[r##"
                Root@0..3
                  Atom@0..3
                    Number@0..3 "#o0"
            "##]],
        );
        check(
            r"#O7",
            expect![[r##"
                Root@0..3
                  Atom@0..3
                    Number@0..3 "#O7"
            "##]],
        );
        check(
            r"#d0",
            expect![[r##"
                Root@0..3
                  Atom@0..3
                    Number@0..3 "#d0"
            "##]],
        );
        check(
            r"#D9",
            expect![[r##"
                Root@0..3
                  Atom@0..3
                    Number@0..3 "#D9"
            "##]],
        );
        check(
            r"#x0",
            expect![[r##"
                Root@0..3
                  Atom@0..3
                    Number@0..3 "#x0"
            "##]],
        );
        check(
            r"#XF",
            expect![[r##"
                Root@0..3
                  Atom@0..3
                    Number@0..3 "#XF"
            "##]],
        );
    }

    #[test]
    fn string() {
        check(
            r#""hello""#,
            expect![[r#"
                Root@0..7
                  Atom@0..7
                    String@0..7 ""hello""
            "#]],
        );
        check(
            r#""\a \b \t \n \v \f \r \" \\ \x3bb;""#,
            expect![[r#"
                Root@0..35
                  Atom@0..35
                    String@0..35 ""\a \b \t \n \v \f \r \" \\ \x3bb;""
            "#]],
        );
        check(
            "\"\\ \n \"",
            expect![[r#"
                Root@0..6
                  Atom@0..6
                    String@0..6 ""\ 
                 ""
            "#]],
        );
    }

    #[test]
    fn identifier() {
        check(
            "hello-world",
            expect![[r#"
                Root@0..11
                  Atom@0..11
                    Identifier@0..11 "hello-world"
            "#]],
        );
        check(
            "+",
            expect![[r#"
                Root@0..1
                  Atom@0..1
                    Identifier@0..1 "+"
            "#]],
        );
        check(
            "-",
            expect![[r#"
                Root@0..1
                  Atom@0..1
                    Identifier@0..1 "-"
            "#]],
        );
        check(
            "...",
            expect![[r#"
                Root@0..3
                  Atom@0..3
                    Identifier@0..3 "..."
            "#]],
        );
        check(
            "->",
            expect![[r#"
                Root@0..2
                  Atom@0..2
                    Identifier@0..2 "->"
            "#]],
        );
        check(
            r"\x3bb;",
            expect![[r#"
                Root@0..6
                  Atom@0..6
                    Identifier@0..6 "\x3bb;"
            "#]],
        );
    }

    #[test]
    fn abbreviation() {
        check(
            "'a",
            expect![[r#"
                Root@0..2
                  Abbreviation@0..2
                    Quote@0..1 "'"
                    Atom@1..1
                      Identifier@1..2 "a"
            "#]],
        );
        check(
            "`#t",
            expect![[r##"
                Root@0..3
                  Abbreviation@0..3
                    Backtick@0..1 "`"
                    Atom@1..2
                      True@1..3 "#t"
            "##]],
        );
        check(
            r",#\a",
            expect![[r##"
                Root@0..4
                  Abbreviation@0..4
                    Comma@0..1 ","
                    Atom@1..3
                      Char@1..4 "#\a"
            "##]],
        );
        check(
            ",@1",
            expect![[r#"
                Root@0..3
                  Abbreviation@0..3
                    CommaAt@0..2 ",@"
                    Atom@2..1
                      Number@2..3 "1"
            "#]],
        );
        check(
            r#"#'"Hi""#,
            expect![[r##"
                Root@0..6
                  Abbreviation@0..6
                    HashQuote@0..2 "#'"
                    Atom@2..4
                      String@2..6 ""Hi""
            "##]],
        );
        check(
            "#`()",
            expect![[r##"
                Root@0..4
                  Abbreviation@0..4
                    HashBacktick@0..2 "#`"
                    List@2..2
                      OpenDelim@2..3 "("
                      CloseDelim@3..4 ")"
            "##]],
        );
        check(
            "#,[]",
            expect![[r##"
                Root@0..4
                  Abbreviation@0..4
                    HashComma@0..2 "#,"
                    List@2..2
                      OpenDelim@2..3 "["
                      CloseDelim@3..4 "]"
            "##]],
        );
        check(
            "#,@(a)",
            expect![[r##"
                Root@0..6
                  Abbreviation@0..6
                    HashCommaAt@0..3 "#,@"
                    List@3..3
                      OpenDelim@3..4 "("
                      Atom@4..1
                        Identifier@4..5 "a"
                      CloseDelim@5..6 ")"
            "##]],
        );
    }

    #[test]
    fn list() {
        check(
            "()",
            expect![[r#"
                Root@0..2
                  List@0..2
                    OpenDelim@0..1 "("
                    CloseDelim@1..2 ")"
            "#]],
        );
        check(
            "[]",
            expect![[r#"
                Root@0..2
                  List@0..2
                    OpenDelim@0..1 "["
                    CloseDelim@1..2 "]"
            "#]],
        );
        check(
            "(#t)",
            expect![[r##"
                Root@0..4
                  List@0..4
                    OpenDelim@0..1 "("
                    Atom@1..2
                      True@1..3 "#t"
                    CloseDelim@3..4 ")"
            "##]],
        );
        check(
            "(#t . #f)",
            expect![[r##"
                Root@0..9
                  List@0..9
                    OpenDelim@0..1 "("
                    Atom@1..2
                      True@1..3 "#t"
                    Whitespace@3..4 " "
                    Dot@4..5 "."
                    Whitespace@5..6 " "
                    Atom@6..2
                      False@6..8 "#f"
                    CloseDelim@8..9 ")"
            "##]],
        );
        check(
            "(() (#t (#f) [] [3 ]))",
            expect![[r##"
                Root@0..22
                  List@0..22
                    OpenDelim@0..1 "("
                    List@1..2
                      OpenDelim@1..2 "("
                      CloseDelim@2..3 ")"
                    Whitespace@3..4 " "
                    List@4..17
                      OpenDelim@4..5 "("
                      Atom@5..2
                        True@5..7 "#t"
                      Whitespace@7..8 " "
                      List@8..4
                        OpenDelim@8..9 "("
                        Atom@9..2
                          False@9..11 "#f"
                        CloseDelim@11..12 ")"
                      Whitespace@12..13 " "
                      List@13..2
                        OpenDelim@13..14 "["
                        CloseDelim@14..15 "]"
                      Whitespace@15..16 " "
                      List@16..4
                        OpenDelim@16..17 "["
                        Atom@17..1
                          Number@17..18 "3"
                        Whitespace@18..19 " "
                        CloseDelim@19..20 "]"
                      CloseDelim@20..21 ")"
                    CloseDelim@21..22 ")"
            "##]],
        );
    }

    #[test]
    fn vector() {
        check(
            "#()",
            expect![[r##"
                Root@0..3
                  Vector@0..3
                    SpecialOpenDelim@0..2 "#("
                    CloseDelim@2..3 ")"
            "##]],
        );

        check(
            "#vu8()",
            expect![[r##"
                Root@0..6
                  Bytevector@0..6
                    SpecialOpenDelim@0..5 "#vu8("
                    CloseDelim@5..6 ")"
            "##]],
        );

        check(
            "#(1 #() #(#t))",
            expect![[r##"
                Root@0..14
                  Vector@0..14
                    SpecialOpenDelim@0..2 "#("
                    Atom@2..1
                      Number@2..3 "1"
                    Whitespace@3..4 " "
                    Vector@4..3
                      SpecialOpenDelim@4..6 "#("
                      CloseDelim@6..7 ")"
                    Whitespace@7..8 " "
                    Vector@8..5
                      SpecialOpenDelim@8..10 "#("
                      Atom@10..2
                        True@10..12 "#t"
                      CloseDelim@12..13 ")"
                    CloseDelim@13..14 ")"
            "##]],
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
                    .span(Span::new(0, 0, 1))
                    .finish()],
                expect![[r#"
                    Root@0..1
                      CloseDelim@0..1 ")"
                "#]],
            );

            check_error(
                "(",
                vec![Diagnostic::builder()
                    .msg("expected ), found <eof>")
                    .span(Span::new(0, 0, 1))
                    .sources(vec![Diagnostic::builder()
                        .msg("unclosed delimiter")
                        .span(Span::new(0, 0, 1))
                        .hint()
                        .finish()])
                    .finish()],
                expect![[r#"
                    Root@0..1
                      List@0..1
                        OpenDelim@0..1 "("
                "#]],
            );
        }

        #[test]
        fn braces() {
            check_error(
                "{}",
                vec![Diagnostic::builder()
                    .error()
                    .msg("braces not supported")
                    .span(Span::new(0, 0, 1))
                    .sources(vec![Diagnostic::builder()
                        .hint()
                        .msg("enable braces with --extended-syntax / --allow-braces")
                        .finish()])
                    .finish()],
                expect![[r#"
                    Root@0..2
                      List@0..2
                        OpenDelim@0..1 "{"
                        CloseDelim@1..2 "}"
                "#]],
            );
        }

        #[test]
        fn characters() {
            check_error(
                r"#\",
                vec![Diagnostic::builder()
                    .error()
                    .msg("expected a character, found <eof>")
                    .span(Span::new(0, 0, 2))
                    .finish()],
                expect![[r##"
                    Root@0..2
                      Atom@0..2
                        Char@0..2 "#\"
                "##]],
            );

            check_error(
                r"#\Space",
                vec![Diagnostic::builder()
                    .error()
                    .msg("invalid character name")
                    .span(Span::new(0, 0, 7))
                    .finish()],
                expect![[r##"
                    Root@0..7
                      Atom@0..7
                        Char@0..7 "#\Space"
                "##]],
            );

            check_error(
                r"#\INVALID-CHAR-NAME",
                vec![Diagnostic::builder()
                    .error()
                    .msg("invalid character name")
                    .span(Span::new(0, 0, 19))
                    .finish()],
                expect![[r##"
                    Root@0..19
                      Atom@0..19
                        Char@0..19 "#\INVALID-CHAR-NAME"
                "##]],
            );

            check_error(
                r"#\xd800",
                vec![Diagnostic::builder()
                    .error()
                    .msg("invalid unicode hex escape sequence")
                    .span(Span::new(0, 0, 7))
                    .finish()],
                expect![[r##"
                    Root@0..7
                      Atom@0..7
                        Char@0..7 "#\xd800"
                "##]],
            );

            check_error(
                r"#\xdfff",
                vec![Diagnostic::builder()
                    .error()
                    .msg("invalid unicode hex escape sequence")
                    .span(Span::new(0, 0, 7))
                    .finish()],
                expect![[r##"
                    Root@0..7
                      Atom@0..7
                        Char@0..7 "#\xdfff"
                "##]],
            );

            check_error(
                r"#\x110000",
                vec![Diagnostic::builder()
                    .error()
                    .msg("invalid unicode hex escape sequence")
                    .span(Span::new(0, 0, 9))
                    .finish()],
                expect![[r##"
                    Root@0..9
                      Atom@0..9
                        Char@0..9 "#\x110000"
                "##]],
            );

            check_error(
                r"#\xQWERTY",
                vec![Diagnostic::builder()
                    .error()
                    .msg("invalid hex scalar value")
                    .span(Span::new(0, 0, 9))
                    .finish()],
                expect![[r##"
                    Root@0..9
                      Atom@0..9
                        Char@0..9 "#\xQWERTY"
                "##]],
            );
        }

        #[test]
        fn identifiers() {
            check_error(
                "+a",
                vec![Diagnostic::builder()
                    .error()
                    .msg("identifiers cannot begin with +")
                    .span(Span::new(0, 0, 2))
                    .finish()],
                expect![[r##"
                    Root@0..2
                      Atom@0..2
                        Identifier@0..2 "+a"
                "##]],
            );

            check_error(
                "-a",
                vec![Diagnostic::builder()
                    .error()
                    .msg("identifiers cannot begin with -")
                    .span(Span::new(0, 0, 2))
                    .finish()],
                expect![[r##"
                    Root@0..2
                      Atom@0..2
                        Identifier@0..2 "-a"
                "##]],
            );

            check_error(
                "..",
                vec![Diagnostic::builder()
                    .error()
                    .msg("identifiers cannot begin with .")
                    .span(Span::new(0, 0, 2))
                    .finish()],
                expect![[r##"
                    Root@0..2
                      Atom@0..2
                        Identifier@0..2 ".."
                "##]],
            );

            check_error(
                r"h\xq65;llo",
                vec![Diagnostic::builder()
                    .error()
                    .msg("invalid hex scalar value")
                    .span(Span::new(0, 1, 6))
                    .finish()],
                expect![[r##"
                    Root@0..10
                      Atom@0..10
                        Identifier@0..10 "h\xq65;llo"
                "##]],
            );

            check_error(
                r"h\xp;l\xq;lo",
                vec![
                    Diagnostic::builder()
                        .error()
                        .msg("invalid hex scalar value")
                        .span(Span::new(0, 1, 4))
                        .finish(),
                    Diagnostic::builder()
                        .error()
                        .msg("invalid hex scalar value")
                        .span(Span::new(0, 6, 4))
                        .finish(),
                ],
                expect![[r##"
                    Root@0..12
                      Atom@0..12
                        Identifier@0..12 "h\xp;l\xq;lo"
                "##]],
            );

            check_error(
                r"\x3bb",
                vec![Diagnostic::builder()
                    .error()
                    .msg("unterminated hex escape sequence")
                    .span(Span::new(0, 0, 5))
                    .finish()],
                expect![[r##"
                    Root@0..5
                      Atom@0..5
                        Identifier@0..5 "\x3bb"
                "##]],
            );
        }

        #[test]
        #[ignore]
        fn strings() {
            check_error(
                r#""\q""#,
                vec![Diagnostic::builder()
                    .error()
                    .msg("invalid escape character")
                    .span(Span::new(0, 1, 2))
                    .finish()],
                expect![[r##"
                    Root@0..4
                      Atom@0..4
                        Identifier@0..4 ""\q""
                "##]],
            );
        }
    }

    mod extended_syntax {
        use super::*;

        fn check(input: &str, expected: Expect) {
            let result = parse_str_with_config(input, 0, ParserConfig::extended());
            assert_eq!(Vec::<Diagnostic>::default(), result.diagnostics);
            expected.assert_debug_eq(&result.tree);
        }

        #[test]
        fn braces() {
            check(
                "{}",
                expect![[r#"
                    Root@0..2
                      List@0..2
                        OpenDelim@0..1 "{"
                        CloseDelim@1..2 "}"
                "#]],
            );
        }
    }
}
