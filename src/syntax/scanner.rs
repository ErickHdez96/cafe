use std::{fmt, iter, str::Chars};

use super::{TokenKind, TokenKind as SK};

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct Token<'input> {
    pub kind: TokenKind,
    pub source: &'input str,
}

impl<'input> Token<'input> {
    pub fn new(kind: TokenKind, source: &'input str) -> Self {
        Self { kind, source }
    }

    pub fn is_trivia(self) -> bool {
        self.kind.is_trivia()
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            TokenKind::Eof => "<eof>".fmt(f),
            TokenKind::Whitespace => "<whitespace>".fmt(f),
            _ => self.source.fmt(f),
        }
    }
}

/// Lexes a string into a vector of tokens. The scanner is based off of the r6rs specification,
/// however it is very forgiving, to prevent returning errors. The parser must verify the tokens to
/// emit the corresponding errors for invalid tokens.
pub fn tokenize_str(input: &str) -> Vec<Token> {
    let mut c = Cursor {
        input: input.chars(),
        offset: 0,
    };

    let mut tokens = iter::from_fn(|| {
        let start = c.offset;

        // multi token
        macro_rules! mt {
            // Single token, eat the token and return the syntax kind `$st`
            ($c:expr, $st:expr) => {{
                $c.next();
                $st
            }};
            // Peeks the next char and test against `$tc`, if any of them match, evaluate `$tt`,
            // otherwise returns `$st`
            ($c:expr, $st:expr; $(($tc:expr, $tt:expr));+) => {
                match $c.peek() {
                    $($tc => { $c.next(); $tt })+
                    _ => $st,
                }
            }
        }

        let kind = match c.next() {
            '\0' if c.at_eof() => return None,

            '(' | '[' | '{' => SK::OpenDelim,
            ')' | ']' | '}' => SK::CloseDelim,
            '\'' => SK::Quote,
            '`' => SK::Backtick,
            ';' => {
                eat_with_end_of_line(&mut c);
                SK::SimpleComment
            }
            ',' => mt!(c, SK::Comma; ('@', SK::CommaAt)),
            '.' => {
                if is_delimiter(c.peek()) {
                    SK::Dot
                } else {
                    // Parser validation required
                    eat_till_delimiter(&mut c);
                    SK::Identifier
                }
            }
            '#' => match c.peek() {
                '(' | '[' | '{' => mt!(c, SK::SpecialOpenDelim),
                '\'' => mt!(c, SK::HashQuote),
                '`' => mt!(c, SK::HashBacktick),
                ',' => {
                    c.next();
                    mt!(c, SK::HashComma; ('@', SK::HashCommaAt))
                }
                // TODO: Implement datum comment
                ';' => mt!(c, SK::HashSemicolon),
                't' | 'T' if is_delimiter(c.peek_nth(1)) => mt!(c, SK::True),
                'f' | 'F' if is_delimiter(c.peek_nth(1)) => mt!(c, SK::False),
                '\\' => {
                    // \
                    c.next();
                    // eat the character
                    c.next();
                    // Parser validation required
                    eat_till_delimiter(&mut c);
                    SK::Char
                }
                '|' => {
                    eat_multi_comment(&mut c);
                    SK::MultiComment
                }
                '!' => {
                    c.next();
                    // script's shebang (e.g. '#!/usr/bin/env cafe')
                    if start == 0 {
                        eat_with_end_of_line(&mut c);
                    } else {
                        // simple shebang (e.g. #!r6rs)
                        eat_till_delimiter(&mut c);
                    }
                    SK::Shebang
                }
                // <prefix R> → <radix R> <exactness>
                //            | <exactness> <radix R>
                'b' | 'B' | 'o' | 'O' | 'd' | 'D' | 'x' | 'X' | 'i' | 'I' | 'e' | 'E' => {
                    // Parser validation required
                    eat_till_delimiter(&mut c);
                    SK::Number
                }
                _ => {
                    if is_delimiter(c.peek()) {
                        SK::Error
                    } else {
                        // handle #vu8(
                        // Parser validation required
                        eat_till_delimiter(&mut c);
                        if is_open_delimiter(c.peek()) {
                            c.next();
                            SK::SpecialOpenDelim
                        } else {
                            SK::Error
                        }
                    }
                }
            },
            '+' | '-' if c.peek().is_ascii_digit() => {
                // Parser validation required
                eat_till_delimiter(&mut c);
                SK::Number
            }
            '+' | '-' => {
                // Parser validation required
                eat_till_delimiter(&mut c);
                SK::Identifier
            }
            '"' => {
                // Parser validation required
                eat_string(&mut c);
                SK::String
            }
            i if i.is_ascii_digit() => {
                // Parser validation required
                eat_till_delimiter(&mut c);
                SK::Number
            }

            i if is_initial(i) => {
                eat_till_delimiter(&mut c);
                SK::Identifier
            }
            // Parser validation required
            '\\' if c.peek() == 'x' || c.peek() == 'X' => {
                eat_till_delimiter(&mut c);
                // eat_till_delimiter starts eating at 'x' and will stop at ;
                // restart to eat the rest of the identifier correctly.
                if c.peek() == ';' {
                    c.next();
                    eat_till_delimiter(&mut c);
                }
                SK::Identifier
            }
            t if is_whitespace(t) => {
                eat_till(&mut c, |c| !is_whitespace(c));
                SK::Whitespace
            }
            _ => SK::Error,
        };

        Some(Token::new(kind, &input[start..c.offset]))
    })
    .collect::<Vec<_>>();

    tokens.push(Token::new(SK::Eof, ""));
    tokens
}

struct Cursor<'a> {
    input: Chars<'a>,
    offset: usize,
}

impl<'a> Cursor<'a> {
    /// Returns `true` if the cursor is at the end of the input.
    fn at_eof(&self) -> bool {
        self.input.clone().next().is_none()
    }

    /// Advances and returns the next character in the input. `'\0'` is returned when cursor
    /// reaches 'eof'. Use [`Self::at_eof`] to test if it actually is the end of the input.
    fn next(&mut self) -> char {
        match self.input.next() {
            Some(c) => {
                self.offset += c.len_utf8();
                c
            }
            None => '\0',
        }
    }

    /// Returns the nth next character without advancing the cursor.
    fn peek_nth(&self, n: usize) -> char {
        self.input.clone().nth(n).unwrap_or_default()
    }

    /// Returns the next character without advancing the cursor.
    fn peek(&self) -> char {
        self.peek_nth(0)
    }
}

const fn is_whitespace(c: char) -> bool {
    // TODO: Unicode Zs, Zl, Zp
    matches!(
        c,
        ' ' | '\t' | '\r' | '\n' | '\u{000B}' | '\u{000C}' | '\u{0085}'
    )
}

const fn is_open_delimiter(c: char) -> bool {
    matches!(c, '(' | '[' | '{')
}

/// <delimiter> → ( | ) | [ | ] | { | } | " | ; | # | <whitespace>
const fn is_delimiter(c: char) -> bool {
    matches!(
        c,
        '(' | ')' | '[' | ']' | '{' | '}' | '"' | ';' | '#' | '\0'
    ) || is_whitespace(c)
}

/// ```text
/// <initial> → <constituent> | <special initial>
/// ```
///  initial hex-escape sequence should be handled differently.
const fn is_initial(c: char) -> bool {
    is_constituent(c) || is_special_init(c)
}

/// <constituent> → <letter> | <any character whose Unicode scalar value is greater than 127, and
/// whose category is Lu, Ll, Lt, Lm, Lo, Mn, Nl, No, Pd, Pc, Po, Sc, Sm, Sk, So, or Co
const fn is_constituent(c: char) -> bool {
    // TODO: Handle Unicode
    c.is_ascii_alphabetic() || c == 'λ'
}

/// <special init> → ! | $ | % | & | * | / | : | < | = | > | ? | ^ | _ | ~
const fn is_special_init(c: char) -> bool {
    matches!(
        c,
        '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~'
    )
}

/// Consume all characters until a delimiter is found.
fn eat_till(c: &mut Cursor, pred: impl Fn(char) -> bool) {
    while !pred(c.peek()) && !c.at_eof() {
        c.next();
    }
}

fn eat_with_end_of_line(c: &mut Cursor) {
    eat_till(c, |c| {
        matches!(c, '\n' | '\r' | '\u{85}' | '\u{2028}' | '\u{2029}')
    });
    if c.peek() == '\r' && c.peek_nth(1) == '\n' {
        c.next();
    }
    c.next();
}

/// Consume all characters until a delimiter is found.
fn eat_till_delimiter(c: &mut Cursor) {
    let mut consume_semicolon = false;
    loop {
        if is_delimiter(c.peek()) {
            if c.peek() == ';' && consume_semicolon {
                consume_semicolon = false;
            } else {
                break;
            }
        }
        if c.peek() == '\\' && (c.peek_nth(1) == 'x' || c.peek_nth(1) == 'X') {
            c.next();
            consume_semicolon = true;
        }
        c.next();
    }
}

fn eat_string(c: &mut Cursor) {
    loop {
        match c.peek() {
            '"' => {
                c.next();
                break;
            }
            '\\' => {
                c.next();
                c.next();
            }
            '\0' if c.at_eof() => {
                break;
            }
            _ => {
                c.next();
            }
        }
    }
}

fn eat_multi_comment(c: &mut Cursor) {
    assert_eq!(c.next(), '|');
    let mut depth = 1;
    loop {
        match c.peek() {
            '|' if c.peek_nth(1) == '#' => {
                c.next();
                c.next();
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            '#' if c.peek_nth(1) == '|' => {
                c.next();
                c.next();
                depth += 1;
            }
            '\0' if c.at_eof() => {
                break;
            }
            _ => {
                c.next();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(input: &str, expected: SK) {
        let tokens = tokenize_str(input);
        assert_eq!(
            tokens,
            vec![Token::new(expected, input), Token::new(SK::Eof, "")]
        );
    }

    #[test]
    fn delimiter() {
        check("(", SK::OpenDelim);
        check(")", SK::CloseDelim);
        check("[", SK::OpenDelim);
        check("]", SK::CloseDelim);
        check("#(", SK::SpecialOpenDelim);
        check("#vu8(", SK::SpecialOpenDelim);
    }

    #[test]
    fn special_symbol() {
        check("'", SK::Quote);
        check("`", SK::Backtick);
        check(",", SK::Comma);
        check(",@", SK::CommaAt);
        check(".", SK::Dot);
        check("...", SK::Identifier);
        check("#'", SK::HashQuote);
        check("#`", SK::HashBacktick);
        check("#,", SK::HashComma);
        check("#,@", SK::HashCommaAt);
    }

    #[test]
    fn boolean() {
        check("#t", SK::True);
        check("#T", SK::True);
        check("#f", SK::False);
        check("#F", SK::False);
    }

    #[test]
    fn char() {
        fn check_char(input: &str) {
            check(input, SK::Char);
        }
        check_char(r"#\a");
        check_char(r"#\λ");
        check_char(r"#\ ");
        check_char(r"#\\");
        check_char(r"#\x3bb");
        check_char(r"#\nul");
        check_char(r"#\alarm");
        check_char(r"#\backspace");
        check_char(r"#\tab");
        check_char(r"#\linefeed");
        check_char(r"#\newline");
        check_char(r"#\vtab");
        check_char(r"#\page");
        check_char(r"#\return");
        check_char(r"#\esc");
        check_char(r"#\space");
        check_char(r"#\delete");
    }

    #[test]
    fn number() {
        fn check_number(input: &str) {
            check(input, SK::Number);
        }
        check_number("1");
        check_number("+1");
        check_number("-1");
        check_number("#b0");
        check_number("#B0");
        check_number("#o0");
        check_number("#O0");
        check_number("#d0");
        check_number("#D0");
        check_number("#xA");
        check_number("#XA");
    }

    #[test]
    fn comment() {
        check(";", SK::SimpleComment);
        check("; hello", SK::SimpleComment);
        check("#;", SK::HashSemicolon);
        check("#| #| hello-world |# |#", SK::MultiComment);
    }

    #[test]
    fn identifier() {
        fn check_ident(input: &str) {
            check(input, SK::Identifier);
        }
        check_ident("+");
        check_ident("list->vector");
        check_ident("!$%&*/:<=>?^_~");
        check_ident("->");
        check_ident("set!");
        check_ident("!");
        check_ident("$");
        check_ident("%");
        check_ident("&");
        check_ident("*");
        check_ident("/");
        check_ident(":");
        check_ident("<");
        check_ident("=");
        check_ident(">");
        check_ident("?");
        check_ident("^");
        check_ident("_");
        check_ident("!");
        check_ident("try2");
        check_ident("+");
        check_ident("-");
        check_ident("...");
        check_ident("->");
        check_ident("->hi");
        check_ident("hello-world");
        check_ident(r"h\x65;llo-world");
        check_ident(r"\x3bb;");
        // hello
        check_ident(r"\x68;\x65;\x6c;\x6c;\x6f;");
    }

    #[test]
    fn string() {
        fn check_string(input: &str) {
            check(&format!("\"{input}\""), SK::String);
        }
        check_string("Hello, world!");
        check_string("hi");
        check_string(r#"\"hi\""#);
        check_string(r"\r\n");
    }

    #[test]
    fn shebang() {
        fn check_shebang(input: &str) {
            check(input, SK::Shebang);
        }
        check_shebang("#!/usr/bin/env cafe");
        check_shebang("#!r6rs");
        check_shebang("#!cafe");
    }

    #[test]
    fn whitespace() {
        fn check_whitespace(input: &str) {
            check(input, SK::Whitespace);
        }
        check_whitespace(" ");
        check_whitespace("\n");
        check_whitespace(" \n ");
        check_whitespace("\t");
    }

    #[test]
    fn extension() {
        check("{", SK::OpenDelim);
        check("}", SK::CloseDelim);

        check("#[", SK::SpecialOpenDelim);
        check("#{", SK::SpecialOpenDelim);
        check("#vu8[", SK::SpecialOpenDelim);
        check("#my-vector[", SK::SpecialOpenDelim);
    }

    #[test]
    fn error_recovery() {
        check("#VU8(", SK::SpecialOpenDelim);

        check("..", SK::Identifier);
        check("....", SK::Identifier);
        check("+a", SK::Identifier);
        check("-a", SK::Identifier);
        check(r"h\xq65;llo", SK::Identifier);

        check(r"#\", SK::Char);
        check(r"#\SPACE", SK::Char);
        check(r"#\unkown-char", SK::Char);
        check(r"#\xffffffff", SK::Char);

        check("#bA", SK::Number);
        check("#oA", SK::Number);
        check("#dA", SK::Number);
        check("#xQ", SK::Number);
        check("3fafeqr$", SK::Number);

        check(r#""unterminated string"#, SK::String);
        check(r#""wrongly escaped\ string"#, SK::String);
    }

    #[test]
    fn errors() {
        fn check_error(input: &str) {
            check(input, SK::Error);
        }
        check_error("#");
        check_error("#true");
        check_error("#false");
        check_error("#t3");
        check_error("|");
        check_error("@");
        check_error("±");
    }
}
