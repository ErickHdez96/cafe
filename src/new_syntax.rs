pub mod ast;
pub mod cst;
pub mod parser;
pub mod scanner;

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SyntaxKind {
    // Tokens
    /// ( [
    OpenDelim,
    /// ) ]
    CloseDelim,

    /// #( #vu8(
    SpecialOpenDelim,

    /// '
    Quote,
    /// `
    Backtick,
    /// ,
    Comma,
    /// ,@
    CommaAt,
    /// .
    Dot,
    /// #'
    HashQuote,
    /// #`
    HashBacktick,
    /// #,
    HashComma,
    /// #,@
    HashCommaAt,

    /// #t #T
    True,
    /// #f #F
    False,

    /// #!something
    Shebang,

    Number,
    Char,
    Identifier,
    String,

    /// ; <till end of line>
    SimpleComment,
    /// #| |#
    MultiComment,
    /// #;
    HashSemicolon,

    Whitespace,
    #[default]
    Error,
    Eof,
}

impl SyntaxKind {
    pub fn is_eof(self) -> bool {
        self == SyntaxKind::Eof
    }

    /// Returns `true` if self is a `SyntaxKind::OpenDelim` or `SyntaxKind::SpecialOpenDelim`.
    pub fn is_open_delim(self) -> bool {
        matches!(self, SyntaxKind::OpenDelim | SyntaxKind::SpecialOpenDelim)
    }

    pub fn is_trivia(self) -> bool {
        matches!(
            self,
            SyntaxKind::Whitespace
                | SyntaxKind::SimpleComment
                | SyntaxKind::MultiComment
                | SyntaxKind::Shebang
        )
    }

    pub fn is_abbrev(self) -> bool {
        matches!(
            self,
            SyntaxKind::Quote
                | SyntaxKind::Backtick
                | SyntaxKind::Comma
                | SyntaxKind::CommaAt
                | SyntaxKind::HashQuote
                | SyntaxKind::HashBacktick
                | SyntaxKind::HashComma
                | SyntaxKind::HashCommaAt
        )
    }
}
