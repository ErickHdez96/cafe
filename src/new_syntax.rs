pub mod ast;
pub mod cst;
pub mod parser;
pub mod scanner;

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum TokenKind {
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

impl TokenKind {
    pub fn is_eof(self) -> bool {
        self == TokenKind::Eof
    }

    /// Returns `true` if self is a `SyntaxKind::OpenDelim` or `SyntaxKind::SpecialOpenDelim`.
    pub fn is_open_delim(self) -> bool {
        matches!(self, TokenKind::OpenDelim | TokenKind::SpecialOpenDelim)
    }

    pub fn is_trivia(self) -> bool {
        matches!(
            self,
            TokenKind::Whitespace
                | TokenKind::SimpleComment
                | TokenKind::MultiComment
                | TokenKind::Shebang
        )
    }

    pub fn is_abbrev(self) -> bool {
        matches!(
            self,
            TokenKind::Quote
                | TokenKind::Backtick
                | TokenKind::Comma
                | TokenKind::CommaAt
                | TokenKind::HashQuote
                | TokenKind::HashBacktick
                | TokenKind::HashComma
                | TokenKind::HashCommaAt
        )
    }
}
