pub mod ast;
pub mod scanner;

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SyntaxKind {
    Root,
    // Ast nodes
    List,

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
    DatumComment,

    Whitespace,
    #[default]
    Error,
    Eof,
}
