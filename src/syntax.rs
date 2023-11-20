pub mod ast;
pub mod parser;
pub mod scanner;

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SyntaxKind {
    Root,
    // Ast nodes
    List,
    Vector,
    Bytevector,
    Atom,
    Abbreviation,

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

impl SyntaxKind {
    pub fn is_eof(self) -> bool {
        self == SyntaxKind::Eof
    }

    pub fn is_trivia(self) -> bool {
        matches!(
            self,
            SyntaxKind::Whitespace
                | SyntaxKind::SimpleComment
                | SyntaxKind::MultiComment
                | SyntaxKind::DatumComment
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

    pub fn expected_repr(self) -> String {
        match &self {
            SyntaxKind::List | SyntaxKind::OpenDelim => "an open delimiter".to_string(),
            SyntaxKind::Atom => "an atom".to_string(),
            SyntaxKind::CloseDelim => todo!(),
            SyntaxKind::SpecialOpenDelim => todo!(),
            SyntaxKind::Quote => todo!(),
            SyntaxKind::Backtick => todo!(),
            SyntaxKind::Comma => todo!(),
            SyntaxKind::CommaAt => todo!(),
            SyntaxKind::Dot => todo!(),
            SyntaxKind::HashQuote => todo!(),
            SyntaxKind::HashBacktick => todo!(),
            SyntaxKind::HashComma => todo!(),
            SyntaxKind::HashCommaAt => todo!(),
            SyntaxKind::True => todo!(),
            SyntaxKind::False => todo!(),
            SyntaxKind::Shebang => todo!(),
            SyntaxKind::Number => todo!(),
            SyntaxKind::Char => todo!(),
            SyntaxKind::Identifier => todo!(),
            SyntaxKind::String => todo!(),
            SyntaxKind::SimpleComment => todo!(),
            SyntaxKind::MultiComment => todo!(),
            SyntaxKind::DatumComment => todo!(),
            SyntaxKind::Whitespace => todo!(),
            SyntaxKind::Error => todo!(),
            SyntaxKind::Eof => todo!(),
            _ => unreachable!(),
        }
    }
}
