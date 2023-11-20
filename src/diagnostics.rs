use crate::span::Span;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Hint,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub span: Span,
    pub message: String,
    pub sources: Vec<Diagnostic>,
}

impl Diagnostic {
    pub fn builder() -> DiagnosticBuilder {
        DiagnosticBuilder::default()
    }

    pub fn is_simple(&self) -> bool {
        self.span.is_dummy() && self.sources.is_empty()
    }
}

pub struct DiagnosticBuilder {
    pub msg: Option<String>,
    pub level: DiagnosticLevel,
    pub span: Span,
    pub sources: Vec<Diagnostic>,
}

impl Default for DiagnosticBuilder {
    fn default() -> Self {
        Self {
            msg: None,
            level: DiagnosticLevel::Error,
            span: Span::dummy(),
            sources: vec![],
        }
    }
}

impl DiagnosticBuilder {
    pub fn msg(mut self, msg: impl Into<String>) -> Self {
        self.msg = Some(msg.into());
        self
    }

    pub fn error(mut self) -> Self {
        self.level = DiagnosticLevel::Error;
        self
    }

    pub fn warn(mut self) -> Self {
        self.level = DiagnosticLevel::Warning;
        self
    }

    pub fn hint(mut self) -> Self {
        self.level = DiagnosticLevel::Hint;
        self
    }

    pub fn level(mut self, level: DiagnosticLevel) -> Self {
        self.level = level;
        self
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn sources(mut self, sources: Vec<Diagnostic>) -> Self {
        self.sources = sources;
        self
    }

    pub fn finish(self) -> Diagnostic {
        Diagnostic {
            level: self.level,
            span: self.span,
            message: self.msg.expect("expected a message"),
            sources: self.sources,
        }
    }
}
