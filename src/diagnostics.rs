use std::fmt::{self, Write};

use crate::{
    atty::{Color, Style},
    query::BuildSystem,
    span::Span,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Hint,
}

impl DiagnosticLevel {
    pub fn style(self) -> Style {
        match self {
            DiagnosticLevel::Error => Style::new().bold().color(Color::BrightRed),
            DiagnosticLevel::Warning => todo!(),
            DiagnosticLevel::Hint => Style::new().bold().color(Color::Cyan),
        }
    }
}

impl fmt::Display for DiagnosticLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagnosticLevel::Error => "error".fmt(f),
            DiagnosticLevel::Warning => "warning".fmt(f),
            DiagnosticLevel::Hint => "hint".fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub span: Span,
    pub message: String,
    /// Whether the diagnostic should be shown one character after the span. Useful when a list is
    /// unclosed, and it reaches the eof.
    pub show_after: bool,
    /// Diagnostics related to the current one (e.g. hints to fix it, previous variable with the
    /// same name, etc.)
    pub related: Vec<Diagnostic>,
}

impl Diagnostic {
    #[must_use]
    pub fn builder() -> DiagnosticBuilder {
        DiagnosticBuilder::default()
    }

    #[must_use]
    pub fn with_builder(f: impl FnOnce(DiagnosticBuilder) -> DiagnosticBuilder) -> Self {
        f(Self::builder()).finish()
    }

    pub fn is_simple(&self) -> bool {
        self.span.is_dummy() && self.related.is_empty()
    }

    fn simple_to_string(&self, atty: bool) -> String {
        let level = self.level.style().atty(atty).finish();
        format!("{}: {}", level(&self.level), self.message)
    }

    fn complex_to_string(&self, qctx: &BuildSystem, atty: bool) -> String {
        let level = self.level.style().atty(atty).finish();
        let file = qctx
            .file(self.span.file_id())
            .unwrap_or_else(|| panic!("invalid file id: {}", self.span.file_id().value()));
        let pos = file.position(self.span);
        let text = file.get_text(self.span);
        let line_number_length = (pos.end_row + 1).to_string().len();
        let blue = Style::new()
            .bold()
            .color(Color::BrightBlue)
            .atty(atty)
            .finish();
        let mut out = String::new();

        let _ = writeln!(
            out,
            "{}: {}",
            level(&self.level),
            Style::new().bold().atty(atty).finish()(&self.message),
        );
        let _ = write!(out, "{}", " ".repeat(line_number_length));
        let _ = writeln!(
            out,
            "{} {}:{}:{}",
            blue(&"-->"),
            file.path.to_string_lossy(),
            pos.start_row + 1,
            pos.start_col + 1,
        );

        if pos.start_row == pos.end_row {
            let _ = writeln!(out, "{} {}", " ".repeat(line_number_length), blue(&'|'));
            let _ = write!(out, "{}", blue(&format!("{} |", pos.start_row + 1)));
            let _ = writeln!(out, " {text}");
            let _ = write!(out, "{} {} ", " ".repeat(line_number_length), blue(&'|'),);
            if self.show_after {
                let _ = write!(out, "{}{}", " ".repeat(pos.end_col), level(&'^'));
            } else {
                let _ = write!(
                    out,
                    "{}{}",
                    " ".repeat(pos.start_col),
                    level(&if self.level == DiagnosticLevel::Hint {
                        "-".repeat(pos.end_col - pos.start_col)
                    } else {
                        "^".repeat(pos.end_col - pos.start_col)
                    }),
                );
            }
        }

        for d in &self.related {
            let _ = write!(out, "\n{}", d.to_string(qctx, atty));
        }

        out
    }

    pub fn to_string(&self, qctx: &BuildSystem, atty: bool) -> String {
        if self.is_simple() {
            self.simple_to_string(atty)
        } else {
            self.complex_to_string(qctx, atty)
        }
    }
}

pub struct DiagnosticBuilder {
    pub msg: Option<String>,
    pub level: DiagnosticLevel,
    pub span: Span,
    pub show_after: bool,
    pub related: Vec<Diagnostic>,
}

impl Default for DiagnosticBuilder {
    fn default() -> Self {
        Self {
            msg: None,
            level: DiagnosticLevel::Error,
            span: Span::dummy(),
            show_after: false,
            related: vec![],
        }
    }
}

impl DiagnosticBuilder {
    #[must_use]
    pub fn msg(mut self, msg: impl Into<String>) -> Self {
        self.msg = Some(msg.into());
        self
    }

    #[must_use]
    pub fn error(mut self) -> Self {
        self.level = DiagnosticLevel::Error;
        self
    }

    #[must_use]
    pub fn warn(mut self) -> Self {
        self.level = DiagnosticLevel::Warning;
        self
    }

    #[must_use]
    pub fn hint(mut self) -> Self {
        self.level = DiagnosticLevel::Hint;
        self
    }

    #[must_use]
    pub fn level(mut self, level: DiagnosticLevel) -> Self {
        self.level = level;
        self
    }

    #[must_use]
    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    #[must_use]
    pub fn related(mut self, related: Vec<Diagnostic>) -> Self {
        self.related = related;
        self
    }

    #[must_use]
    pub fn show_after(mut self) -> Self {
        self.show_after = true;
        self
    }

    #[must_use]
    pub fn show_after_select(mut self, v: bool) -> Self {
        self.show_after = v;
        self
    }

    pub fn finish(self) -> Diagnostic {
        Diagnostic {
            level: self.level,
            span: self.span,
            show_after: self.show_after,
            message: self.msg.expect("expected a message"),
            related: self.related,
        }
    }
}

pub fn emit_diagnostics(qctx: &BuildSystem, diagnostics: &[Diagnostic]) {
    for d in diagnostics {
        emit_diagnostic(qctx, d);
    }
}

pub fn emit_diagnostic(qctx: &BuildSystem, d: &Diagnostic) {
    eprintln!("{}", d.to_string(qctx, true));
}
