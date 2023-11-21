use std::fmt::{self, Write};

use crate::{
    atty::{Color, Style},
    query::Query,
    span::Span,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

    pub fn to_string(&self, qctx: &Query, atty: bool) -> String {
        if self.is_simple() {
            format!(
                "{}{}{}: {}",
                if atty {
                    self.level.style().to_string()
                } else {
                    String::new()
                },
                self.level,
                if atty {
                    Style::new().to_string()
                } else {
                    String::new()
                },
                self.message
            )
        } else {
            let file = qctx.file(self.span.file_id()).expect("invalid file id");
            let pos = file.position(self.span);
            let text = file.get_text(self.span);
            let line_number_length = pos.end_row.to_string().len();
            let blue = if atty {
                Style::new().bold().color(Color::BrightBlue).to_string()
            } else {
                String::new()
            };
            let clear = if atty {
                Style::new().to_string()
            } else {
                String::new()
            };
            let mut out = String::new();

            let _ = writeln!(
                out,
                "{}{}{}: {}{}{}",
                if atty {
                    self.level.style().to_string()
                } else {
                    String::new()
                },
                self.level,
                if atty {
                    Style::new().to_string()
                } else {
                    String::new()
                },
                if atty {
                    Style::new().bold().to_string()
                } else {
                    String::new()
                },
                self.message,
                if atty {
                    Style::new().to_string()
                } else {
                    String::new()
                }
            );
            let _ = write!(out, "{}", " ".repeat(line_number_length));
            let _ = writeln!(
                out,
                "{}-->{} {}:{}:{}",
                blue,
                clear,
                file.path.to_string_lossy(),
                pos.start_row + 1,
                pos.start_col + 1,
            );

            if pos.start_row == pos.end_row {
                let _ = writeln!(out, "{} {}│{}", " ".repeat(line_number_length), blue, clear);
                let _ = write!(out, "{}{} │{}", blue, pos.start_row + 1, clear);
                let _ = writeln!(out, " {}", text);
                let _ = write!(
                    out,
                    "{} {}│{} ",
                    " ".repeat(line_number_length),
                    blue,
                    clear
                );
                let _ = write!(
                    out,
                    "{}{}{}{}",
                    " ".repeat(pos.start_col),
                    self.level.style(),
                    if self.level == DiagnosticLevel::Hint {
                        "-".repeat(pos.end_col - pos.start_col)
                    } else {
                        "^".repeat(pos.end_col - pos.start_col)
                    },
                    clear
                );
            }

            for d in &self.sources {
                let _ = write!(out, "\n{}", d.to_string(qctx, atty));
            }

            out
        }
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

pub fn emit_diagnostics(qctx: &Query, diagnostics: &[Diagnostic]) {
    for d in diagnostics {
        emit_diagnostic(qctx, d);
    }
}

pub fn emit_diagnostic(qctx: &Query, d: &Diagnostic) {
    eprintln!("{}", d.to_string(qctx, true));
}
