#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Language {
    #[default]
    Cafe,
    R6rs,
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ParserConfig {
    pub braces: bool,
}

impl ParserConfig {
    pub fn extended() -> Self {
        Self { braces: true }
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct CompilerConfig {
    pub parser: ParserConfig,
    pub language: Language,
}

impl CompilerConfig {
    pub fn extended() -> Self {
        Self {
            parser: ParserConfig::extended(),
            ..Default::default()
        }
    }
}
