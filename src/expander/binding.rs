use std::cell::Cell;

use crate::{symbol::Symbol, syntax::ast};

use super::{scopes::Scopes, CoreDefTransformer, CoreExprTransformer};

/// The possible values a name may be bound to at compile time.
///
/// Every Binding has a set of scopes in which it is visible (for macro expansion).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Binding {
    /// A normal runtime value. The value isn't stored here, but is generated at runtime.
    Value {
        scopes: Scopes,
        /// The [`Module`] where it comes from. Macros uses may introduce bindings from other
        /// modules without needing to import them.
        orig_module: ast::ModId,
        name: Symbol,
    },
    /// Built-in importer binding.
    Import { scopes: Scopes },
    /// Built-in module defintion binding.
    Module { scopes: Scopes },
    /// Built-in `define` binding.
    CoreDefTransformer {
        scopes: Scopes,
        name: Symbol,
        transformer: CoreDefTransformer,
    },
    /// Built-in macro-like expression bindings (e.g. if, lambda, quote, set!).
    CoreExprTransformer {
        scopes: Scopes,
        name: Symbol,
        transformer: CoreExprTransformer,
    },
}

impl Binding {
    /// Returns a new [`Binding::Value`] [`Binding`].
    pub fn new_var(name: &str, orig_module: ast::ModId, scopes: Scopes) -> Self {
        Self::Value {
            scopes,
            orig_module,
            name: name.into(),
        }
    }

    /// Returns the original name associated with the [`Binding`]. For debug and diagnostic
    /// purposes.
    pub fn name(&self) -> &str {
        match self {
            Binding::Import { .. } => "import",
            Binding::Module { .. } => "module",
            Binding::Value { name, .. } => name.resolve(),
            Binding::CoreDefTransformer { name, .. }
            | Binding::CoreExprTransformer { name, .. } => name.resolve(),
        }
    }

    pub fn scopes(&self) -> &Scopes {
        match self {
            Binding::Value { scopes, .. }
            | Binding::Import { scopes }
            | Binding::Module { scopes }
            | Binding::CoreDefTransformer { scopes, .. }
            | Binding::CoreExprTransformer { scopes, .. } => scopes,
        }
    }

    pub fn scopes_mut(&mut self) -> &mut Scopes {
        match self {
            Binding::Value { ref mut scopes, .. }
            | Binding::Import { ref mut scopes }
            | Binding::Module { ref mut scopes }
            | Binding::CoreDefTransformer { ref mut scopes, .. }
            | Binding::CoreExprTransformer { ref mut scopes, .. } => scopes,
        }
    }
}

thread_local! {
    static IDENTIFIER_ID: Cell<u64> = const { Cell::new(1) };
}
