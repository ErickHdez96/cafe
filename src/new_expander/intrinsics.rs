use crate::env::Env;

use super::{scopes::Scopes, Binding};

pub fn intrinsics_env() -> Env<'static, String, Binding> {
    let mut env = Env::default();
    env.insert(
        String::from("import"),
        Binding::Import {
            scopes: Scopes::core(),
        },
    );
    env.insert(
        String::from("module"),
        Binding::Module {
            scopes: Scopes::core(),
        },
    );
    //env.insert(
    //    String::from("library"),
    //    Binding::SyntaxTransformer {
    //        scopes: Scopes::core(),
    //        name: String::from("library"),
    //        transformer: core::library_transformer,
    //    },
    //);
    env
}
