use std::path::PathBuf;

use cafe::{
    config::CompilerConfig,
    diagnostics::{emit_diagnostic, emit_diagnostics},
    expander::core_expander_interface,
    syntax::ast::ModuleName,
    BuildSystem,
};

const HELP_MESSAGE: &str = "\
The Cafe Compiler
  USAGE: cafe [OPTIONS] INPUT

Options:
  -h, --help        Shows this message and exits.
";

fn main() {
    let qctx = BuildSystem::default();
    let (config, input) = parse_args();
    qctx.feed_compiler_config(config);
    qctx.feed_module_name((ModuleName::script(), input));
    qctx.feed_intrinsic_lib(core_expander_interface());

    match qctx.expand(ModuleName::script()) {
        Ok(res) => {
            println!("{:#?}", res.module);
            emit_diagnostics(&qctx, &res.diagnostics);
        }
        Err(e) => {
            emit_diagnostic(&qctx, &e);
        }
    }
}

fn parse_args() -> (CompilerConfig, PathBuf) {
    let mut input = None;
    let mut config = CompilerConfig::default();

    for arg in std::env::args().skip(1) {
        if let Some(stripped) = arg.strip_prefix("--") {
            match stripped {
                "help" => bail(HELP_MESSAGE, 0),
                "extended-syntax" => {
                    config = CompilerConfig::extended();
                }
                "allow-braces" => {
                    config.parser.braces = true;
                }
                _ => bail(HELP_MESSAGE, 1),
            }
        } else if let Some(stripped) = arg.strip_prefix('-') {
            match stripped {
                "h" => bail(HELP_MESSAGE, 0),
                _ => bail(HELP_MESSAGE, 1),
            }
        } else if input.is_some() {
            bail(HELP_MESSAGE, 1);
        } else {
            input = Some(arg);
        }
    }

    (
        config,
        input.unwrap_or_else(|| bail(HELP_MESSAGE, 1)).into(),
    )
}

fn bail(msg: &str, exit_code: i32) -> ! {
    if exit_code == 0 {
        println!("{}", msg);
    } else {
        eprintln!("{}", msg);
    }
    std::process::exit(exit_code);
}
