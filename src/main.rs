use std::path::PathBuf;
mod repl;

use cafe::{compiler::Compiler, config::CompilerConfig, diagnostics::emit_diagnostics};

const HELP_MESSAGE: &str = "\
The Cafe Compiler
  USAGE: cafe [OPTIONS] INPUT

Options:
  -h, --help        Shows this message and exits.
";

fn main() {
    let mut compiler = Compiler::default();
    let (config, input) = parse_args();
    let Some(input) = input else {
        repl::repl();
    };

    compiler.config = config;

    match compiler.compile_file(input) {
        Ok(insts) => {
            println!("{insts}");
            emit_diagnostics(&compiler, &compiler.take_diagnostics());
        }
        Err(_) => todo!(),
    }
}

fn parse_args() -> (CompilerConfig, Option<PathBuf>) {
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

    (config, input.map(|i| i.into()))
}

fn bail(msg: &str, exit_code: i32) -> ! {
    if exit_code == 0 {
        println!("{}", msg);
    } else {
        eprintln!("{}", msg);
    }
    std::process::exit(exit_code);
}
