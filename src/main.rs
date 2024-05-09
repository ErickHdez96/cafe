use std::{
    io::Write,
    path::{Path, PathBuf},
};
mod repl;

use cafe::{
    compiler::Compiler, config::CompilerConfig, diagnostics::emit_diagnostics, driver::compile_asm,
};

const HELP_MESSAGE: &str = "\
The Cafe Compiler
  USAGE: cafe [OPTIONS] INPUT

Options:
  -h, --help        Shows this message and exits.
";

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() {
    let (config, input) = parse_args();
    match input {
        Some(input) => {
            if let Err(e) = run_compiler(&input, config) {
                eprintln!("{e}");
                std::process::exit(1);
            }
        }
        None => repl::repl(),
    }
}

fn run_compiler(input: &Path, config: CompilerConfig) -> Result<()> {
    let mut compiler = Compiler::default();
    compiler.config = config;

    match compiler.compile_file(input) {
        Ok(insts) => {
            emit_diagnostics(&compiler, &compiler.take_diagnostics());
            let out_asm_path = {
                let path = input.with_extension("S");
                let mut out_asm = std::fs::File::create(&path)?;
                writeln!(out_asm, "{}", compiler.runtime())?;
                write!(out_asm, "{insts}")?;
                path
            };
            compile_asm(&out_asm_path, &input.with_extension(""))?;

            Ok(())
        }
        Err(d) => Result::Err(d.into()),
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
