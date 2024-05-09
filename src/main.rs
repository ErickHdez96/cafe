use std::{
    ffi::OsStr,
    io::Write,
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
    process::Command,
};
mod repl;

use cafe::{compiler::Compiler, config::CompilerConfig, diagnostics::emit_diagnostics};

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
            let out_asm_path = input.with_extension("S");
            let out_object_path = input.with_extension("o");
            let out_binary_path = input.with_extension("");

            let mut out_asm = std::fs::File::create(&out_asm_path)?;
            writeln!(out_asm, "{}", compiler.runtime())?;
            write!(out_asm, "{insts}")?;
            std::mem::drop(out_asm);

            let out_asm_str = out_asm_path.to_str().unwrap();
            let out_binary_str = out_binary_path.to_str().unwrap();
            let out_object_str = out_object_path.to_str().unwrap();

            let xcode = Command::new("xcodebuild")
                .args(["-find", "clang"])
                .output()
                .unwrap()
                .stdout;
            let xcode = Path::new(OsStr::from_bytes(&xcode)).with_file_name("as");

            let p_as = Command::new(&xcode)
                .args(["-o", out_object_str, out_asm_str])
                .output()
                .unwrap();
            if !p_as.stderr.is_empty() {
                return Err(Box::from(String::from_utf8_lossy(&p_as.stderr).to_string()));
            }
            // find libSystem.dylib
            let lib_system_path = Command::new("xcrun")
                .args(["-sdk", "macosx", "--show-sdk-path"])
                .output()
                .unwrap()
                .stdout;
            let lib_system_path = String::from_utf8(lib_system_path)?;
            let lib_system_path = lib_system_path.trim();
            let xcode = xcode.with_file_name("ld");
            let p_ld = Command::new(xcode)
                .args([
                    "-o",
                    out_binary_str,
                    out_object_str,
                    "-lSystem",
                    "-syslibroot",
                    lib_system_path,
                    "-e",
                    "_main",
                    "-arch",
                    "arm64",
                ])
                .output()
                .unwrap();
            if !p_ld.stderr.is_empty() {
                return Err(Box::from(String::from_utf8_lossy(&p_ld.stderr).to_string()));
            }

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
