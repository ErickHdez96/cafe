use std::{io::Write, path::Path, process::Command};

use cafe::{asm::Aarch64, compiler::Compiler, driver::compile_asm};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[test]
#[ignore]
fn generate_assembly() -> Result<()> {
    for entry in std::fs::read_dir("./tests/cafe")? {
        let path = entry?.path();
        if path.extension().unwrap_or_default() == "cafe" {
            run_test(&path)?;
        }
    }
    Ok(())
}

fn run_test(source_path: &Path) -> Result<()> {
    let mut compiler = Compiler::new(Aarch64);
    let insts = compiler.compile_file(source_path)?;
    let asm_path = {
        let path = source_path.with_extension("S");
        let mut file = std::fs::File::create(&path)?;
        writeln!(file, "{}", compiler.runtime())?;
        writeln!(file, "{}", insts)?;
        path
    };
    let binary_path = source_path.with_extension("");

    compile_asm(&asm_path, &binary_path)?;
    run_binary(&source_path, &binary_path)?;

    Ok(())
}

fn run_binary(source_path: &Path, binary_path: &Path) -> Result<()> {
    let (_input, expected_output, expected_exit_code) = {
        let content = std::fs::read_to_string(source_path)?;
        let mut contents = content.lines();
        let mut input = vec![];
        let mut expected_output = vec![];

        assert_eq!(
            Some("#|"),
            contents.next(),
            "file must begin with multi-line comment"
        );
        for line in contents.by_ref() {
            if line == "|#" {
                break;
            }
            input.push(line);
        }
        assert_eq!(
            Some("#|"),
            contents.next(),
            "expected output for the program must come after the input"
        );
        for line in contents.by_ref() {
            if line == "|#" {
                break;
            }
            expected_output.push(line);
        }
        let exit_code = contents.next().unwrap_or_default();
        assert_eq!(
            Some(';'),
            exit_code.chars().next(),
            "expected exit code must come after the expected output"
        );

        (
            input.join("\n"),
            expected_output.join("\n"),
            (&exit_code[1..]).trim().to_string(),
        )
    };

    let result = Command::new(binary_path.to_str().unwrap()).output()?;

    assert_eq!(
        expected_output.as_bytes(),
        result.stdout,
        "{source_path:?}: expected output from the progam"
    );
    assert_eq!(
        expected_exit_code,
        result
            .status
            .code()
            .map(|s| s.to_string())
            .unwrap_or_default(),
        "{source_path:?}: expected exit code"
    );

    Ok(())
}
