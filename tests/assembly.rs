use std::{env, fmt::Write, path::Path};

use anyhow::Context;

use cafe::{asm::Aarch64, compiler::Compiler};

#[test]
fn generate_assembly() -> anyhow::Result<()> {
    for entry in std::fs::read_dir("./tests/cafe")? {
        let path = entry?.path();
        if path.extension().unwrap_or_default() == "cafe" {
            run_test(&path, &path.with_extension("S"))?;
        }
    }
    Ok(())
}

fn run_test(input_path: &Path, expected_path: &Path) -> anyhow::Result<()> {
    if !expected_path.exists() {
        std::fs::File::create(expected_path)?;
    }
    let mut expected = std::fs::read_to_string(expected_path)
        .with_context(|| format!("failed to read from {:?}", expected_path))?;
    let mut compiler = Compiler::new(Aarch64);
    let insts = format!(
        "{}\n{}",
        compiler.runtime(),
        compiler.compile_file(input_path)?
    );

    if env::var("UPDATE_EXPECT").unwrap_or_default() == "1" {
        writeln!(expected, "{insts}")?;
    } else {
        assert_eq!(
            expected.trim(),
            insts.trim(),
            "compiling {:?}, comparing with {:?}",
            input_path,
            expected_path
        );
    }

    Ok(())
}
