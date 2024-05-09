use std::{env, path::Path};

use anyhow::Context;

use cafe::{asm::Aarch64, compiler::Compiler};

#[test]
fn generate_assembly() -> anyhow::Result<()> {
    for entry in std::fs::read_dir("./tests/cafe")? {
        let path = entry?.path();
        if path.extension().unwrap() == "cafe" {
            run_test(&path, &path.with_extension("S"))?;
        }
    }
    Ok(())
}

fn run_test(input_path: &Path, expected_path: &Path) -> anyhow::Result<()> {
    let expected = std::fs::read_to_string(expected_path)
        .with_context(|| format!("failed to read from {:?}", expected_path))?;
    let mut compiler = Compiler::new(Aarch64);
    let insts = compiler.compile_file(input_path)?.to_string();

    if env::var("UPDATE_EXPECT").unwrap_or_default() == "1" {
        std::fs::write(expected_path, &insts)?;
    } else {
        assert_eq!(
            expected, insts,
            "compiling {:?}, comparing with {:?}",
            input_path, expected_path
        );
    }

    Ok(())
}
