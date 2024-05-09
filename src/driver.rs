use std::path::Path;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[cfg(target_os = "macos")]
pub fn compile_asm(asm_path: &Path, out_path: &Path) -> Result<()> {
    use std::{ffi::OsStr, os::unix::ffi::OsStrExt, process::Command};

    let out_object_path = asm_path.with_extension("o");

    let out_asm_str = asm_path.to_str().unwrap();
    let out_binary_str = out_path.to_str().unwrap();
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
