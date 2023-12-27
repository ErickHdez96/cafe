use std::io::{self, Write};

use cafe::{
    diagnostics::{emit_diagnostic, emit_diagnostics},
    expander::core_expander_interface,
    file::SourceFile,
    syntax::ast::ModuleName,
    BuildSystem,
};

pub fn repl() -> ! {
    let qctx = BuildSystem::default();
    qctx.feed_intrinsic_lib(core_expander_interface());
    qctx.feed_module_name((ModuleName::script(), "<script>".into()));
    let source_file = SourceFile::new("<script>", String::new());
    let mut input = String::new();

    loop {
        input.clear();
        print!(">> ");
        io::stdout().flush().unwrap();

        io::stdin().read_line(&mut input).unwrap();

        match input.trim() {
            ",help" => {
                println!("Cafe Scheme\n");
                println!("\t,help    Prints this message");
                println!("\t,q ,quit Exits the repl");
                continue;
            }
            ",q" | ",quit" => {
                std::process::exit(0);
            }
            "" if input.is_empty() => {
                println!();
                std::process::exit(0);
            }
            _ => {}
        }

        qctx.feed_source_file(SourceFile {
            id: source_file.id,
            path: source_file.path.clone(),
            contents: std::mem::take(&mut input),
        });
        qctx.mark_dirty(qctx.read_file as usize, &source_file.path);
        qctx.mark_dirty(qctx.file as usize, source_file.id);
        let res = match qctx.expand(ModuleName::script()) {
            Ok(res) => res,
            Err(d) => {
                emit_diagnostic(&qctx, &d);
                continue;
            }
        };

        if !res.diagnostics.is_empty() {
            emit_diagnostics(&qctx, &res.diagnostics);
            continue;
        }

        println!("{:#?}", res.module);
    }
}
