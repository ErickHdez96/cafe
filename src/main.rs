use cafe::{
    diagnostics::{emit_diagnostic, emit_diagnostics},
    query::BuildSystem,
};

fn main() {
    let qctx = BuildSystem::default();
    let mut args = std::env::args();

    match qctx.parse(args.nth(1).expect("USAGE: cafe [INPUT]").into()) {
        Ok(f) => {
            println!("{:#?}", f.tree);
            emit_diagnostics(&qctx, &f.diagnostics);
        }
        Err(e) => {
            emit_diagnostic(&qctx, &e);
        }
    }
}
