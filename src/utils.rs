use std::fmt::Write;

use crate::symbol::Symbol;

pub fn mangle_symbol(symbol: Symbol) -> Symbol {
    let sym = symbol.resolve();
    if !sym.contains(' ') {
        symbol
    } else {
        let paths = sym.split(' ');
        paths
            .into_iter()
            .fold(String::from("_T"), |mut out, p| {
                let p = encode_symbol(p);
                let _ = write!(out, "{}{}", p.len(), p);
                out
            })
            .into()
    }
}

fn encode_symbol(symbol: &str) -> String {
    let mut out = String::with_capacity(symbol.len());

    for c in symbol.chars() {
        if c.is_ascii_alphanumeric() {
            out.push(c);
            continue;
        }

        out.push_str(&format!("_{}_", c as u32));
    }

    out
}
