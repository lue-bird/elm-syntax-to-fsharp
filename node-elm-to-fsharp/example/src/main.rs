use std::io;
use std::io::Write;
#[path = "./Elm.fs.rs"]
mod elm;

pub fn main() {
    match io::read_to_string(io::stdin()) {
        Ok(input) => {
            io::stdout().write_all(
                elm::Elm::formatSingleModule_formatSingleModule(input.into()).as_bytes(),
            );
        }
        Err(_) => println!("failed to read input from stdin"),
    }
}
