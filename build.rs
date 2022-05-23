use std::convert::TryInto;
use std::env;
use std::env::VarError;
use std::error::Error;
use std::fs::{read_dir, DirEntry, File};
use std::io::Write;
use std::path::Path;
use std::process::Command;

fn main() {
    gen_for_grammar().unwrap();

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/parser/Cb.g4");
}

fn gen_for_grammar(
) -> Result<(), Box<dyn Error>> {
    let input = env::current_dir().unwrap().join("src").join("parser");
    let file_name = "Cb.g4";

    let c = Command::new("java")
        .current_dir(input)
        .arg("org.antlr.v4.Tool")
        .arg("-Dlanguage=Rust")
        .arg(&file_name)
        .spawn()
        .expect("antlr tool failed to start")
        .wait_with_output()?;
    // .unwrap()
    // .stdout;
    // eprintln!("xx{}",String::from_utf8(x).unwrap());
    Ok(())
}