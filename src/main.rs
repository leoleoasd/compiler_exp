#![feature(try_blocks)]

use clap::Parser;
use std::{process::{Command, Stdio}, io, path::Path};
mod parser;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    /// File name to compile
    name: String,

    #[clap(short, long)]
    /// Show lexer result for debug
    lex: bool,

    #[clap(short, long)]
    /// Show preprocessor result for debug
    preprocess: bool,
}

fn main() {
    let cli = Cli::parse();
    let code = preprocess(&cli.name).unwrap();
    if cli.preprocess {
        println!("Preprocessed Code:");
        println!("{}", code);
    }
}

fn preprocess(file: &str) -> Result<String, io::Error> {
    let file_path = Path::new(file);
    // call cpp on file
    #[allow(unused)]
    let output = Command::new("cpp")
        .arg(file)
        .arg("-P")
        .arg("-I")
        .arg(file_path.parent().unwrap())
        .stdout(Stdio::piped())
        .spawn()
        .expect("antlr tool failed to start")
        .wait_with_output()?;
    let code = String::from_utf8(output.stdout).unwrap();
    Ok(code)
}
