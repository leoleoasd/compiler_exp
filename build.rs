use std::env;
use std::error::Error;
use std::process::Command;

fn main() {
    gen_for_grammar().unwrap();

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/parser/Cb.g4");
}

fn gen_for_grammar() -> Result<(), Box<dyn Error>> {
    let input = env::current_dir().unwrap().join("src").join("parser");
    let file_name = "Cb.g4";

    let _ = Command::new("java")
        .current_dir(input)
        .arg("org.antlr.v4.Tool")
        .arg("-Dlanguage=Rust")
        .arg("-visitor")
        .arg(&file_name)
        .spawn()
        .expect("antlr tool failed to start")
        .wait_with_output()?;
    let _ = Command::new("cargo")
        .current_dir(env::current_dir()?)
        .arg("fmt")
        .spawn()
        .expect("failed to format antlr code")
        .wait_with_output()?;

    // .unwrap()
    // .stdout;
    // eprintln!("xx{}",String::from_utf8(x).unwrap());
    Ok(())
}
