#![feature(try_blocks)]
#![feature(trait_upcasting)]
#![feature(io_read_to_string)]
#![allow(dead_code)]
#![allow(unused_variables)]

use antlr_rust::common_token_stream::CommonTokenStream;
use antlr_rust::token_stream::TokenStream;

use antlr_rust::Parser;
use antlr_rust::{int_stream::IntStream, token::Token, InputStream};
use clap::Parser as ClapParser;
use inkwell::context::Context;
use parser::cbparser::CbParser;
use parser::errors::CodeSpanListener;
use std::fs;
use std::io::{BufReader,Read, BufRead, Write};
use std::{
    io,
    ops::Deref,
    process::{Command, Stdio},
    rc::Rc,
};

use crate::codegen::CodeGen;
use crate::parser::{cblexer, errors};
mod ast;
mod codegen;
mod parser;

#[derive(ClapParser, Debug)]
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
    let code;
    
    let reader: Box<dyn BufRead> = match cli.name.as_str() {
        "-" => Box::new(BufReader::new(io::stdin())),
        _ => Box::new(BufReader::new(fs::File::open(&cli.name).unwrap())),
    };
    if cli.preprocess {
        code = Box::leak(Box::new(preprocess(reader).unwrap()));
        println!("Preprocessed Code:");
        println!("{}", code);
    } else {
        code = Box::leak(Box::new(io::read_to_string(reader).unwrap()));
    }
    let mut tokens = lex(code, cli.name.clone()).unwrap();
    let mut token_vec = Vec::new();
    for i in 1.. {
        let token = match tokens.lt(i) {
            Some(token) => token,
            None => break,
        };
        if token.get_token_type() == antlr_rust::token::TOKEN_EOF {
            break;
        }
        token_vec.push(*token.clone());

        if cli.lex {
            let rule_name = if (token.get_token_type() as usize) < cblexer::_LITERAL_NAMES.len() {
                cblexer::_LITERAL_NAMES[token.get_token_type() as usize].unwrap()
            } else {
                cblexer::_SYMBOLIC_NAMES[token.get_token_type() as usize].unwrap()
            };
            println!(
                "{: <3} {: <20} at {}:{}",
                token.get_token_type(),
                rule_name,
                token.line,
                token.column
            );
        }
    }
    tokens.seek(0);
    let token_vec = Rc::new(token_vec);
    let mut parser = CbParser::new(tokens);
    let listener = errors::CodeSpanListener::new(&cli.name, code, token_vec.clone());
    parser.remove_error_listeners();
    parser.add_error_listener(Box::new(listener));
    let result = parser.compUnit();
    let result = match result {
        Ok(result) => result,
        Err(err) => {
            println!("Got error: {}, aborting", err);
            return;
        }
    };
    let context: &'static mut Context = Box::leak(Box::new(Context::create()));
    let codegen = Box::leak(Box::new(CodeGen::new(
        context,
        cli.name.to_string(),
        parser,
    )));
    codegen.gen(result);
    let command = Command::new("clang")
        .arg("test.ll")
        .arg("-o")
        .arg("test")
        .stdout(Stdio::piped())
        .spawn()
        .expect("antlr tool failed to start")
        .wait_with_output().unwrap();
}

fn preprocess<T>(file: T)  -> Result<String, io::Error> where T: Read {
    let code = io::read_to_string(file)?;
    // call cpp on file
    #[allow(unused)]
    let child = Command::new("cpp")
        .arg("-")
        .arg("-P")
        .arg("-w")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;
    child.stdin.as_ref().unwrap().write_all(code.as_bytes())?;
    let output = child.wait_with_output()?;
    let code = String::from_utf8(output.stdout).unwrap();
    Ok(code)
}

fn lex(
    code: &String,
    name: String,
) -> std::result::Result<
    CommonTokenStream<parser::cblexer::CbLexer<antlr_rust::InputStream<&str>>>,
    (),
> {
    let stream = InputStream::new(code.deref());
    let mut lexer = cblexer::CbLexer::new(stream);
    let listener: CodeSpanListener<&str> =
        errors::CodeSpanListener::new(&name, code, Rc::new(vec![]));
    lexer.remove_error_listeners();
    lexer.add_error_listener(Box::new(listener));
    let token_source = CommonTokenStream::new(lexer);
    Ok(token_source)
}
