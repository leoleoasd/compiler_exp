#![feature(try_blocks)]

use antlr_rust::common_token_stream::CommonTokenStream;
use antlr_rust::token_stream::TokenStream;
use antlr_rust::Parser;
use antlr_rust::{
    int_stream::IntStream, token::Token, token_stream::UnbufferedTokenStream, InputStream,
};
use clap::Parser as ClapParser;
use parser::cbparser::CbParser;
use parser::errors::CodeSpanListener;
use std::error::Error;
use std::fmt::Display;
use std::{
    io,
    ops::Deref,
    path::Path,
    process::{Command, Stdio},
    rc::Rc,
};

use crate::parser::{cblexer, errors};
mod ast;
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
    let code = Box::leak(Box::new(preprocess(&cli.name).unwrap()));
    if cli.preprocess {
        println!("Preprocessed Code:");
        println!("{}", code);
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

        if cli.lex {
            token_vec.push(*token.clone());
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
    let listener = errors::CodeSpanListener::new(&cli.name, &code, token_vec.clone());
    parser.remove_error_listeners();
    parser.add_error_listener(Box::new(listener));
    let result = parser.compUnit().unwrap();
    println!("{:?}", result);
    println!("{:#?}", parser.types);
    println!("{:#?}", parser.scope);
}

fn preprocess(file: &str) -> Result<String, io::Error> {
    let file_path = Path::new(file);
    // call cpp on file
    #[allow(unused)]
    let output = Command::new("cpp")
        .arg(file)
        .arg("-P")
        .arg("-w")
        .arg("-I")
        .arg(file_path.parent().unwrap())
        .stdout(Stdio::piped())
        .spawn()
        .expect("antlr tool failed to start")
        .wait_with_output()?;
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
        errors::CodeSpanListener::new(&name, &code, Rc::new(vec![]));
    lexer.remove_error_listeners();
    lexer.add_error_listener(Box::new(listener));
    let token_source = CommonTokenStream::new(lexer);
    Ok(token_source)
}
