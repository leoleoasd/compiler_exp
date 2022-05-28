use std::cell::RefCell;
use std::fmt::Display;
use std::ops::Range;
use std::rc::Rc;

use antlr_rust::errors::ANTLRError;
use antlr_rust::errors::ANTLRError::LexerNoAltError;
use antlr_rust::token::GenericToken;
use antlr_rust::token::Token;
use antlr_rust::{error_listener::ErrorListener, recognizer::Recognizer};
use bit_set::BitSet;
use codespan_reporting::diagnostic::{self, Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use quick_error::quick_error;
pub struct CodeSpanListener<T> {
    file: SimpleFile<String, String>,
    code: String,
    tokens: Rc<Vec<GenericToken<T>>>,
}

impl<T> CodeSpanListener<T> {
    pub fn new(name: &str, code: &str, tokens: Rc<Vec<GenericToken<T>>>) -> Self {
        Self {
            file: SimpleFile::new(name.to_string(), code.to_string()),
            tokens,
            code: code.to_string(),
        }
    }
}

quick_error! {
    #[derive(Debug)]
    pub enum ParserError {
        TypeNotFound(name: String) {
            display("Type {} not found", name)
        }
        VariableRedefination(name: String, previous_index: Range<usize>) {
            display("Variable {} exists", name)
        }
        VariableNotFound(name: String) {
            display("Variable {} not found", name)
        }
    }
}

impl ParserError {
    fn diagnostic<T: Clone>(&self, file_id: &T, range: impl Into<Range<usize>>) -> Diagnostic<T> {
        match self {
            ParserError::TypeNotFound(name) => Diagnostic::error()
                .with_message(format!("Type {name} not found"))
                .with_labels(vec![Label::primary(file_id.clone(), range)
                    .with_message(format!("Type {name} not found"))]),
            ParserError::VariableRedefination(name, previously_occur) => Diagnostic::error()
                .with_message(format!("Variable {name} is already defined!"))
                .with_labels(vec![
                    Label::primary(file_id.clone(), range)
                        .with_message(format!("Variable {name} is already defined!")),
                    Label::secondary(file_id.clone(), previously_occur.clone())
                        .with_message(format!("Previously defined here")),
                ]),
            ParserError::VariableNotFound(name) => Diagnostic::error()
                .with_message(format!("Variable {name} not found"))
                .with_labels(vec![Label::primary(file_id.clone(), range)
                    .with_message(format!("Variable {name} not found"))]),
        }
    }
}

impl<'a, T, T2> ErrorListener<'a, T> for CodeSpanListener<T2>
where
    T: Recognizer<'a>,
{
    fn report_ambiguity(
        &self,
        _recognizer: &T,
        _dfa: &antlr_rust::dfa::DFA,
        _start_index: isize,
        _stop_index: isize,
        _exact: bool,
        _ambig_alts: &BitSet,
        _configs: &antlr_rust::atn_config_set::ATNConfigSet,
    ) {
        let diagnostic = Diagnostic::error()
            .with_message("Found ambiguity ")
            .with_labels(vec![
                Label::primary((), 328..331).with_message("expected `String`, found `Nat`")
            ]);
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();
        term::emit(&mut writer.lock(), &config, &self.file, &diagnostic).unwrap();
    }

    fn report_attempting_full_context(
        &self,
        _recognizer: &T,
        _dfa: &antlr_rust::dfa::DFA,
        _start_index: isize,
        _stop_index: isize,
        _conflicting_alts: &BitSet,
        _configs: &antlr_rust::atn_config_set::ATNConfigSet,
    ) {
    }

    fn report_context_sensitivity(
        &self,
        _recognizer: &T,
        _dfa: &antlr_rust::dfa::DFA,
        _start_index: isize,
        _stop_index: isize,
        _prediction: isize,
        _configs: &antlr_rust::atn_config_set::ATNConfigSet,
    ) {
    }

    fn syntax_error(
        &self,
        _recognizer: &T,
        _offending_symbol: Option<&<<T>::TF as antlr_rust::token_factory::TokenFactory<'a>>::Inner>,
        _line: isize,
        _column: isize,
        _msg: &str,
        _error: Option<&antlr_rust::errors::ANTLRError>,
    ) {
        // Try to get from symbol first
        let range = _offending_symbol.map_or_else(
            || {
                // If isn't fesiable, get index from line and col
                if let Some(LexerNoAltError { start_index }) = _error {
                    return *start_index as usize..*start_index as usize + 1;
                }
                let mut line = 1;
                let mut col = 1;
                for (i, char) in self.code.chars().enumerate() {
                    if char == '\n' {
                        line += 1;
                        col = 1;
                    }
                    if line == _line && col == _column {
                        return i..i + 1;
                    }
                }
                // And finally defaults to 0..0
                0..0
            },
            |s| s.get_start() as usize..s.get_stop() as usize + 1,
        );

        let diagnostic = if let Some(ANTLRError::FallThrough(err)) = _error {
            if err.is::<ParserError>() {
                let err = err.downcast_ref::<ParserError>().unwrap();
                err.diagnostic(&(), range)
            } else {
                Diagnostic::error()
                    .with_message(_msg)
                    .with_labels(vec![Label::primary((), range).with_message(_msg)])
            }
        } else {
            Diagnostic::error()
                .with_message(_msg)
                .with_labels(vec![Label::primary((), range).with_message(_msg)])
        };
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();
        term::emit(&mut writer.lock(), &config, &self.file, &diagnostic).unwrap();
    }
}
