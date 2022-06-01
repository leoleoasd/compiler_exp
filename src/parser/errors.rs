#![allow(clippy::extra_unused_lifetimes)]

use std::borrow::Borrow;
use std::ops::Range;
use std::rc::Rc;

use antlr_rust::errors::ANTLRError;
use antlr_rust::errors::ANTLRError::LexerNoAltError;
use antlr_rust::token::GenericToken;
use antlr_rust::token::Token;
use antlr_rust::{error_listener::ErrorListener, recognizer::Recognizer};
use bit_set::BitSet;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use quick_error::quick_error;
use snailquote::UnescapeError;

use crate::ast::types::Type;
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
        EntityNameConflict(name: String, previous_index: Range<usize>) {
            display("Variable {} exists", name)
        }
        VariableNotFound(name: String) {
            display("Variable {} not found", name)
        }
        InvalidEscapeSequence(text: String, err: UnescapeError) {
            display("Invalid escape sequence: {}, {:?}", text, err)
            context(text: String, err: UnescapeError) -> (text, err)
        }
        InvalidCharacterLiteral(text: String) {
            display("Invalid character literal: {}", text)
        }
        VariableUndefined(name: String) {
            display("Variable {} is undefined", name)
        }
        InvalidFunctionDefination(name: String) {
            display("Cannot define function {} here", name)
        }
        DuplicateStructField(name: String,  previous_index: Range<usize>) {
            display("Duplicate field {}", name)
        }
        TypeMismatch(expected: String, found: String, location: Range<usize>) {
            display("Type mismatch, expected {}, found {}", expected, found)
        }
        AddressableOprandRequired(location: Range<usize>) {
            display("Addressable operand is required")
        }
        ArgumentCountMismatch(expected: usize, found: usize) {
            display("Function call parameter count mismatch, expected {}, found {}", expected, found)
        }
        ArgumentTypeMismatch(index: usize, expected: String, found: String) {
            display("Function call parameter type mismatch at {}, expected {}, found {}", index, expected, found)
        }
        FieldNotFound(field: String, struct_name: String, struct_location: Range<usize>) {
            display("Field {} not found of struct {}", field, struct_name)
        }
        IncapableTypeCast(from: String, to: String) {
            display("Cannot cast {} to {}", from, to)
        }
    }
}

impl ParserError {
    fn diagnostic<T: Clone>(&self, file_id: &T, range: impl Into<Range<usize>>) -> Diagnostic<T> {
        let range: Range<usize> = range.into();
        match self {
            ParserError::TypeNotFound(name) => Diagnostic::error()
                .with_message(format!("Type {name} not found"))
                .with_labels(vec![Label::primary(file_id.clone(), range)
                    .with_message(format!("Type {name} not found"))]),
            ParserError::EntityNameConflict(name, previously_occur) => Diagnostic::error()
                .with_message(format!("Entity {name} is already defined!"))
                .with_labels(vec![
                    Label::primary(file_id.clone(), range)
                        .with_message(format!("Entity {name} is already defined!")),
                    Label::secondary(file_id.clone(), previously_occur.clone())
                        .with_message("Previously defined here".to_string()),
                ]),
            ParserError::VariableNotFound(name) => Diagnostic::error()
                .with_message(format!("Variable {name} not found"))
                .with_labels(vec![Label::primary(file_id.clone(), range)
                    .with_message(format!("Variable {name} not found"))]),
            ParserError::InvalidEscapeSequence(text, err) => Diagnostic::error()
                .with_message(format!("Invalid escape sequence: {text} {err:?}"))
                .with_labels(vec![Label::primary(file_id.clone(), range)
                    .with_message(format!("Invalid escape sequence: {text} {err:?}"))]),
            ParserError::InvalidCharacterLiteral(text) => Diagnostic::error()
                .with_message(format!("Invalid character literal: {text}"))
                .with_labels(vec![
                    Label::primary(file_id.clone(), range.clone())
                        .with_message(format!("Invalid character literal: {text}")),
                    Label::secondary(file_id.clone(), range)
                        .with_message("Note: only ASCII characters are supported"),
                ]),
            ParserError::VariableUndefined(name) => Diagnostic::error()
                .with_message(format!("Variable {name} is undefined"))
                .with_labels(vec![Label::primary(file_id.clone(), range)
                    .with_message(format!("Variable {name} is undefined"))]),
            ParserError::InvalidFunctionDefination(name) => Diagnostic::error()
                .with_message(format!("Cannot define function {name} here"))
                .with_labels(vec![Label::primary(file_id.clone(), range)
                    .with_message(format!("Cannot define function {name} here"))]),
            ParserError::DuplicateStructField(name, previously_occur) => Diagnostic::error()
                .with_message(format!("Duplicate field {name}"))
                .with_labels(vec![
                    Label::primary(file_id.clone(), range.clone())
                        .with_message(format!("Duplicate field {name}")),
                    Label::secondary(file_id.clone(), previously_occur.clone())
                        .with_message("Previously used here".to_string()),
                ]),
            ParserError::TypeMismatch(expected, found, location) => Diagnostic::error()
                .with_message(format!(
                    "Type mismatch, expected {}, found {}",
                    expected, found
                ))
                .with_labels(vec![Label::primary(file_id.clone(), location.clone())
                    .with_message(format!(
                        "Type mismatch, expected {}, found {}",
                        expected, found
                    ))]),
            ParserError::AddressableOprandRequired(location) => Diagnostic::error()
                .with_message("Addressable operand is required")
                .with_labels(vec![Label::primary(file_id.clone(), location.clone())
                    .with_message("Addressable operand is required")]),
            ParserError::ArgumentCountMismatch(expected, found) => Diagnostic::error()
                .with_message(format!(
                    "Function call parameter count mismatch, expected {}, found {}",
                    expected, found
                ))
                .with_labels(vec![Label::primary(file_id.clone(), range).with_message(
                    format!(
                        "Function call parameter count mismatch, expected {}, found {}",
                        expected, found
                    ),
                )]),
            ParserError::ArgumentTypeMismatch(index, expected, found) => Diagnostic::error()
                .with_message(format!(
                    "Function call parameter type mismatch at {}, expected {}, found {}",
                    index, expected, found
                ))
                .with_labels(vec![Label::primary(file_id.clone(), range).with_message(
                    format!(
                        "Function call parameter type mismatch at {}, expected {}, found {}",
                        index, expected, found
                    ),
                )]),
            ParserError::FieldNotFound(field, struct_name, struct_location) => Diagnostic::error()
                .with_message(format!(
                    "Field {} not found of struct {}",
                    field, struct_name
                ))
                .with_labels(vec![
                    Label::primary(file_id.clone(), range.clone()).with_message(format!(
                        "Field {} not found of struct {}",
                        field, struct_name
                    )),
                    Label::secondary(file_id.clone(), struct_location.clone())
                        .with_message("Struct {} defined here".to_string()),
                ]),
            ParserError::IncapableTypeCast(from, to) => Diagnostic::error()
                .with_message(format!("Cannot cast {} to {}", from, to))
                .with_labels(vec![Label::primary(file_id.clone(), range)
                    .with_message(format!("Cannot cast {} to {}", from, to))]),
        }
    }
}

impl From<ParserError> for ANTLRError {
    fn from(err: ParserError) -> Self {
        ANTLRError::FallThrough(Rc::new(err))
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
        let location = self.tokens[_start_index as usize].start as usize
            ..self.tokens[_stop_index as usize].stop as usize;
        let diagnostic = Diagnostic::error()
            .with_message("Found ambiguity")
            .with_labels(vec![
                Label::primary((), location).with_message(format!("{:?}", _ambig_alts))
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
