use std::cell::RefCell;
use std::rc::Rc;

use antlr_rust::token::GenericToken;
use antlr_rust::token::Token;
use antlr_rust::{error_listener::ErrorListener, recognizer::Recognizer};
use bit_set::BitSet;
use codespan_reporting::diagnostic::{self, Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use antlr_rust::errors::ANTLRError;
use antlr_rust::errors::ANTLRError::LexerNoAltError;
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
            code: code.to_string()
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
        let mut diagnostic =
            Diagnostic::error()
                .with_message(_msg)
                .with_labels(vec![Label::primary(
                    (),
                    _offending_symbol
                        .map_or_else(|| {
                            if let Some(LexerNoAltError{start_index}) = _error {
                                return *start_index as usize..*start_index as usize+1;
                            }
                            let mut line = 1;
                            let mut col = 1;
                            for (i, char) in self.code.chars().enumerate() {
                                if char == '\n' {
                                    line += 1;
                                    col = 1;
                                    break;
                                }
                                if line == _line && col == _column {
                                    return i..i+1;
                                }
                            }
                            0..0
                        }, |s| s.get_start() as usize..s.get_stop() as usize + 1),
                )
                .with_message(_msg)]);
        if let Some(err) = _error {
            diagnostic = diagnostic.with_notes(vec![_error.map(|e| e.to_string()).unwrap()]);
        }
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();
        term::emit(&mut writer.lock(), &config, &self.file, &diagnostic).unwrap();
    }
}
