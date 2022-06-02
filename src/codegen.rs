use std::collections::{HashMap, VecDeque};
use std::path::Path;

use crate::ast::scope::Entity;
use crate::ast::types::Type;
use crate::parser::cblistener::CbListener;
use crate::parser::{self, cbvisitor};

use crate::parser::cbparser::{CbParser, CbParserContextType, CbParserExt, LocalTokenFactory};
use antlr_rust::parser::ParserNodeType;
use antlr_rust::token_stream::TokenStream;
use antlr_rust::tree::{ParseTreeVisitor, VisitChildren, VisitableDyn};
use antlr_rust::{DefaultErrorStrategy, TidAble};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::BasicTypeEnum;
use inkwell::values::FunctionValue;

pub struct CodeGen<'input, I>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
{
    source_path: String,
    context: &'input Context,
    module: Module<'input>,
    builder: Builder<'input>,
    parser: CbParser<'input, I, DefaultErrorStrategy<'input, CbParserContextType>>,
    // current function block
    current_function: Option<(FunctionValue<'input>, Type)>,
    // break labels (in loop statements)
    break_labels: VecDeque<BasicBlock<'input>>,
    // continue labels (in loop statements)
    continue_labels: VecDeque<BasicBlock<'input>>,
}

impl<'input, I> ParseTreeVisitor<'input, CbParserContextType> for CodeGen<'input, I> where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>
{
}

impl<'input, I> cbvisitor::CbVisitor<'input> for CodeGen<'input, I>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
{
    fn visit_compUnit(&mut self, ctx: &crate::parser::cbparser::CompUnitContext<'input>) {
        // define global function and variables
        let scope = &self.parser.scope;
        println!("{scope:?}");
    }
}

impl<'input, I> CodeGen<'input, I>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
{
    pub fn new(
        context: &'input Context,
        source_path: String,
        parser: CbParser<'input, I, DefaultErrorStrategy<'input, CbParserContextType>>,
    ) -> CodeGen<'input, I> {
        let module_name = Path::new(&source_path)
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();
        let module = context.create_module(module_name.as_str());
        let builder = context.create_builder();

        CodeGen {
            source_path,
            parser,
            context,
            module,
            builder,
            current_function: None,
            break_labels: VecDeque::new(),
            continue_labels: VecDeque::new(),
        }
    }
}
