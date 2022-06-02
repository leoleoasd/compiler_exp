use std::collections::{VecDeque};
use std::path::Path;
use std::rc::Rc;
use std::sync::Arc;

use crate::ast::scope::{Entity, Scope};
use crate::ast::types::Type;
use crate::parser::cblistener::CbListener;
use crate::parser::cbparser::{
    CbParser, CbParserContextType, CbParserExt, CompUnitContext, LocalTokenFactory,
};
use antlr_rust::parser::ParserNodeType;
use antlr_rust::token_stream::TokenStream;
use antlr_rust::tree::{ParseTreeVisitor, VisitChildren, VisitableDyn};
use antlr_rust::{DefaultErrorStrategy, TidAble};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FunctionValue, InstructionOpcode};
use inkwell::AddressSpace;

pub struct CodeGen<I>
where
    I: TokenStream<'static, TF = LocalTokenFactory<'static>> + TidAble<'static>,
{
    source_path: String,
    context: &'static Context,
    module: Module<'static>,
    builder: Builder<'static>,
    parser: CbParser<'static, I, DefaultErrorStrategy<'static, CbParserContextType>>,
    // current function block
    current_function: Option<(FunctionValue<'static>, Type)>,
    // break labels (in loop statements)
    break_labels: VecDeque<BasicBlock<'static>>,
    // continue labels (in loop statements)
    continue_labels: VecDeque<BasicBlock<'static>>,
}
impl<I> CodeGen<I>
where
    I: TokenStream<'static, TF = LocalTokenFactory<'static>> + TidAble<'static>,
{
    pub fn gen(&'static mut self, ctx: Rc<CompUnitContext<'static>>) {
        // define global function and variables
        let scope: &Scope = &self.parser.scope;
        println!("{:#?}", scope.root);
        let root = scope.root.borrow();
        for ent in root.entities.values() {
            match &mut *ent.borrow_mut() {
                Entity::Variable {
                    name,
                    location,
                    init_expr,
                    _type,
                    llvm,
                } => {
                    let llvm_type = _type.to_llvm_type(self.context);
                    let llvm_ptr =
                        self.module
                            .add_global(llvm_type, Some(AddressSpace::Local), name.as_str());
                    if let Some(init_expr) = init_expr {
                        // TODO: check
                        let expr = init_expr.value(self.context, &self.builder);
                        assert!(expr.is_int_value());
                        let to_type =
                            Type::binary_cast(_type.clone(), init_expr.get_type()).unwrap();
                        let expr = self.cast_value(expr, _type.clone(), to_type);
                        llvm_ptr.set_initializer(&expr);
                    } else {
                        llvm_ptr.set_initializer(&llvm_type.const_zero());
                    }
                    *llvm = Some(llvm_ptr.as_pointer_value());
                }
                Entity::Function {
                    name,
                    location,
                    _type,
                    _extern,
                    llvm,
                } => {
                    todo!();
                }
            }
        }
        // self.visit_children(ctx);
        self.module.print_to_stderr();
        self.module.print_to_file("test.llvm").unwrap();
        self.module.verify().unwrap();
    }

    fn cast_value(
        &self,
        value: BasicValueEnum<'static>,
        from_type: Arc<Type>,
        to_type: Arc<Type>,
    ) -> BasicValueEnum<'static> {
        if from_type == to_type {
            return value;
        }
        self.builder.build_cast(
            self.cast_op(&*from_type, &*to_type),
            value,
            to_type.to_llvm_type(self.context),
            "cast",
        )
    }
    fn cast_op(&self, from_type: &Type, to_type: &Type) -> InstructionOpcode {
        match (&*from_type, &*to_type) {
            (
                Type::Integer {
                    size: size1,
                    signed: signed1,
                },
                Type::Integer {
                    signed: signed2,
                    size: size2,
                },
            ) => {
                if size1 > size2 {
                    InstructionOpcode::Trunc
                } else if size1 < size2 {
                    if *signed1 {
                        InstructionOpcode::SExt
                    } else {
                        InstructionOpcode::ZExt
                    }
                } else {
                    InstructionOpcode::BitCast
                }
            }
            (Type::Pointer { .. }, Type::Pointer { .. }) => InstructionOpcode::BitCast,
            (Type::Pointer { .. }, Type::Integer { .. }) => InstructionOpcode::PtrToInt,
            (Type::Integer { .. }, Type::Pointer { .. }) => InstructionOpcode::IntToPtr,
            _ => panic!(
                "cast from {} to {} not supported",
                from_type.name(),
                to_type.name()
            ),
        }
    }
    pub fn new(
        context: &'static Context,
        source_path: String,
        parser: CbParser<'static, I, DefaultErrorStrategy<'static, CbParserContextType>>,
    ) -> CodeGen<I> {
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
