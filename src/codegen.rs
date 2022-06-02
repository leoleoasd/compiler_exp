use std::collections::VecDeque;
use std::path::Path;
use std::rc::Rc;
use std::sync::Arc;

use crate::ast::scope::{Entity, FunctionEntity, Scope, VariableEntity};
use crate::ast::types::Type;

use crate::parser::cbparser::StmtsContextAttrs;
use crate::parser::cbparser::*;

use antlr_rust::token_stream::TokenStream;

use antlr_rust::{DefaultErrorStrategy, TidAble};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};
use inkwell::AddressSpace;

pub struct CodeGen<I>
where
    I: TokenStream<'static, TF = LocalTokenFactory<'static>> + TidAble<'static>,
{
    source_path: String,
    context: &'static Context,
    module: &'static Module<'static>,
    builder: Builder<'static>,
    parser: CbParser<'static, I, DefaultErrorStrategy<'static, CbParserContextType>>,
    // current function block
    current_function: Option<(FunctionValue<'static>, Arc<Type>)>,
    // break labels (in loop statements)
    break_labels: VecDeque<BasicBlock<'static>>,
    // continue labels (in loop statements)
    continue_labels: VecDeque<BasicBlock<'static>>,
}
impl<I> CodeGen<I>
where
    I: TokenStream<'static, TF = LocalTokenFactory<'static>> + TidAble<'static>,
{
    pub fn gen(&mut self, ctx: Rc<CompUnitContext<'static>>) {
        // define global function and variables
        {
            let scope: &Scope = &self.parser.scope;
            println!("{:#?}", scope.root);
            let root = scope.root.borrow();
            for ent in root.entities.values() {
                match &mut *ent.borrow_mut() {
                    Entity::Variable(VariableEntity {
                        name,
                        location,
                        init_expr,
                        _type,
                        llvm,
                    }) => {
                        let llvm_type = _type.to_llvm_type(self.context);
                        let llvm_ptr = self.module.add_global(
                            llvm_type,
                            Some(AddressSpace::Local),
                            name.as_str(),
                        );
                        if let Some(init_expr) = init_expr {
                            let expr = init_expr.cast_value(
                                self.context,
                                self.module,
                                &self.builder,
                                _type.clone(),
                            );
                            // todo: better error handling
                            match expr {
                                BasicValueEnum::ArrayValue(v) => assert!(v.is_const()),
                                BasicValueEnum::IntValue(v) => assert!(v.is_const()),
                                BasicValueEnum::PointerValue(v) => assert!(v.is_const()),
                                BasicValueEnum::VectorValue(v) => assert!(v.is_const()),
                                BasicValueEnum::FloatValue(v) => unreachable!(),
                                BasicValueEnum::StructValue(v) => unreachable!(),
                            }
                            println!("{:?}", expr);
                            llvm_ptr.set_initializer(&expr);
                        } else {
                            llvm_ptr.set_initializer(&llvm_type.const_zero());
                        }
                        *llvm = Some(llvm_ptr.as_pointer_value());
                    }
                    Entity::Function(FunctionEntity {
                        name,
                        location,
                        _type,
                        _extern,
                        llvm,
                    }) => {
                        let (return_type, args, variadic) = if let Type::Function {
                            return_type,
                            parameters,
                            variadic,
                        } = &**_type
                        {
                            (return_type, parameters, variadic)
                        } else {
                            unreachable!()
                        };
                        let param_type: Vec<BasicMetadataTypeEnum> = args
                            .iter()
                            .map(|t| t.1.to_llvm_type(self.context).into())
                            .collect();
                        let fn_type = match &**return_type {
                            Type::Void => self.context.void_type().fn_type(&param_type, *variadic),
                            Type::Integer { .. } | Type::Pointer { .. } | Type::Struct { .. } => {
                                match return_type.to_llvm_type(self.context) {
                                    BasicTypeEnum::IntType(t) => t.fn_type(&param_type, *variadic),
                                    BasicTypeEnum::PointerType(t) => {
                                        t.fn_type(&param_type, *variadic)
                                    }
                                    BasicTypeEnum::StructType(t) => {
                                        t.fn_type(&param_type, *variadic)
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            _ => unreachable!(),
                        };
                        let func = self.module.add_function(
                            name.as_str(),
                            fn_type,
                            if *_extern {
                                Some(Linkage::External)
                            } else {
                                None
                            },
                        );
                        *llvm = Some(func);
                    }
                }
            }
        }
        for top_def in ctx.topDef_all() {
            if let Some(func) = top_def.funcDef() {
                self.gen_func(func);
            }
        }
        self.module.print_to_file("test.ll").unwrap();
        self.module.print_to_stderr();
        self.module
            .verify()
            .map_err(|e| eprintln!("{}", e.to_str().unwrap()))
            .unwrap();
    }

    fn gen_func(&mut self, func: Rc<FuncDefContext<'static>>) {
        // todo!
        let ent = func.e.clone().unwrap();
        let mut ent = ent.borrow_mut();
        let func_entity = ent.as_function_mut();
        let llvm_func = func_entity.llvm.unwrap();
        let entry = self.context.append_basic_block(llvm_func, "entry");
        let func_arg_scope = func.scope.clone().unwrap();
        let func_type = func_entity._type.clone();
        self.builder.position_at_end(entry);
        self.current_function = Some((llvm_func, func_entity._type.clone()));

        if let Type::Function {
            return_type,
            parameters,
            variadic,
        } = &*func_type
        {
            for (index, arg) in parameters.iter().enumerate() {
                let e = func_arg_scope.borrow_mut();
                let arg_ent = e.entities.get(&arg.0).unwrap();
                let mut arg_ent = arg_ent.borrow_mut();
                let arg_ent = arg_ent.as_variable_mut();
                let alloca = self.builder.build_alloca(
                    arg.1.to_llvm_type(self.context),
                    &("_func_arg_".to_string() + &arg.0),
                );
                arg_ent.llvm = Some(alloca);
                let arg_value = self
                    .current_function
                    .as_ref()
                    .unwrap()
                    .0
                    .get_nth_param(index as u32)
                    .unwrap();
                self.builder.build_store(alloca, arg_value);
            }
        } else {
            unreachable!()
        }

        self.gen_block(func.body.as_ref().unwrap());

        let mut block_iter = llvm_func.get_first_basic_block();
        while block_iter.is_some() {
            let block = block_iter.unwrap();
            if block.get_terminator().is_none() {
                let terminator_builder = self.context.create_builder();
                terminator_builder.position_at_end(block);
                if let Type::Function { return_type, .. } = &*func_type {
                    if return_type.is_void() {
                        terminator_builder.build_return(None);
                    } else {
                        let val = return_type.to_llvm_type(self.context);
                        terminator_builder.build_return(Some(&val.const_zero()));
                    }
                }
            }
            block_iter = block.get_next_basic_block();
        }
        llvm_func.verify(true);
        self.current_function = None;
    }

    fn gen_block(&mut self, block: &BlockContext<'static>) {
        let scope = block.scope.clone().unwrap();
        let mut scope = scope.borrow_mut();
        for (name, arg) in &mut scope.entities {
            let mut arg = arg.borrow_mut();
            let arg_ent = arg.as_variable_mut();
            let alloca = self
                .builder
                .build_alloca(arg_ent._type.to_llvm_type(self.context), name);
            arg_ent.llvm = Some(alloca);
        }
        for (name, arg) in &mut scope.entities {
            let mut arg = arg.borrow_mut();
            let arg_ent = arg.as_variable_mut();
            if let Some(init_expr) = &arg_ent.init_expr {
                let expr = init_expr.cast_value(
                    self.context,
                    self.module,
                    &self.builder,
                    arg_ent._type.clone(),
                );
                self.builder.build_store(arg_ent.llvm.unwrap(), expr);
            } else {
                self.builder.build_store(
                    arg_ent.llvm.unwrap(),
                    arg_ent._type.to_llvm_type(self.context).const_zero(),
                );
            }
        }
        for stmt in block.body.clone().unwrap().stmt_all() {
            self.gen_stmt(stmt.as_ref());
        }
    }

    fn gen_stmt(&mut self, stmt: &StmtContextAll<'static>) {
        match &*stmt {
            StmtContextAll::DowhileContext(_) => todo!(),
            StmtContextAll::ExprStmtContext(s) => self.gen_expr_stmt(s),
            StmtContextAll::BlockStmtContext(b) => self.gen_block(b.block().as_ref().unwrap()),
            StmtContextAll::BreakContext(_) => todo!(),
            StmtContextAll::ForContext(_) => todo!(),
            StmtContextAll::ContineContext(_) => todo!(),
            StmtContextAll::WhileContext(_) => todo!(),
            StmtContextAll::IfContext(s) => self.gen_if_stmt(s.ifStmt().as_ref().unwrap()),
            StmtContextAll::ReturnContext(s) => {
                let r = s.returnStmt();
                let expr = r.as_ref().unwrap().expr();
                let expr = expr
                    .as_ref()
                    .unwrap()
                    .e
                    .as_ref()
                    .map(|expr| expr.value(self.context, self.module, &self.builder));
                let expr = expr.as_ref().map(|v| v as &dyn BasicValue);
                self.builder.build_return(expr);
            }
            StmtContextAll::EmptyContext(_) => {
                // do nothing
            }
            StmtContextAll::Error(_) => unreachable!(),
        }
    }

    fn gen_if_stmt(&mut self, stmt: &IfStmtContext<'static>) {
        let cond = stmt.cond.clone().unwrap();
        let cond = cond.e.as_ref().unwrap();
        let cond = cond.value(self.context, self.module, &self.builder);
        let func = self.current_function.as_ref().unwrap().0;
        let then_block = self.context.append_basic_block(func, "then_block");
        let else_block = self.context.append_basic_block(func, "else_block");
        let merge_block = self.context.append_basic_block(func, "merge_block");

        self.builder
            .build_conditional_branch(cond.into_int_value(), then_block, else_block);
        self.builder.position_at_end(then_block);
        self.gen_stmt(stmt.thenStmt.clone().unwrap().as_ref());

        if self.no_terminator() {
            self.builder.build_unconditional_branch(merge_block);
        }

        // build else_block
        self.builder.position_at_end(else_block);
        self.gen_stmt(stmt.elseStmt.clone().unwrap().as_ref());

        if self.no_terminator() {
            self.builder.build_unconditional_branch(merge_block);
        }

        self.builder.position_at_end(merge_block);
    }

    fn gen_expr_stmt(&mut self, stmt: &ExprStmtContext<'static>) {
        // this should do the work
        let expr = stmt.expr();
        let expr = expr.clone().unwrap();
        let expr = expr.e.as_ref();
        expr.unwrap()
            .value(self.context, self.module, &self.builder);
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
            module: Box::leak(Box::new(module)),
            builder,
            current_function: None,
            break_labels: VecDeque::new(),
            continue_labels: VecDeque::new(),
        }
    }

    // check if a basic block has no terminator
    fn no_terminator(&self) -> bool {
        let block = self.builder.get_insert_block();
        block.unwrap().get_terminator().is_none()
    }
}
