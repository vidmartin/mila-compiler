
use std::ffi::CString;

use llvm_sys as llvm;
use crate::ast::{self, ExpressionNode, StorageDeclarationNode};

#[derive(Debug)]
pub enum GenError {
    MissingModule,
    InvalidContext,
    InvalidName,
    InvalidDataType,
    NameConflict,
    TypeMismatch,
    UndefinedSymbol(String),
    InvalidScope,
    InvalidEncoding,
    InvalidMacroUsage,
    MissingCFunction,
    InvalidAssignment,
}

pub struct LlvmTypes {
    i64: *mut llvm::LLVMType,
    i32: *mut llvm::LLVMType,
    i16: *mut llvm::LLVMType,
    i8: *mut llvm::LLVMType,
    i1: *mut llvm::LLVMType,
    void: *mut llvm::LLVMType,
}

impl LlvmTypes {
    pub fn new(ctx: *mut llvm::LLVMContext) -> Self {
        unsafe {
            Self {
                i64: llvm_sys::core::LLVMInt64TypeInContext(ctx),
                i32: llvm_sys::core::LLVMInt32TypeInContext(ctx),
                i16: llvm_sys::core::LLVMInt16TypeInContext(ctx),
                i8: llvm_sys::core::LLVMInt8TypeInContext(ctx),
                i1: llvm_sys::core::LLVMInt1TypeInContext(ctx),
                void: llvm_sys::core::LLVMVoidTypeInContext(ctx),
            }
        }
    }

    pub fn get_type(&self, ast_type: Option<&ast::DataType>) -> Result<*mut llvm::LLVMType, GenError> {
        match ast_type {
            Some(ast::DataType::One(dtype)) => {
                match dtype.as_str() {
                    "integer" => Ok(self.i64),
                    _ => return Err(GenError::InvalidDataType),
                }
            },
            Some(ast::DataType::OneInternal(dtype)) => Ok(*dtype),
            Some(ast::DataType::Array { item, from, to }) => todo!(),
            None => Ok(self.void),
        }
    }
}

#[derive(Clone)]
pub struct BuiltInFunctions {
    // C standard library:
    printf: Option<TypedSymbol>,
    scanf: Option<TypedSymbol>,

    // hard-coded implementations:
    readln: Option<TypedSymbol>,
}

const C_FUNCTIONS_DEFAULT: BuiltInFunctions = BuiltInFunctions {
    printf: None,
    scanf: None,
    readln: None,
};

impl BuiltInFunctions {
    pub fn new() -> Self {
        C_FUNCTIONS_DEFAULT.clone()
    }
}

pub fn gen_printf(ctx: &mut GenContext) -> Result<TypedSymbol, GenError> {
    unsafe {
        let mut i8ptr = llvm::core::LLVMPointerType(ctx.types.i8, 0); // address space = 0 should be default
        let llvm_fn_type = llvm::core::LLVMFunctionType(ctx.types.i32, (&mut i8ptr) as *mut *mut llvm::LLVMType, 1, 1);
        let llvm_fn_value = llvm::core::LLVMAddFunction(ctx.module, b"printf\0".as_ptr() as *const i8, llvm_fn_type);
        return Ok(TypedSymbol {
            llvm_type: llvm_fn_type,
            llvm_value: llvm_fn_value,
        });
    }
}

pub fn gen_scanf(ctx: &mut GenContext) -> Result<TypedSymbol, GenError> {
    unsafe {
        let mut i8ptr = llvm::core::LLVMPointerType(ctx.types.i8, 0); // address space = 0 should be default
        let llvm_fn_type = llvm::core::LLVMFunctionType(ctx.types.i32, (&mut i8ptr) as *mut *mut llvm::LLVMType, 1, 1);
        let llvm_fn_value = llvm::core::LLVMAddFunction(ctx.module, b"scanf\0".as_ptr() as *const i8, llvm_fn_type);
        return Ok(TypedSymbol {
            llvm_type: llvm_fn_type,
            llvm_value: llvm_fn_value,
        });
    }
}

const ANON: *const i8 = b"\0".as_ptr() as *const i8;

pub fn gen_readln(ctx: &mut GenContext, scanf: &TypedSymbol) -> Result<TypedSymbol, GenError> {
    // hard coded implementation for readln functionality

    unsafe {
        let mut i64ptr = llvm::core::LLVMPointerType(ctx.types.i64, 0);
        let llvm_readln_fn_type = llvm::core::LLVMFunctionType(ctx.types.i64, (&mut i64ptr) as *mut *mut llvm::LLVMType, 1, 1);
        let llvm_readln_fn_value = llvm::core::LLVMAddFunction(ctx.module, b"readln\0".as_ptr() as *const i8, llvm_readln_fn_type);
        
        let bb = llvm::core::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, llvm_readln_fn_value, ANON);
        llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, bb);

        let llvm_ptr = llvm::core::LLVMGetParam(llvm_readln_fn_value, 0);
        
        let mut scanf_params = vec![fetch_string_literal(ctx, "%ld")?, llvm_ptr];

        let llvm_scanf_retval = llvm::core::LLVMBuildCall2(
            ctx.builder,
            scanf.llvm_type,
            scanf.llvm_value,
            scanf_params.as_mut_ptr(),
            scanf_params.len() as u32,
            ANON
        );

        let llvm_ge0_val = llvm::core::LLVMBuildICmp(
            ctx.builder,
            llvm::LLVMIntPredicate::LLVMIntSGE,
            llvm_scanf_retval,
            llvm::core::LLVMConstInt(ctx.types.i32, 0 as u64, 0),
            ANON
        );

        let llvm_cast = llvm::core::LLVMBuildIntCast2(
            ctx.builder, llvm_ge0_val,
            ctx.types.i64,
            1,
            ANON
        );

        llvm::core::LLVMBuildRet(ctx.builder, llvm_cast);

        Ok(TypedSymbol {
            llvm_type: llvm_readln_fn_type,
            llvm_value: llvm_readln_fn_value,
        })
    }
}

pub struct CallableContext {
    callable_name: String,
    callable: TypedSymbol,
    return_store: Option<TypedSymbol>,
    params: Vec<(String, *mut llvm::LLVMType)>,
}

#[derive(Clone)]
pub struct TypedSymbol {
    llvm_value: *mut llvm::LLVMValue,
    llvm_type: *mut llvm::LLVMType,
}

pub struct Scope<'a> {
    map: std::collections::HashMap<String, TypedSymbol>,
    parent: Option<&'a Scope<'a>>,
    callable_context: Option<CallableContext>,
    c_functions: &'a BuiltInFunctions,
}

impl<'a> Scope<'a> {
    pub fn new() -> Self {
        Self {
            map: std::collections::HashMap::new(),
            parent: None,
            callable_context: None,
            c_functions: &C_FUNCTIONS_DEFAULT,
        }
    }

    pub fn get(&self, name: &str) -> Option<TypedSymbol> {
        let mut curr = Some(self);
        while let Some(scope) = curr {
            if let Some(val) = scope.map.get(name) {
                return Some(val.clone());
            }
            curr = scope.parent;
        }
        return None;
    }

    pub fn set(&mut self, name: &str, val: TypedSymbol) -> Result<(), GenError> {
        if self.map.contains_key(name) {
            return Err(GenError::NameConflict);
        }
        self.map.insert(name.to_string(), val);
        return Ok(());
    }

    pub fn sub(&'a self) -> Self {
        let mut scope = Self::new();
        scope.parent = Some(self);
        scope.c_functions = self.c_functions;
        return scope;
    }
}

pub struct GenContext {
    llvm_ctx: *mut llvm::LLVMContext,
    module: *mut llvm::LLVMModule,
    builder: *mut llvm::LLVMBuilder,
    types: LlvmTypes,
    string_literals: std::collections::HashMap<String, *mut llvm::LLVMValue>,
}

impl GenContext {
    pub fn new() -> Self {
        unsafe {
            let llvm_ctx = llvm::core::LLVMContextCreate();
            let builder = llvm::core::LLVMCreateBuilderInContext(llvm_ctx);

            Self {
                llvm_ctx: llvm_ctx,
                builder: builder,
                module: std::ptr::null_mut(),
                types: LlvmTypes::new(llvm_ctx),
                string_literals: std::collections::HashMap::new(),
            }
        }
    }

    pub fn get_module(&self) -> Result<*mut llvm::LLVMModule, GenError> {
        if self.module.is_null() {
            return Err(GenError::MissingModule);
        }

        return Ok(self.module);
    }

    pub fn get_string(&self) -> Result<String, GenError> {
        unsafe {
            let cstr = llvm::core::LLVMPrintModuleToString(self.get_module()?);
            let rstring = std::ffi::CStr::from_ptr(cstr).to_str().map_err(|_| GenError::InvalidEncoding)?.to_owned();
            llvm::core::LLVMDisposeMessage(cstr);
            return Ok(rstring);
        }
    }
}

impl Drop for GenContext {
    fn drop(&mut self) {
        unsafe {
            llvm::core::LLVMDisposeBuilder(self.builder);
            llvm::core::LLVMDisposeModule(self.module);
            llvm::core::LLVMContextDispose(self.llvm_ctx);
        }
    }
}

pub trait CodeGen<T> {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<T, GenError>;
}

pub fn add_global(ctx: &mut GenContext, global_scope: &mut Scope, storage: &StorageDeclarationNode) -> Result<*mut llvm::LLVMValue, GenError> {
    let llvm_type = ctx.types.get_type(Some(&storage.dtype))?;

    let cstr = std::ffi::CString::new(
        storage.name.as_str()
    ).map_err(|_| GenError::InvalidName)?;

    unsafe {
        let llvm_value = llvm::core::LLVMAddGlobal(ctx.get_module()?, llvm_type, cstr.as_ptr());

        llvm::core::LLVMSetInitializer(
            llvm_value,
            if let Some(init) = &storage.init {
                init.gen(ctx, Some(global_scope))?
            } else {
                // if we don't set initializer, llc command will ignore it
                // TODO: more robust default value providers
                llvm::core::LLVMConstInt(llvm_type, 0, 0)
            }
        ); 

        global_scope.set(
            &storage.name,
            TypedSymbol {
                llvm_value: llvm_value,
                llvm_type: llvm_type
            }
        )?;

        return Ok(llvm_value);
    }
}

impl CodeGen<()> for ast::ProgramNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        if scope.is_some() {
            return Err(GenError::InvalidScope);
        }

        if ctx.get_module().is_ok() {
            // assert that module is not defined yet
            return Err(GenError::InvalidContext);
        }

        let name_cstr = std::ffi::CString::new(self.name.as_str()).map_err(|_| GenError::InvalidName)?;
        ctx.module = unsafe {
            llvm::core::LLVMModuleCreateWithNameInContext(name_cstr.as_ptr(), ctx.llvm_ctx)
        };

        let mut c_functions = BuiltInFunctions::new();
        c_functions.printf = Some(gen_printf(ctx)?);
        c_functions.scanf = Some(gen_scanf(ctx)?);
        c_functions.readln = Some(gen_readln(ctx, c_functions.scanf.as_ref().unwrap())?);

        let mut global_scope = Scope::new();
        global_scope.c_functions = &c_functions;

        for variable in self.declarations.variables.iter() {
            // TODO: arrays
            // TODO: default values?

            add_global(ctx, &mut global_scope, &variable)?;
        }

        for constant in self.declarations.constants.iter() {
            let llvm_value = add_global(ctx, &mut global_scope, &constant)?;
            unsafe {
                llvm::core::LLVMSetGlobalConstant(llvm_value, 1);
            }
        }
    
        for callable in self.declarations.callables.iter() {
            callable.gen(ctx, Some(&mut global_scope))?;
        }

        return Ok(());
    }
}

impl CodeGen<()> for ast::CallableDeclarationNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        let scope = scope.ok_or(GenError::InvalidScope)?;
        if scope.callable_context.is_some() {
            return Err(GenError::InvalidScope);
        }

        let my_symbol = if scope.get(&self.name).is_none() {
            // function with this name not declared yet, so we declare it

            let cstr = std::ffi::CString::new(self.name.clone()).map_err(|_| GenError::InvalidName)?;

            let return_type = ctx.types.get_type(self.return_type.as_ref())?;
            let mut param_types = self.params.iter().map(
                |(_pname, ptype)| ctx.types.get_type(Some(ptype))
            ).collect::<Result<Vec<*mut llvm::LLVMType>, GenError>>()?;

            unsafe {
                // TODO: consider not passing the name of the function to LLVM, so that it's anonymous
                // and we don't get conflicts with functions imported from C standard library

                let llvm_fn_type = llvm::core::LLVMFunctionType(return_type, param_types.as_mut_ptr(), param_types.len() as u32, 0);
                let llvm_fn_value = llvm::core::LLVMAddFunction(ctx.module, cstr.as_ptr(), llvm_fn_type);
                let my_symbol = TypedSymbol {
                    llvm_type: llvm_fn_type,
                    llvm_value: llvm_fn_value,
                };
                scope.set(&self.name, my_symbol.clone())?;
                my_symbol
            }
        } else {
            scope.get(&self.name).ok_or(GenError::UndefinedSymbol(self.name.clone()))?
        };

        if let Some(implementation) = self.implementation.as_ref() {
            // here we generate implementation for the function
            // TODO: check that implementation for this function hasn't been generated yet
            // TODO: check that the function signature matches

            let function = scope.get(&self.name).ok_or(
                GenError::UndefinedSymbol(self.name.clone())
            )?;

            let mut inner_scope = scope.sub();
            let params = self.params.iter().map(
                |(pname, ptype)| ctx.types.get_type(Some(ptype)).map(|llvm_type| (pname.to_owned(), llvm_type))
            ).collect::<Result<Vec<(String, *mut llvm::LLVMType)>, GenError>>()?;

            unsafe {
                let bb = llvm::core::LLVMAppendBasicBlockInContext(
                    ctx.llvm_ctx,
                    function.llvm_value,
                    ANON
                );

                llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, bb);

                // create callable context (e.g. register storage for return value and pass it to implementation)
                if let Some(ret_type) = self.return_type.as_ref() {
                    // when this is a function, allocate space for return value

                    let llvm_type = ctx.types.get_type(Some(&ret_type))?;
                    let llvm_value = llvm::core::LLVMBuildAlloca(
                        ctx.builder,
                        llvm_type,
                        ANON
                    );
                    
                    inner_scope.callable_context = Some(CallableContext {
                        callable_name: self.name.clone(),
                        callable: my_symbol,
                        return_store: Some(TypedSymbol {
                            llvm_value: llvm_value,
                            llvm_type: llvm_type,
                        }),
                        params: params,
                    });
                } else {
                    // when this is a procedure, let the return value be void
                    // TODO: test if this works

                    inner_scope.callable_context = Some(CallableContext {
                        callable_name: self.name.clone(),
                        callable: my_symbol,
                        return_store: None,
                        params: params,
                    });
                }

                // generate code
                implementation.gen(ctx, Some(&mut inner_scope))?;

                // virtual exit at the end of basic block
                llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, bb); // ensure we're still at the correct basic block
                ast::StatementNode::Exit.gen(ctx, Some(&mut inner_scope))?; // kind of hacky but whatever
            }
        }

        return Ok(());
    }
}

impl CodeGen<()> for ast::CallableImplementationNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        let scope = scope.ok_or(GenError::InvalidScope)?;

        unsafe {
            // allocate local variables
            for local in self.variables.iter() {
                // let cstr = std::ffi::CString::new(local.name.clone()).map_err(|_| GenError::InvalidName)?;
                let llvm_type = ctx.types.get_type(Some(&local.dtype))?;
                let llvm_value = llvm::core::LLVMBuildAlloca(ctx.builder, llvm_type, ANON);
                scope.set(
                    &local.name,
                    TypedSymbol {
                        llvm_type: llvm_type,
                        llvm_value: llvm_value,
                    }
                )?;
            }
        }

        // generate code
        self.implementation.gen(ctx, Some(scope))?;

        return Ok(());
    }
}

impl CodeGen<()> for ast::StatementNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        match self {
            ast::StatementNode::StatementBlock(node) => node.gen(ctx, scope),
            ast::StatementNode::Assignment(node) => node.gen(ctx, scope),
            ast::StatementNode::Expression(node) => node.gen(ctx, scope).map(|_| ()),
            ast::StatementNode::ForLoop(node) => node.gen(ctx, scope),
            ast::StatementNode::WhileLoop(node) => node.gen(ctx, scope),
            ast::StatementNode::IfStatement(node) => node.gen(ctx, scope),
            ast::StatementNode::Exit => {
                let scope = scope.ok_or(GenError::InvalidScope)?;
                let call_ctx = scope.callable_context.as_ref().ok_or(GenError::InvalidScope)?;
                unsafe {
                    if let Some(llvm_return_store) = call_ctx.return_store.clone() {
                        let llvm_return_value = llvm::core::LLVMBuildLoad2(
                            ctx.builder,
                            llvm_return_store.llvm_type, // this is the type of the value, not the pointer to it
                            llvm_return_store.llvm_value, // this is the pointer to it
                            ANON
                        );
                        llvm::core::LLVMBuildRet(ctx.builder, llvm_return_value);
                    } else {
                        llvm::core::LLVMBuildRetVoid(ctx.builder);
                    }
                }
                Ok(())
            }
        }
    }
}

impl CodeGen<()> for ast::StatementBlockNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        let mut scope = scope.ok_or(GenError::InvalidScope)?;

        for stmt in self.statements.iter() {
            stmt.gen(ctx, Some(&mut scope))?;
        }

        return Ok(());
    }
}

impl CodeGen<()> for ast::AssignmentNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        let mut scope = scope.ok_or(GenError::InvalidScope)?;

        match &self.target {
            ExpressionNode::Access(name) => {
                let lhs = self.value.gen(ctx, Some(&mut scope))?;
                let storage = scope.get(name).ok_or(GenError::UndefinedSymbol(name.clone()))?;
                unsafe {
                    llvm::core::LLVMBuildStore(ctx.builder, lhs, storage.llvm_value);
                }
                Ok(())
            },
            ExpressionNode::ArrayAccess(_) => todo!(),
            _ => Err(GenError::InvalidAssignment)
        }
    }
}

pub fn fetch_string_literal(ctx: &mut GenContext, string: &str) -> Result<*mut llvm::LLVMValue, GenError> {
    unsafe {
        if !ctx.string_literals.contains_key(string) {
            let cstr = std::ffi::CString::new(string.clone()).map_err(|_| GenError::InvalidEncoding)?;
            let llvm_string_literal = llvm::core::LLVMConstStringInContext(ctx.llvm_ctx, cstr.as_ptr(), string.len() as u32, 0);
            let llvm_string_type = llvm::core::LLVMTypeOf(llvm_string_literal);
            let storage = llvm::core::LLVMAddGlobal(ctx.module, llvm_string_type, ANON);
            llvm::core::LLVMSetInitializer(storage, llvm_string_literal);
            llvm::core::LLVMSetGlobalConstant(storage, 1);
            ctx.string_literals.insert(string.to_owned(), storage);
        }
        Ok(*ctx.string_literals.get(string).unwrap())
    }
}

impl CodeGen<*mut llvm::LLVMValue> for ast::LiteralNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<*mut llvm::LLVMValue, GenError> {
        unsafe {
            match self {
                ast::LiteralNode::Integer(integer) => {
                    Ok(llvm::core::LLVMConstInt(ctx.types.i64, *(integer as *const i64 as *const u64), 0))
                },
                ast::LiteralNode::String(string) => {
                    Ok(fetch_string_literal(ctx, string)?)
                },
            }
        }
    }
}

impl CodeGen<*mut llvm::LLVMValue> for ast::ExpressionNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<*mut llvm::LLVMValue, GenError> {
        match self {
            ast::ExpressionNode::Call(node) => Ok(node.gen(ctx, scope)?),
            ast::ExpressionNode::Literal(node) => Ok(node.gen(ctx, scope)?),
            ast::ExpressionNode::ArrayAccess(node) => todo!(),
            ast::ExpressionNode::BinaryOperator(node) => Ok(node.gen(ctx, scope)?),
            ast::ExpressionNode::Access(name) => unsafe {
                let scope = scope.ok_or(GenError::InvalidScope)?;

                // test if this is a function param:
                if let Some(callable_context) = scope.callable_context.as_ref() {
                    if let Some((pidx, (pname, ptype))) = callable_context.params.iter().enumerate().find(
                        |(pidx, (pname, ptype))| *pname == *name
                    ) {
                        let llvm_value = llvm::core::LLVMGetParam(callable_context.callable.llvm_value, pidx as u32);
                        return Ok(llvm_value);
                    }
                }

                let target = scope.get(&name).ok_or(GenError::UndefinedSymbol(name.clone()))?;
                let llvm_value = llvm::core::LLVMBuildLoad2(ctx.builder, target.llvm_type, target.llvm_value, ANON);
                Ok(llvm_value)
            },
        }
    }
}

fn gen_write_macro(pseudocall: &ast::CallNode, ctx: &mut GenContext, scope: &mut Scope, newline: bool) -> Result<*mut llvm::LLVMValue, GenError> {
    if pseudocall.params.len() != 1 {
        return Err(GenError::InvalidMacroUsage);
    }

    let printf = scope.c_functions.printf.clone().ok_or(GenError::MissingCFunction)?;

    if let ast::ExpressionNode::Literal(ast::LiteralNode::String(msg)) = &pseudocall.params[0] {
        // special case for string literal
        let mut msg = msg.clone();
        if newline {
            msg.push('\n');
        }
        unsafe {
            // let cstr = std::ffi::CString::new(msg).map_err(|_| GenError::InvalidEncoding)?;
            // let llvm_string_literal = llvm::core::LLVMConstStringInContext(ctx.llvm_ctx, cstr.as_ptr(), msglen as u32, 0);
            // let llvm_string_type = llvm::core::LLVMTypeOf(llvm_string_literal);
            // let mut storage = llvm::core::LLVMAddGlobal(ctx.module, llvm_string_type, ANON);
            // llvm::core::LLVMSetInitializer(storage, llvm_string_literal);
            // llvm::core::LLVMSetGlobalConstant(storage, 1);
            let mut storage = ast::LiteralNode::String(msg).gen(ctx, Some(scope))?;
            llvm::core::LLVMBuildCall2(
                ctx.builder,
                printf.llvm_type,
                printf.llvm_value,
                (&mut storage) as *mut *mut llvm::LLVMValue,
                1,
                ANON
            );

            return Ok(llvm::core::LLVMConstNull(ctx.types.void)); // return void
        }
    }

    let param = pseudocall.params[0].gen(ctx, Some(scope))?; // evaluate param
    unsafe {
        if llvm::core::LLVMTypeOf(param) != ctx.types.i64 {
            return Err(GenError::TypeMismatch);
        }

        let formatter = fetch_string_literal(ctx, if newline { "%ld\n" } else { "%ld" })?;

        let mut params = vec![formatter, param];

        llvm::core::LLVMBuildCall2(
            ctx.builder,
            printf.llvm_type,
            printf.llvm_value,
            params.as_mut_ptr() as *mut *mut llvm::LLVMValue,
            2,
            ANON
        );

        return Ok(llvm::core::LLVMConstNull(ctx.types.void)); // return void
    }
}

fn gen_readln_macro(pseudocall: &ast::CallNode, ctx: &mut GenContext, scope: &mut Scope) -> Result<*mut llvm::LLVMValue, GenError> {
    if pseudocall.params.len() != 1 {
        return Err(GenError::InvalidMacroUsage);
    }

    let param = &pseudocall.params[0];
    let mut llvm_ptr = match param {
        ExpressionNode::Access(name) => scope.get(&name).ok_or(GenError::UndefinedSymbol(name.clone()))?.llvm_value,
        ExpressionNode::ArrayAccess(_) => todo!(),
        _ => Err(GenError::InvalidMacroUsage)?,
    };

    let readln = scope.c_functions.readln.clone().ok_or(GenError::MissingCFunction)?;

    unsafe {
        Ok(
            llvm::core::LLVMBuildCall2(
                ctx.builder,
                readln.llvm_type,
                readln.llvm_value,
                &mut llvm_ptr as *mut *mut llvm::LLVMValue,
                1,
                ANON
            )
        )
    }
}

impl CodeGen<*mut llvm::LLVMValue> for ast::CallNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<*mut llvm::LLVMValue, GenError> {
        let scope = scope.ok_or(GenError::InvalidScope)?;

        // built-in macros:
        if let Some(val) = match self.callable_name.as_str() {
            "dec" => todo!(),
            "inc" => todo!(),
            "writeln" => Some(gen_write_macro(self, ctx, scope, true)),
            "write" => Some(gen_write_macro(self, ctx, scope, false)),
            "readln" => Some(gen_readln_macro(self, ctx, scope)),
            _ => None,
        } {
            return val;
        }

        let mut params = self.params.iter().map(
            |par| par.gen(ctx, Some(scope))
        ).collect::<Result<Vec<*mut llvm::LLVMValue>, GenError>>()?;

        let callable = scope.get(&self.callable_name).ok_or(GenError::UndefinedSymbol(self.callable_name.clone()))?;
        unsafe {
            // if not a macro, it's a normal function call.
            let retval = llvm::core::LLVMBuildCall2(
                ctx.builder,
                callable.llvm_type,
                callable.llvm_value,
                params.as_mut_ptr(),
                params.len() as u32,
                ANON
            );

            return Ok(retval);
        }   
    }
}

impl CodeGen<*mut llvm::LLVMValue> for ast::BinaryOperatorNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<*mut llvm::LLVMValue, GenError> {
        let mut scope = scope.ok_or(GenError::InvalidContext)?;

        let lhs = self.lhs.gen(ctx, Some(&mut scope))?;
        let rhs = self.rhs.gen(ctx, Some(&mut scope))?;

        unsafe {
            Ok(match self.kind {
                ast::BinaryOperatorKind::Add => llvm::core::LLVMBuildAdd(ctx.builder, lhs, rhs, ANON),
                ast::BinaryOperatorKind::Mul => llvm::core::LLVMBuildMul(ctx.builder, lhs, rhs, ANON),
                ast::BinaryOperatorKind::Sub => llvm::core::LLVMBuildSub(ctx.builder, lhs, rhs, ANON),
                ast::BinaryOperatorKind::Div => llvm::core::LLVMBuildSDiv(ctx.builder, lhs, rhs, ANON),
                ast::BinaryOperatorKind::Mod => llvm::core::LLVMBuildSRem(ctx.builder, lhs, rhs, ANON),
                ast::BinaryOperatorKind::And => llvm::core::LLVMBuildAnd(ctx.builder, lhs, rhs, ANON),
                ast::BinaryOperatorKind::Or => llvm::core::LLVMBuildOr(ctx.builder, lhs, rhs, ANON),
                ast::BinaryOperatorKind::Eq => llvm::core::LLVMBuildICmp(ctx.builder, llvm::LLVMIntPredicate::LLVMIntEQ, lhs, rhs, ANON),
                ast::BinaryOperatorKind::Ne => llvm::core::LLVMBuildICmp(ctx.builder, llvm::LLVMIntPredicate::LLVMIntNE, lhs, rhs, ANON),
                ast::BinaryOperatorKind::Lt => llvm::core::LLVMBuildICmp(ctx.builder, llvm::LLVMIntPredicate::LLVMIntSLT, lhs, rhs, ANON),
                ast::BinaryOperatorKind::Gt => llvm::core::LLVMBuildICmp(ctx.builder, llvm::LLVMIntPredicate::LLVMIntSGT, lhs, rhs, ANON),
                ast::BinaryOperatorKind::Le => llvm::core::LLVMBuildICmp(ctx.builder, llvm::LLVMIntPredicate::LLVMIntSLE, lhs, rhs, ANON),
                ast::BinaryOperatorKind::Ge => llvm::core::LLVMBuildICmp(ctx.builder, llvm::LLVMIntPredicate::LLVMIntSGE, lhs, rhs, ANON),
            })
        }
    }
}

impl CodeGen<()> for ast::ForLoopNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        todo!()
    }
}

impl CodeGen<()> for ast::WhileLoopNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        todo!()
    }
}

impl CodeGen<()> for ast::IfStatementNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        todo!()
    }
}
