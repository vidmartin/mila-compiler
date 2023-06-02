
use llvm_sys as llvm;
use crate::ast::{self, LiteralNode, CallableDeclarationNode, CallableImplementationNode, StatementNode, StatementBlockNode, AssignmentNode, ExpressionNode, ForLoopNode, WhileLoopNode, IfStatementNode};

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
    InvalidEncoding
}

pub struct LlvmTypes {
    i64: *mut llvm::LLVMType,
    i32: *mut llvm::LLVMType,
    void: *mut llvm::LLVMType,
}

impl LlvmTypes {
    pub fn new() -> Self {
        unsafe {
            Self {
                i32: llvm_sys::core::LLVMInt32Type(),
                i64: llvm_sys::core::LLVMInt64Type(),
                void: llvm_sys::core::LLVMVoidType(),
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

pub struct CallableContext {
    return_store: Option<*mut llvm::LLVMValue>,
}

pub struct Scope<'a> {
    map: std::collections::HashMap<String, *mut llvm::LLVMValue>,
    parent: Option<&'a Scope<'a>>,
    callable_context: Option<CallableContext>,
}

impl<'a> Scope<'a> {
    pub fn new() -> Self {
        Self {
            map: std::collections::HashMap::new(),
            parent: None,
            callable_context: None,
        }
    }

    pub fn get(&self, name: &str) -> Option<*mut llvm::LLVMValue> {
        let mut curr = Some(self);
        while let Some(scope) = curr {
            if let Some(val) = scope.map.get(name) {
                return Some(*val);
            }
            curr = scope.parent;
        }
        return None;
    }

    pub fn set(&mut self, name: &str, val: *mut llvm::LLVMValue) -> Result<(), GenError> {
        if self.map.contains_key(name) {
            return Err(GenError::NameConflict);
        }
        self.map.insert(name.to_string(), val);
        return Ok(());
    }

    pub fn sub(&'a self) -> Self {
        let mut scope = Self::new();
        scope.parent = Some(self);
        return scope;
    }
}

pub struct GenContext {
    llvm_ctx: *mut llvm::LLVMContext,
    module: *mut llvm::LLVMModule,
    builder: *mut llvm::LLVMBuilder,
    types: LlvmTypes,
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
                types: LlvmTypes::new(),
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

pub trait CodeGen {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError>;
}

impl CodeGen for ast::ProgramNode {
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
            llvm::core::LLVMModuleCreateWithName(name_cstr.as_ptr())
        };

        let mut global_scope = Scope::new();

        for variable in self.declarations.variables.iter() {
            // TODO: arrays
            // TODO: default values?

            let llvm_type = ctx.types.get_type(Some(&variable.dtype))?;

            let cstr = std::ffi::CString::new(
                variable.name.as_str()
            ).map_err(|_| GenError::InvalidName)?;

            unsafe {
                let gvref = llvm::core::LLVMAddGlobal(ctx.get_module()?, llvm_type, cstr.as_ptr());

                global_scope.set(&variable.name, gvref)?;
            }
        }

        for constant in self.declarations.constants.iter() {
            match &constant.dtype {
                ast::DataType::One(dtype) => {
                    match dtype.as_str() {
                        "integer" => unsafe {
                            let value = match constant.init {
                                Some(LiteralNode::Integer(i)) => i,
                                _ => return Err(GenError::TypeMismatch),
                            };

                            let vref = llvm::core::LLVMConstInt(
                                ctx.types.get_type(Some(&constant.dtype))?,
                                *((&value) as *const i64 as *const u64),
                                0
                            );
                            
                            global_scope.set(&constant.name, vref)?;
                        },
                        _ => return Err(GenError::InvalidDataType),
                    }
                },
                ast::DataType::OneInternal(dtype) => {
                    return Err(GenError::InvalidDataType);
                },
                ast::DataType::Array { item, from, to } => todo!(),
            }
        }

    
        for callable in self.declarations.callables.iter() {
            callable.gen(ctx, Some(&mut global_scope))?;
        }

        return Ok(());
    }
}

impl CodeGen for CallableDeclarationNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        let scope = scope.ok_or(GenError::InvalidScope)?;
        if scope.callable_context.is_some() {
            return Err(GenError::InvalidScope);
        }

        if scope.get(&self.name).is_none() {
            // function with this name not declared yet, so we declare it

            let cstr = std::ffi::CString::new(self.name.clone()).map_err(|_| GenError::InvalidName)?;

            let return_type = ctx.types.get_type(self.return_type.as_ref())?;
            let mut param_types = self.param_types.iter().map(
                |ptype| ctx.types.get_type(Some(ptype))
            ).collect::<Result<Vec<*mut llvm::LLVMType>, GenError>>()?;

            unsafe {
                let fn_type = llvm::core::LLVMFunctionType(return_type, param_types.as_mut_ptr(), param_types.len() as u32, 0);
                let fn_ref = llvm::core::LLVMAddFunction(ctx.module, cstr.as_ptr(), fn_type);
                scope.set(&self.name, fn_ref)?;
            }
        }

        if let Some(implementation) = self.implementation.as_ref() {
            // here we generate implementation for the function
            // TODO: check that implementation for this function hasn't been generated yet
            // TODO: check that the function signature matches

            let function = scope.get(&self.name).ok_or(
                GenError::UndefinedSymbol(self.name.clone())
            )?;

            let mut inner_scope = scope.sub();

            unsafe {
                let bb = llvm::core::LLVMAppendBasicBlockInContext(
                    ctx.llvm_ctx,
                    function,
                    b"\0".as_ptr() as *const i8
                );

                llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, bb);

                // create callable context (e.g. register storage for return value and pass it to implementation)
                if let Some(ret_type) = self.return_type.as_ref() {
                    // when this is a funcion, allocate space for return value

                    let vref = llvm::core::LLVMBuildAlloca(
                        ctx.builder,
                        ctx.types.get_type(Some(&ret_type))?,
                        b"0\0".as_ptr() as *const i8
                    );
                    
                    inner_scope.callable_context = Some(CallableContext {
                        return_store: Some(vref)
                    });
                } else {
                    // when this is a function, let the return value be void
                    // TODO: test if this works

                    inner_scope.callable_context = Some(CallableContext {
                        return_store: None
                    });
                }
                
                // generate code
                implementation.gen(ctx, Some(&mut inner_scope))?;
            }
        }

        return Ok(());
    }
}

impl CodeGen for CallableImplementationNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        let scope = scope.ok_or(GenError::InvalidScope)?;

        unsafe {
            // allocate local variables
            for local in self.variables.iter() {
                let cstr = std::ffi::CString::new(local.name.clone()).map_err(|_| GenError::InvalidName)?;
                let dtype = ctx.types.get_type(Some(&local.dtype))?;

                let vref = llvm::core::LLVMBuildAlloca(ctx.builder, dtype, cstr.as_ptr());
                scope.set(&local.name, vref)?;
            }
        }

        // generate code
        self.implementation.gen(ctx, Some(scope))?;

        return Ok(());
    }
}

impl CodeGen for StatementNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        match self {
            StatementNode::StatementBlock(node) => node.gen(ctx, scope),
            StatementNode::Assignment(node) => node.gen(ctx, scope),
            StatementNode::Expression(node) => node.gen(ctx, scope),
            StatementNode::ForLoop(node) => node.gen(ctx, scope),
            StatementNode::WhileLoop(node) => node.gen(ctx, scope),
            StatementNode::IfStatement(node) => node.gen(ctx, scope),
            StatementNode::Exit => {
                let scope = scope.ok_or(GenError::InvalidScope)?;
                let call_ctx = scope.callable_context.as_ref().ok_or(GenError::InvalidScope)?;
                unsafe {
                    if let Some(retval) = call_ctx.return_store {
                        llvm::core::LLVMBuildRet(ctx.builder, retval);
                    } else {
                        llvm::core::LLVMBuildRetVoid(ctx.builder);
                    }
                }
                Ok(())
            }
        }
    }
}

impl CodeGen for StatementBlockNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        let mut scope = scope.ok_or(GenError::InvalidScope)?;

        for stmt in self.statements.iter() {
            stmt.gen(ctx, Some(&mut scope))?;
        }

        return Ok(());
    }
}

impl CodeGen for AssignmentNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        todo!()
    }
}

impl CodeGen for ExpressionNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        todo!()
    }
}

impl CodeGen for ForLoopNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        todo!()
    }
}

impl CodeGen for WhileLoopNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        todo!()
    }
}

impl CodeGen for IfStatementNode {
    fn gen(&self, ctx: &mut GenContext, scope: Option<&mut Scope>) -> Result<(), GenError> {
        todo!()
    }
}
