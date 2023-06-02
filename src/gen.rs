
use llvm_sys as llvm;
use crate::ast::{self, LiteralNode};

pub enum GenError {
    MissingModule,
    InvalidContext,
    InvalidString,
    InvalidDataType,
    NameConflict,
    TypeMismatch,
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

pub struct Scope<'a> {
    map: std::collections::HashMap<String, *mut llvm::LLVMValue>,
    parent: Option<&'a Scope<'a>>,
}

impl<'a> Scope<'a> {
    pub fn new() -> Self {
        Self {
            map: std::collections::HashMap::new(),
            parent: None,
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
    fn gen(&self, ctx: &mut GenContext) -> Result<(), GenError>;
}

impl CodeGen for ast::ASTNode {
    fn gen(&self, ctx: &mut GenContext) -> Result<(), GenError> {
        match self {
            ast::ASTNode::Program(program) => program.gen(ctx),
        }
    }
}

impl CodeGen for ast::ProgramNode {
    fn gen(&self, ctx: &mut GenContext) -> Result<(), GenError> {
        if ctx.get_module().is_ok() {
            // assert that module is not defined yet
            return Err(GenError::InvalidContext);
        }

        let name_cstr = std::ffi::CString::new(self.name.as_str()).map_err(|_| GenError::InvalidString)?;
        ctx.module = unsafe {
            llvm::core::LLVMModuleCreateWithName(name_cstr.as_ptr())
        };

        let mut global_scope = Scope::new();

        for variable in self.declarations.variables.iter() {
            // TODO: arrays

            let llvm_type = ctx.types.get_type(Some(&variable.dtype))?;

            let cstr = std::ffi::CString::new(
                variable.name.as_str()
            ).map_err(|_| GenError::InvalidString)?;

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
            // TODO: forward refs

            let cstr = std::ffi::CString::new(
                callable.name.clone()
            ).map_err(|_| GenError::InvalidString)?;

            let return_type = ctx.types.get_type(callable.return_type.as_ref())?;
            let mut param_types = callable.param_types.iter().map(
                |ptype| ctx.types.get_type(Some(ptype))
            ).collect::<Result<Vec<*mut llvm::LLVMType>, GenError>>()?;

            unsafe {
                let fn_type = llvm::core::LLVMFunctionType(return_type, param_types.as_mut_ptr(), param_types.len() as u32, 0);
                let fn_ref = llvm::core::LLVMAddFunction(ctx.module, cstr.as_ptr(), fn_type);
                global_scope.set(&callable.name, fn_ref)?;
            }

            // llvm::core::LLVMBuildAlloca(arg1, Ty, Name)
        }

        todo!()
    }
}
