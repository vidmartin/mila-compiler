
use llvm_sys as llvm;
use crate::ast;

pub enum GenError {
    MissingModule,
    InvalidContext,
    InvalidString,
    InvalidDataType,
}

pub struct LlvmTypes {
    i64: *mut llvm::LLVMType,
    i32: *mut llvm::LLVMType,
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

    pub fn set(&mut self, name: &str, val: *mut llvm::LLVMValue) {
        self.map.insert(name.to_string(), val);
    }

    pub fn sub(&'a self) -> Self {
        let mut scope = Self::new();
        scope.parent = Some(self);
        return scope;
    }
}

impl LlvmTypes {
    pub fn new() -> Self {
        unsafe {
            Self {
                i32: llvm_sys::core::LLVMInt32Type(),
                i64: llvm_sys::core::LLVMInt64Type(),
            }
        }
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
            match &variable.dtype {
                ast::DataType::One(dtype) => {
                    match dtype.as_str() {
                        "integer" => unsafe {
                            let cstr = std::ffi::CString::new(
                                variable.name.as_str()
                            ).map_err(|_| GenError::InvalidString)?;
                            let gvref = llvm::core::LLVMAddGlobal(
                                ctx.get_module()?,
                                ctx.types.i64,
                                cstr.as_ptr(),
                            );
                            global_scope.set(&variable.name, gvref);
                        },
                        _ => return Err(GenError::InvalidDataType),
                    }
                },
                ast::DataType::Array { item, from, to } => todo!(),
            }
        }

        todo!()
    }
}
