
mod tokens;
mod lex;
mod ast;
mod ast_display;
mod syn;
mod gen;
mod args;

use std::io::{stdin, Read};
use clap::Parser;

use llvm_sys;

use crate::gen::CodeGen;

fn llvm_test() {
    unsafe {
        println!("LLVMContextCreate()");
        let context = llvm_sys::core::LLVMContextCreate();
        println!("LLVMModuleCreateWithName(...)");
        let module = llvm_sys::core::LLVMModuleCreateWithName(b"TestModule\0".as_ptr() as *const i8);
        println!("LLVMCreateBuilderInContext(...)");
        let builder = llvm_sys::core::LLVMCreateBuilderInContext(context);

        println!("LLVMInt32Type()");
        let i32t = llvm_sys::core::LLVMInt32Type();
        
        println!("LLVMFunctionType(...)");
        let fntype_i32 = llvm_sys::core::LLVMFunctionType(
            i32t, std::ptr::null_mut(), 0, 0
        );

        println!("LLVMAddFunction(...)");
        let fn_main = llvm_sys::core::LLVMAddFunction(
            module,
            b"main\0".as_ptr() as *const i8,
            fntype_i32
        );

        println!("LLVMAppendBasicBlockInContext(...)");
        let fn_main_bb1 = llvm_sys::core::LLVMAppendBasicBlockInContext(context, fn_main, b"\0".as_ptr() as *const i8);
        println!("LLVMPositionBuilderAtEnd(...)");
        llvm_sys::core::LLVMPositionBuilderAtEnd(builder, fn_main_bb1);
        println!("LLVMConstInt(...)");
        let i32v1234 = llvm_sys::core::LLVMConstInt(i32t, 12, 0);
        println!("LLVMBuildRet(...)");
        let iret = llvm_sys::core::LLVMBuildRet(
            builder, i32v1234
        );

        println!("LLVMDumpModule(...)");
        let mut errmsg: *mut std::ffi::c_char = std::ptr::null_mut();
        if llvm_sys::core::LLVMPrintModuleToFile(module, b"./test.ll\0".as_ptr() as *const i8, &mut errmsg as *mut *mut std::ffi::c_char) == 0 {
            println!(" -> output written successfully :)");
        } else {
            println!(" -> error happened! {}", std::ffi::CStr::from_ptr(errmsg).to_str().unwrap());
        }

        llvm_sys::core::LLVMDisposeBuilder(builder);
        llvm_sys::core::LLVMDisposeModule(module);
        llvm_sys::core::LLVMContextDispose(context);
    }
}

fn main() {
    let args = args::Args::parse();

    // read input (stdin)
    let mut s = String::new();
    stdin().read_to_string(&mut s).unwrap();
    if s.chars().last().unwrap() != '\n' {
        s.push('\n');
    }

    match args.output_mode {
        args::OutputMode::Lex => {
            // print output from lexer

            let mut lexer = lex::Lexer::new(s.as_str().chars());
            while let Some(tok) = lexer.next() {
                println!("{}", tok);
            }
            if let Some(err) = lexer.get_error() {
                panic!("lex error! {}", err);
            }
        },
        args::OutputMode::Ast | args::OutputMode::Gen => {
            let mut lexer = lex::Lexer::new(s.as_str().chars()).peekable();
            let mut parser = syn::Parser::new(&mut lexer);

            let program_ast = match parser.parse_program() {
                Ok(ast) => ast,
                Err(err) => panic!("lex or parse error! {:?}", err),
            };

            if let args::OutputMode::Ast = args.output_mode {
                // print output from parser
                println!("{}", ast_display::indent(format!("{}", program_ast), 4, false));
            } else {
                // generate LLVM IR
                let mut context = gen::GenContext::new();

                match program_ast.gen(&mut context, None).and_then(|_| context.get_string()) {
                    Ok(llvm_ir) => println!("{}", llvm_ir),
                    Err(err) => panic!("gen error! {:?}", err),
                }
            }
        },
    }
}
