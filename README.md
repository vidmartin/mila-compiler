# BI-PJP: Semestral Work

This is my semestral work for the subject BI-PJP. It is a program that compiles the Mila language into LLVM IR. It is written in Rust and uses the LLVM C API through the [`llvm-sys`](https://crates.io/crates/llvm-sys) crate. It is compatible with LLVM 16.

The `clap` crate is used for parsing command line arguments.

## Building

I recommend compiling this with `cargo`, which can be done through the included Makefile. The Makefile runs `cargo build` and then copies the output to the `build` directory, from which it is used by the `mila` script.

When I started working on this project, I couldn't get it to compile. After a nontrivial investigation I found out, that the `llvm-sys` crate only works with static LLVM libraries, but the LLVM package on my Linux distribution only included dynamic LLVM libraries. So I had to compile LLVM manually and then pass the path to the result of this manual compilation to `cargo` via the `LLVM_SYS_160_PREFIX` environment variable.

## Implemented features

The following features of the Mila language were implemented:

- `write`, `writeln`, `readln` functions
- global variables and constants
- assignment
- expressions (arithmetic, logic, comparasion)
- `inc`, `dec` functions
- integer literals (decadic, octal, hexadecimal)
- string literals for `write` and `writeln`
- `if`/`else`
- `while`, `for`, `break`
- functions, procedures, local variables, `exit`
- parameters of functions & procedures
- nested blocks
- arrays (can be multidimensional!)
- direct & indirect recursion 

## Story and remarks

I first guessed the grammar for the Mila language and then converted it to (almost) LL(1). The resulting grammar is in the `grammar.txt` file. I then used [the tool](https://pages.fit.cvut.cz/peckato1/parsingtbl/) to generate a parsing table, which I hard-coded into my compiler.

I tried to separate code reasonably into files. This is the meaning of each file:
- `main.rs`: the entry point that used the tools provided by the other files to read standard input and compile it into something
- `tokens.rs`: the tokens that we recognize
- `lex.rs`: code for the lexer - an object that reads characters and outputs a stream of tokens
- `syn.rs`: code for the parser - takes a lexer, reads its output and transforms it into an AST
- `ast.rs`: the definition for the AST; it is done with Rust enums
- `gen.rs`: code that generates LLVM IR from AST nodes
- `ast_display.rs`: for printing of the AST
- `args.rs`: defines the CLI args that the compiler can be executed with

The parser was implemented using the recursive descent pattern with some tweaks. The grammar is not completely LL(1), so the parser has an extra stack to help it escape "dead ends". Whenever the parser wants to read a symbol, it first tries to pop it from this extra stack, and only when this stack is empty, it will read it from the lexer. This is useful for parsing stuff like

    var a: integer; b: integer; var c: integer
    
After parsing `var a: integer;`, the parser reads the next symbol and sees that it's an identifier. It is happy and parses `b: integer;` as another variable. However, then it reads the next symbol and sees the `var` keyword. It is not happy, because the current nonterminal doesn't understand that keyword. But this keyword was already removed from the lexer. So the parser pushes this keyword to the extra stack and returns the parsed declarations of variables `a` and `b`. `var c: integer` will then be parsed as a separate declaration in some shallower nonterminal.
