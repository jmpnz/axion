# axion : a small, statically typed, compiled language

axion is a toy language hacked together for the sole purpose of learning
how compilers fit together. This isn't something I will work on in the
future but is here mostly as a reference to myself and others.

The `axion` compiler is written in Rust and includes a front end with with
a lexer and hand written recursive descent parser and a backend that includes
a type checker and code generator.

I haven't tested much of the generated code and there is no standard library
or runtime to do things like print stuff to stdout but the assembly can be
run through a debugger such as `gdb` or `blink`.

`axion` compiles code to a subset of x86-64 assembly and will need either
`gcc` or `clang` to actually get an executable file from the assembly.

If you ever feel like hacking on this here are some interesting things I wanted
to try but didn't end up doing :

- Write a pass to transform the AST into an IR (MIR/LLVM/BRIL...)
- Write a set of optimizations as nano passes.
- Write an emulator for the subset of x86 so you can test the output of codegen.
- Add support for arrays.

## Examples

I wouldn't say the language is designed at all, as it is pretty much a subset
of JavaScript with a couple primitive types, also I've left the arrays as a
future TODO if I ever get interested in finishing this.

Here is a code example that sums up most of the language.

```javascript

let global_variable : int = 4000;

function areWeDoneYet(x : int) -> bool {
    if (x == global_variable) {
        return true;
    }
    return false;
}

function until4000(x : int) -> int {
    while (!areWeDoneYet(x)) {
        if (x > 4000) {
            x = x - 1;
        } else {
            x = x + 1;
        }
    }
    return x;
}

// If a function has no return type it is assumed it returns "void".
// `main` denotes the entry point.
function main() {
    let x : int = 987;
    let y : int = until4000(x);
    return x + y;
}

```
