# axion : a small, statically typed, compiled language

axion is a toy language hacked together for the sole purpose of learning
how compilers fit together.

The current `axion` compiler is written in Rust and includes a front end with
a lexer, hand written recursive descent parser with `panic!` error reporting and
a type checker that does most of the semantic analysis.

`axion` compiles code to x86 assembly  so it needs `gcc` or `clang` to bundle
the assembly to object file and link them to executable binaries.

## Examples

Here are some code examples for what axion looks like.

```javascript

const answer : int = 42;

function sumArray(arr: array[int], size: int) -> int {
    let sum: int = 0;
    let i:int = 0;
    for (i = 0;i < size;i++) {
        sum = sum + arr[i];
    }
    return sum;
}

```


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
