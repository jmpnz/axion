# axion : a small, statically typed and compiled language

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

The current `axion` compiler is written in Rust and includes a front end with
a lexer, hand written recursive descent parser with basic error reporting and
a type checker that does most of the semantic analysis.

The middleware translates the AST representation into an IR and runs various
optimiziation passes. Currently `axion` has one backend and compiles to only
a single target architecture (x86-64). The rest of work is dispatched to your
linker.

Here are some examples of code in `axion`.

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
function @main() {
    // Maybe do type inference.
    let x : int = 987;
    let y : int = until4000(x);
    // Print is a native statement that writes to stdout
    print(x);
}

```
