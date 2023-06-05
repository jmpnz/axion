# axion : a small, statically typed and compiled language
---

```javascript

const answer : int = 42;

function sumArray(arr: array[int], size: int) -> integer {
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

