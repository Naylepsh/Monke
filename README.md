## Monke 

Monke is a scala implementation of an interpreter for `Monkey` language based on "Writing an interpreted in GO" book by Thorsten Ball.

### Feature list

#### Variables

```
let a = 42;
let b = true;
```

#### If expressions

```
let a = if (b == c) {
    42;
} else {
    0;
}
```

#### Functions

```
let add = fn(a, b) {
    return a + b;
}
let result = add(1, 2);

let add2 = fn(a) { fn(b) { a + b }}
let result2 = add2(1)(2)

let factorial = fn(n) { if (n == 0) { 1 } else { n * fact(n - 1) } }

let apply = fn(f) { fn(x) { f(x) } }
let result3 = apply(fn(x) { x })(42)
```

### REPL

Execute `sbt run`.

### Misc.

Interpretation is done with 3 distinct steps:
- Lexer
- Parser -- using Pratt Parsing
- Evaluator -- using Tree-walking interpreter (meaning it's VERY slow)

