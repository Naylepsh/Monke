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
```

### Usage

Currently only ~REPL~ RPPL is available. To run it execute `sbt run`.

### Misc.

Interpretation is done with 3 distinct steps:
- Lexer
- Parser -- using Pratt Parsing
- [TODO] Evaluator -- using Tree-walking interpreter

