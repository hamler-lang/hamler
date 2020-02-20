
## Hamler Design

```
Hamler Source Code -> Hamler Compiler -> Core Erlang -> BEAM VM Bytecode
```

## The Compiler

Lexical Analysis
Parsing
Semantic Analysis
Optimization
Code Generation

```
Source Code --Scanning(Lexical Analyze)--> Tokens --Parsing--> Initial AST --Type Checking/Type Inference--> Final AST --Transforming--> Core Erlang
```

Three phases:

1. Lexical Analysis (also known as lexing): top-down, recursive-descent

Lexical analysis (also known as lexing or tokenization) breaks the source code text into a sequence of small pieces called lexical tokens.[41] This phase can be divided into two stages: the scanning, which segments the input text into syntactic units called lexemes and assign them a category; and the evaluating, which converts lexemes into a processed value. A token is a pair consisting of a token name and an optional token value.[42] Common token categories may include identifiers, keywords, separators, operators, literals and comments, although the set of token categories varies in different programming languages. The lexeme syntax is typically a regular language, so a finite state automaton constructed from a regular expression can be used to recognize it. The software doing lexical analysis is called a lexical analyzer. This may not be a separate step—it can be combined with the parsing step in scannerless parsing, in which case parsing is done at the character level, not the token level.

2. Syntax Analysis (also known as scanning or parsing):

Syntax analysis (also known as parsing) involves parsing the token sequence to identify the syntactic structure of the program. This phase typically builds a parse tree, which replaces the linear sequence of tokens with a tree structure built according to the rules of a formal grammar which define the language's syntax. The parse tree is often analyzed, augmented, and transformed by later phases in the compiler.

3. and Semantic Analysis:

Semantic analysis adds semantic information to the parse tree and builds the symbol table. This phase performs semantic checks such as type checking (checking for type errors), or object binding (associating variable and function references with their definitions), or definite assignment (requiring all local variables to be initialized before use), rejecting incorrect programs or issuing warnings. Semantic analysis usually requires a complete parse tree, meaning that this phase logically follows the parsing phase, and logically precedes the code generation phase, though it is often possible to fold multiple phases into one pass over the code in a compiler implementation.

## Implementation

- http://hackage.haskell.org/package/parsec

## Tools

### Config

[HOCON(Human-Optimized Config Object Notation)](https://github.com/lightbend/config/blob/master/HOCON.md) as the default syntax of config file.

### Rebar3 Plugin

### Interpreter

### Package Manager

### Vim & Emacs

### Intellij|VSCode Plugin

### Debug Tools

## The Languages we used

```
Golang Erlang Haskell Scala
         C, C++
```

- SML, Haskell, F#, OCaml
- Erlang, Elixir
- Rust, Scala
- C, C++, Go

## References

- https://github.com/happi/theBeamBook
- https://en.wikipedia.org/wiki/Recursive_descent_parser
- https://en.wikipedia.org/wiki/Parser_combinator
- https://en.wikipedia.org/wiki/Tail_recursive_parser
- http://eprints.nottingham.ac.uk/221/1/parsing.pdf
- https://wiki.haskell.org/Parsec
- https://en.wikipedia.org/wiki/Category_theory
- https://en.wikipedia.org/wiki/Backus–Naur_form
- https://en.wikipedia.org/wiki/Lambda_calculus

## Author

Feng Lee <feng at emqx.io>

