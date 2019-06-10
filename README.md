# Abstract

Functional, Haskell-like language interpreter (written in Haskell).

## Getting started

```
ghc Interpreter.hs -o interpreter; cat examples/good/good1.in | ./interpreter
```

## Features

* algebraic, polimorphic data types
* Hindley-Milner types reconstruction
* pattern matching
* strict expression evaluation
* modules importing
* recursive let and functions
* set of builtins (Just, List)
* list input and output desugaring ([1,2,3])
* static parsing (i.a main finding, typecheck)
* monadic environment and errors handling

## Examples

There is exhaustive bunch of examples of proper and bad use cases, all placed in
```
/examples/good/*
```

```
/examples/bad/*
```
