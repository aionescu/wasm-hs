<!-- markdownlint-disable first-line-heading -->

<div align="center">
  <h1>wasm-hs</h1>

  <img src="assets/logo.png" alt="wasm-hs Logo" width="150" height="150"/>
</div>

This project provides an embedded domain-specific language (eDSL) for encoding WebAssembly in Haskell in a type-safe way (i.e. writing invalid Wasm results in a Haskell type error).

Below is a simple example of the DSL, a Wasm program that computes the factorial of 5:

```haskell
factorial :: WasmModule
factorial = wasm do
  fn #factorial @'[Int] do
    dup
    const 1

    if gt then do
      dup
      const 1
      sub
      call #factorial
      mul
    else do
      drop
      const 1

  fn #main do
    const 5
    call #factorial
    print
```

## Supported features

* Arithmetic and comparison instructions
* Blocks and structured control-flow
* Local and global variables
* Type-safe dynamic memory access
* Functions, including mutual recursion

## Ergonomics

To provide better ergonomics as a Haskell DSL, the project deviates from the WebAssembly specification in a few aspects:

* The operand stack can contain values of any Haskell type, not just the Wasm primitive types (`i32`, `i64`, `f32`, `f64`).
* Additional instructions are supported (`dup`, `swap`) that are not present in Wasm.
* Arithmetic and comparison instructions are overloaded using the standard Haskell typeclasses (`Num`, `Ord` etc.).
* Boolean instructions (comparisons, `br_if` etc.) use Haskell's native `Bool` type, rather than encoding booleans as `0` or `1` of type `i32`.
* Local variables are scoped explicitly (using a `let'` instruction), instead of being function-scoped.
* Memories are typed, and are scoped similarly to local variables (allocated with the `let_mem` instruction).

## Interpreter

The project also includes an interpreter that uses continuation-passing style for efficient jumps, and local instances (via [`WithDict`](https://hackage.haskell.org/package/base/docs/GHC-Exts.html#t:WithDict)) for constant-time variable lookup.

Global variables and memories can be initialised with host-provided mutable references (`IORef`s), which allows the host to pass input data to the Wasm module, and inspect its mutations after execution.

## Limitations

* The DSL only allows the construction of self-contained Wasm modules (i.e. no external imports or exports).
* `call_indirect` and `br_table` are not supported.

## Project structure

The main modules of the library are [`Language.Wasm.Instr`](src/Language/Wasm/Instr.hs), which defines the core `Instr` AST datatype and evaluation functions; and [`Language.Wasm.Module`](src/Language/Wasm/Module.hs), which builds upon `Instr` and defines a datatype for bundling definitions into modules, as well as module evaluation functions.

[`Language.Wasm.Syntax`](src/Language/Wasm/Syntax.hs) defines the DSL's syntactic sugar, and [`Language.Wasm.Prelude`](src/Language/Wasm/Prelude.hs) ties everything together into a single import.

The [`Language.Wasm.Examples`](src/Language/Wasm/Examples.hs) module defines a number of example Wasm programs, and [`Main`](app/Main.hs) contains the driver code.

## Dependencies

* GHC >=9.8.1
* Cabal >=3.10.2.0

(Both can be installed via [GHCup](https://www.haskell.org/ghcup/))

## Building and running

The project has no external (non-Haskell) dependencies, and can simply be built using `cabal`:

```sh
cabal build
```

Running the project with no arguments will run all the example programs:

```sh
cabal run
```

If you want to run only specific examples, pass their names as arguments:

```sh
cabal run . -- factorial fibonacci
```

If you specify an example multiple times, it will be executed that many times. This can be used to observe side-effects such as mutating memory shared by the host:

```sh
cabal run . -- squareAll squareAll
```

## Running the tests

To run the library's test-suite, use the following command:

```sh
cabal run wasm-hs-test
```
