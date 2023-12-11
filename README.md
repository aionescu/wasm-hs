# wasm-hs

This project provides an embedded domain-specific language (eDSL) that enables the embedding of a significant subset of WASM into Haskell, in a type-safe way (i.e. writing invalid WASM results in a Haskell type error).

Below is a simple example of the DSL, a WASM program that counts up to 10:

```haskell
countTo10 = main do
  const @Int 0
  let' #i do
    loop #next do
      local.get #i
      const 1
      add
      local.tee #i
      dup
      print
      const 10
      cmp.lt
      br_if #next
```

## Supported WASM features

* Arithmetic and comparison instructions
* Blocks and structured control-flow
* Local and global variables
* Type-safe dynamic memory access
* Recursive functions

## Ergonomics

In order to provide better ergonomics as a Haskell DSL, the project deviates from the WASM specification in a few aspects:

* The operand stack can contain values of any Haskell type, not just the WASM primitive types (`i32`, `i64`, `f32`, `f64`).
* Arithmetic and comparison instructions are overloaded using the standard Haskell typeclasses (`Num`, `Ord` etc.).
* Boolean instructions (comparisons, `br_if` etc.) use Haskell's native `Bool` type, rather than encoding booleans as `0` or `1` of type `i32`.
* Variables use alphanumeric names (rather than numeric indices), and are scoped explicitly (using a `let'` instruction), rather than being function-scoped.
* Similarly, block labels are also named, and are introduced by the block instructions (`block`, `loop`, `if`).
* Rather than using a single untyped linear memory, the user can explicitly allocate typed _memory segments_, inspired by the [MSWasm](https://dl.acm.org/doi/10.1145/3571208) paper. Segments are scoped similarly to local variables, and are allocated with the `let_seg` instruction.

## Interpretation

The project also includes an interpreter that uses continuation-passing style for efficient jumps, and local instances (via [`WithDict`](https://hackage.haskell.org/package/base/docs/GHC-Exts.html#t:WithDict)) for constant-time variable lookup.

Global variables and segments can be initialised with host-provided mutable references (`IORef`s), which allows the host to pass inputs to the WASM module, and inspect its outputs and side-effects.

## Limitations

* The DSL only allows the construction of self-contained WASM modules (i.e. no external imports or exports).
* Functions can only refer to functions defined before them in the same module (and to themselves), thus mutually-recursive functions are not supported.
* Indirect calls and `br_table` are not supported.

## Project Structure

The main modules of the library are [`Language.WASM.Instr`](src/Language/WASM/Instr.hs), which defines the core `Instr` AST datatype and evaluation functions; and [`Language.WASM.Module`](src/Language/WASM/Module.hs), which builds upon `Instr` and defines a datatype for bundling WASM definitions into modules, as well as module evaluation functions.

[`Language.WASM.Syntax`](src/Language/WASM/Syntax.hs) defines the DSL's syntactic sugar, and [`Language.WASM.Prelude`](src/Language/WASM/Prelude.hs) ties everything together into a single import.

The [`Examples`](app/Examples.hs) module defines a number of example WASM programs, and [`Main`](app/Main.hs) contains the driver code.

## Dependencies

* GHC >=9.8.1
* Cabal >=3.10.2.0

(Both can be installed via [GHCup](https://www.haskell.org/ghcup/))

## Building and running

The project has no external (non-Haskell) dependencies, and can simply be built using `cabal`:

```sh
cabal build
```

Running the project with no arguments will run all the WASM example programs:

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
