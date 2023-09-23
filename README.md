# wasm-hs

This project provides an eDSL that enables the embedding of a subset of WASM into Haskell, in a type-safe way.

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

To achieve a near-native look and feel, the DSL relies on the following GHC language extensions:

* `RebindableSyntax` for overloaded `do` notation and `if` expressions.
* `OverloadedRecordDot` for "sub-instructions" such as `local.get` and `cmp.gt`.
* `OverloadedLabels` for variable, label, and segment names (e.g. `local.get #a`, `seg.store #s`).
* `NoImplicitPrelude` and a custom `Language.WASM.Prelude` module for easy importing.

## Interpretation

The project also includes an interpreter that uses continuation-passing style for efficient jumps, and local instances (via [`WithDict`](https://hackage.haskell.org/package/base/docs/GHC-Exts.html#t:WithDict)) for constant-time variable lookup.

Global variables and segments can be initialised with host-provided mutable references (`IORef`s), which allows the host to pass inputs to the WASM module, and inspect its outputs and side-effects.

## Limitations

* The DSL only allows the construction of self-contained WASM modules (i.e. no external imports or exports).
* Functions can only refer to functions defined before them in the same module (and to themselves), thus mutually-recursive functions are not supported.
* Indirect calls and `br_table` are not supported.

## Examples

Below is a simple WASM program that counts up to 10, written using the DSL:

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

More examples can be found in the [`Examples`](app/Examples.hs) module.

## Dependencies

* GHC >=9.6
* Cabal >=3.10

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
