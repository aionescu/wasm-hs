# wasm-hs

Type-safe WASM eDSL in Haskell.

## Prerequisites

* GHC >=9.6
* Cabal >=3.10

## Examples

You can find some simple WASM programs written using the DSL in the [`Examples`](app/Examples.hs) module.

## Running the examples

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
