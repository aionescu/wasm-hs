module Language.Wasm.Examples where

import Language.Wasm.Prelude

-- Print the numbers from 1 to 10
countTo10 :: WasmModule
countTo10 = wasm do
  fn #main do
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
        lt
        br_if #next

-- Small example illustrating functions and globals.
functions :: WasmModule
functions = wasm do
  global #g 2

  fn #add_to_g @'[Int] do
    local.get #g
    add
    local.set #g

  fn #add_to_g_twice @'[Int] do
    dup
    call #add_to_g
    call #add_to_g

  fn #print_g @'[] do
    local.get #g
    print

  fn #main do
    const 2
    call #add_to_g_twice
    call #print_g

-- Small example to illustrate recursive functions.
-- Function #f just prints its argument and keeps calling
-- itself with a smaller and smaller argument, until it reaches 0.
recursion :: WasmModule
recursion = wasm do
  fn #f @'[Int] do
    dup
    const 0

    if gt then do
      dup
      print
      const 1
      sub
      call #f
    else
      drop

  fn #main do
    const 10
    call #f

-- This programs traps with an out-of-bounds memory access.
outOfBounds :: WasmModule
outOfBounds = wasm do
  fn #main do
    const 10
    const ()
    let_mem #m do
      const 100
      mem.load #m
      print

-- This program traps with a division by zero.
divByZero :: WasmModule
divByZero = wasm do
  fn #main do
    const @Int 1
    const 0
    div
    print

-- Allocate a memory to store the first n fibonacci numbers, and print it.
fibonacci :: WasmModule
fibonacci = wasm do
  global #n 10

  fn #main do
    local.get #n
    const @Int 0
    let_mem #fibs do
      const 0
      const 1
      mem.store #fibs

      const 1
      const 2
      mem.store #fibs

      const 2
      let' #i do
        loop #l do
          local.get #i
          local.get #n

          if lt then do
            local.get #i
            const 2
            sub
            mem.load #fibs

            local.get #i
            const 1
            sub
            mem.load #fibs

            add
            local.get #i
            swap
            mem.store #fibs

            local.get #i
            const 1
            add
            local.set #i
            br #l
          else do
            mem.print #fibs

-- Calculate and print the factorial of n, using a recursive implementation.
factorial :: Int -> WasmModule
factorial n = wasm do
  global #n n

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
    local.get #n
    call #factorial
    print

mutualRecursion :: WasmModule
mutualRecursion = wasm do
  fn #f @'[Int] do
    dup
    const 0

    if eq then
      drop
    else do
      dup
      print
      const 1
      sub
      call #g

  fn #g @'[Int] do
    dup
    const 0

    if eq then
      drop
    else do
      dup
      print
      const 1
      sub
      call #f

  fn #main do
    local.get #x
    call #g

  global #x 7

-- Squares all the elements in the host-provided memory buffer.
squareAll :: IORef (Vector Int) -> WasmModule
squareAll r = wasm do
  host_mem #m r do
    fn #main do
      mem.size #m

      let' #n do
        const 0
        let' #i do
          loop #l do
            local.get #i
            local.get #n

            if lt then do
              local.get #i
              dup
              mem.load #m
              dup
              mul
              mem.store #m

              local.get #i
              const 1
              add
              local.set #i
              br #l
            else do
              nop

      mem.print #m
