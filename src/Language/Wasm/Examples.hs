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

  fn @'[Int] #add_to_g do
    local.get #g
    add
    local.set #g

  fn @'[Int] #add_to_g_twice do
    dup
    call #add_to_g
    call #add_to_g

  fn @'[] #print_g do
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
  fn @'[Int] #f do
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
    let_seg #s do
      const 100
      seg.load #s
      print

-- This program traps with a division by zero.
divByZero :: WasmModule
divByZero = wasm do
  fn #main do
    const @Int 1
    const 0
    i.div
    print

-- Allocate a segment to store the first n fibonacci numbers, and print it.
fibonacci :: WasmModule
fibonacci = wasm do
  global #n 10

  fn #main do
    local.get #n
    const @Int 0
    let_seg #fibs do
      const 0
      const 1
      seg.store #fibs

      const 1
      const 2
      seg.store #fibs

      const 2
      let' #i do
        loop #l do
          local.get #i
          local.get #n

          if lt then do
            local.get #i
            const 2
            sub
            seg.load #fibs

            local.get #i
            const 1
            sub
            seg.load #fibs

            add
            local.get #i
            swap
            seg.store #fibs

            local.get #i
            const 1
            add
            local.set #i
            br #l
          else do
            seg.print #fibs

-- Calculate and print the factorial of n, using a recursive implementation.
factorial :: Int -> WasmModule
factorial n = wasm do
  global #n n

  fn @'[Int] #factorial do
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
  fn @'[Int] #f do
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

  fn @'[Int] #g do
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

-- Squares all the elements in the host-provided memory segment.
squareAll :: IORef (Vector Int) -> WasmModule
squareAll r = wasm do
  host_global_seg #s r do
    fn #main do
      seg.size #s

      let' #n do
        const 0
        let' #i do
          loop #l do
            local.get #i
            local.get #n

            if lt then do
              local.get #i
              dup
              seg.load #s
              dup
              mul
              seg.store #s

              local.get #i
              const 1
              add
              local.set #i
              br #l
            else do
              nop

      seg.print #s
