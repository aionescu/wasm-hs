module Language.Wasm.Examples where

import Language.Wasm.Prelude

-- Print the numbers from 1 to 10
countTo10 :: Mod '[Fn "main" '[] '[]]
countTo10 = do
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
functions :: Mod '[Var "g" Int, Fn "add_to_g" '[Int] '[], Fn "add_to_g_twice" '[Int] '[], Fn "print_g" '[] '[], Fn "main" '[] '[]]
functions = do
  let_global #g 2

  fn #add_to_g do
    local.get #g
    add
    local.set #g

  fn #add_to_g_twice do
    dup
    call #add_to_g
    call #add_to_g

  fn #print_g do
    local.get #g
    print

  fn #main do
    const 2
    call #add_to_g_twice
    call #print_g

-- Small example to illustrate recursive functions.
-- Function #f just prints its argument and keeps calling
-- itself with a smaller and smaller argument, until it reaches 0.
recursion :: Mod '[Fn "f" '[Int] '[], Fn "main" '[] '[]]
recursion = do
  fn #f do
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
outOfBounds :: Mod '[Fn "main" '[] '[]]
outOfBounds = do
  fn #main do
    const 10
    const ()
    let_seg #s do
      const 100
      seg.load #s
      print

-- This program traps with a division by zero.
divByZero :: Mod '[Fn "main" '[] '[]]
divByZero = do
  fn #main do
    const @Int 1
    const 0
    i.div
    print

-- Allocate a segment to store the first n fibonacci numbers, and print it.
fibonacci :: Mod '[Var "n" Int, Fn "main" '[] '[]]
fibonacci = do
  let_global #n 10

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
factorial :: Int -> Mod '[Var "n" Int, Fn "factorial" '[Int] '[Int], Fn "main" '[] '[]]
factorial n = do
  let_global #n n

  fn #factorial do
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

-- Squares all the elements in the host-provided memory segment.
squareAll :: Vector Int -> Mod '[Seg "s" Int, Fn "main" '[] '[]]
squareAll v = do
  let_global_seg #s v

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

mutualRecursion :: Mod '[Fn "f" '[Int] '[], Fn "g" '[Int] '[], Var "x" Int, Fn "main" '[] '[]]
mutualRecursion = do
  fn #f do
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

  fn #g do
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

  let_global #x 7
