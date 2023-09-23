module Examples where

import Language.WASM.Prelude

-- Print the numbers from 1 to 10
countTo10 :: Module
countTo10 =
  main do
    const @Int 0
    let' #i do
      loop #next do
        local.get #i
        const 1
        add
        local.tee #i
        dup
        log
        const 10
        cmp.lt
        br_if #next

-- Allocate a segment to store the first n fibonacci numbers, and print it.
fibonacci :: Module
fibonacci = do
  global.var #n 10

  main do
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
          cmp.lt
          if #_ then do
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
            seg.log #fibs

-- Small example illustrating functions and globals.
functions :: Module
functions = do
  global.var #g 2

  fn @'[Int] #add_to_g do
    local.get #g
    add
    local.set #g

  fn @'[Int] #add_to_g_twice do
    dup
    call #add_to_g
    call #add_to_g

  fn @'[] #log_g do
    local.get #g
    log

  main do
    const 2
    call #add_to_g_twice
    call #log_g

-- Small example to illustrate recursive functions.
-- Function #f just prints its argument and keeps calling
-- itself with a smaller and smaller argument, until it reaches 0.
recursion :: Module
recursion = do
  fn @'[Int] #f do
    dup
    const 0
    cmp.gt

    if #_ then do
      dup
      log
      const 1
      sub
      call #f
    else
      drop

  main do
    const 10
    call #f

-- Squares all the elements in the host-provided memory segment.
squareAll :: IORef (Vector Int) -> Module
squareAll r = do
  global.seg_ref #s r

  main do
    seg.size #s
    let' #n do
      const 0
      let' #i do
        loop #l do
          local.get #i
          local.get #n
          cmp.lt
          if #_ then do
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
    seg.log #s

-- Calculate and print the factorial of n, using a recursive implementation.
factorial :: Int -> Module
factorial n = do
  global.var #n n

  fn @'[Int] #factorial do
    dup
    const 1
    cmp.gt
    if #_ then do
      dup
      const 1
      sub
      call #factorial
      mul
    else do
      drop
      const 1

  main do
    local.get #n
    call #factorial
    log

-- This programs traps with an out-of-bounds memory access.
outOfBounds :: Module
outOfBounds = main do
  const 10
  const ()
  let_seg #s do
    const 100
    seg.load #s
    log
