module Main(main) where

import Data.IORef(readIORef)
import GHC.IsList(IsList(..))
import Prelude
import System.IO(hFlush, stdout)
import System.IO.Silently(capture_, silence)
import Test.Tasty
import Test.Tasty.HUnit

import Language.Wasm.Instr
import Language.Wasm.Module
import Language.Wasm.Examples

testOutput :: TestName -> SomeMod -> String -> TestTree
testOutput modName mod expected =
  testCase modName $ capture_ (hFlush stdout *> runWasm mod *> hFlush stdout) >>= (@?= expected)

testHostSharedMemory :: Assertion
testHostSharedMemory = do
  silence $ runWasmWith (squareAll [1 .. 10]) do
    readIORef (segRef @"s") >>= (@?= [1,4,9,16,25,36,49,64,81,100])

tests :: TestTree
tests =
  testGroup "wasm-hs tests"
  [ testOutput "countTo10"       (someMod countTo10)       "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n"
  , testOutput "functions"       (someMod functions)       "6\n"
  , testOutput "recursion"       (someMod recursion)       "10\n9\n8\n7\n6\n5\n4\n3\n2\n1\n"
  , testOutput "outOfBounds"     (someMod outOfBounds)     "Execution trapped: Segment access out of bounds\n"
  , testOutput "divByZero"       (someMod divByZero)       "Execution trapped: Division by zero\n"
  , testOutput "fibonacci"       (someMod fibonacci)       "[1,2,3,5,8,13,21,34,55,89]\n"
  , testOutput "factorial"       (someMod $ factorial 10)  "3628800\n"
  , testOutput "mutualRecursion" (someMod mutualRecursion) "7\n6\n5\n4\n3\n2\n1\n"
  , testCase   "squareAll"       testHostSharedMemory
  ]

main :: IO ()
main = defaultMain tests
