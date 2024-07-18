module Main(main) where

import Data.IORef(newIORef, readIORef)
import GHC.IsList(IsList(..))
import Prelude
import System.IO(hFlush, stdout)
import System.IO.Silently(capture_, silence)
import Test.Tasty
import Test.Tasty.HUnit

import Language.Wasm.Module
import Language.Wasm.Examples

testOutput :: TestName -> WasmModule -> String -> TestTree
testOutput modName mod expected =
  testCase modName $ capture_ (hFlush stdout *> runWasm mod *> hFlush stdout) >>= (@?= expected)

testHostSharedMemory :: Assertion
testHostSharedMemory = do
  r <- newIORef [1 .. 10]
  silence $ runWasm $ squareAll r
  readIORef r >>= (@?= [1,4,9,16,25,36,49,64,81,100])

tests :: TestTree
tests =
  testGroup "wasm-hs tests"
  [ testOutput "countTo10"       countTo10       "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n"
  , testOutput "functions"       functions       "6\n"
  , testOutput "recursion"       recursion       "10\n9\n8\n7\n6\n5\n4\n3\n2\n1\n"
  , testOutput "outOfBounds"     outOfBounds     "Execution trapped: Memory access out of bounds\n"
  , testOutput "divByZero"       divByZero       "Execution trapped: Division by zero\n"
  , testOutput "fibonacci"       fibonacci       "[1,2,3,5,8,13,21,34,55,89]\n"
  , testOutput "factorial"       (factorial 10)  "3628800\n"
  , testOutput "mutualRecursion" mutualRecursion "7\n6\n5\n4\n3\n2\n1\n"
  , testCase   "squareAll"       testHostSharedMemory
  ]

main :: IO ()
main = defaultMain tests
