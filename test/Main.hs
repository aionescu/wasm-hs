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

testOutput :: TestName -> SomeModule -> String -> TestTree
testOutput modName mod expected =
  testCase modName $ capture_ (hFlush stdout *> runModule mod *> hFlush stdout) >>= (@?= expected)

testHostSharedMemory :: Assertion
testHostSharedMemory = do
  silence $ runModuleWith (squareAll [1 .. 10]) do
    readIORef (segRef @"s") >>= (@?= [1,4,9,16,25,36,49,64,81,100])

tests :: TestTree
tests =
  testGroup "wasm-hs tests"
  [ testOutput "countTo10"       (someModule countTo10)       "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n"
  , testOutput "functions"       (someModule functions)       "6\n"
  , testOutput "recursion"       (someModule recursion)       "10\n9\n8\n7\n6\n5\n4\n3\n2\n1\n"
  , testOutput "outOfBounds"     (someModule outOfBounds)     "Execution trapped: Segment access out of bounds\n"
  , testOutput "divByZero"       (someModule divByZero)       "Execution trapped: Division by zero\n"
  , testOutput "fibonacci"       (someModule fibonacci)       "[1,2,3,5,8,13,21,34,55,89]\n"
  , testOutput "factorial"       (someModule $ factorial 10)  "3628800\n"
  , testOutput "mutualRecursion" (someModule mutualRecursion) "7\n6\n5\n4\n3\n2\n1\n"
  , testCase   "squareAll"       testHostSharedMemory
  ]

main :: IO ()
main = defaultMain tests
