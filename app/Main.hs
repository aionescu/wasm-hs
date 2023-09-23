module Main where

import Data.Foldable(for_)
import Data.IORef(newIORef)
import Data.Vector qualified as V
import Prelude
import System.Environment(getArgs)

import Language.WASM.Module(evalModule)
import Examples

main :: IO ()
main = do
  args <- getArgs
  r <- newIORef $ V.fromList [1 .. 10]

  let
    examples =
      [ ("countTo10", countTo10)
      , ("fibonacci", fibonacci)
      , ("functions", functions)
      , ("recursion", recursion)
      , ("squareAll", squareAll r)
      , ("factorial", factorial 10)
      ]

    selected =
      case args of
        [] -> examples
        _ -> filter (\(name, _) -> name `elem` args) examples

  for_ selected \(name, mod) -> do
    putStrLn $ name <> ": "
    evalModule mod
    putStrLn ""
