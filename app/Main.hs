module Main where

import Data.Foldable(for_)
import Data.IORef(newIORef)
import GHC.IsList(IsList(..))
import Prelude
import System.Environment(getArgs)

import Language.Wasm.Module
import Language.Wasm.Examples

main :: IO ()
main = do
  args <- getArgs
  r <- newIORef [1 .. 10]

  let
    examples =
      [ ("countTo10", countTo10)
      , ("functions", functions)
      , ("recursion", recursion)
      , ("outOfBOunds", outOfBounds)
      , ("divByZero", divByZero)
      , ("fibonacci", fibonacci)
      , ("factorial", factorial 10)
      , ("mutualRecursion", mutualRecursion)
      , ("squareAll", squareAll r)
      ]

    selected =
      case args of
        [] -> fst <$> examples -- If 'args' is empty, get the names of all 'examples'
        _ -> args

  for_ selected \name ->
    case lookup name examples of
      Nothing -> putStrLn $ "Unknown example: " <> name <> "\n"
      Just mod -> do
        putStrLn $ name <> ": "
        runWasm mod
        putStrLn ""
