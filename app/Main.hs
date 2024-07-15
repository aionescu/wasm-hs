module Main where

import Data.Foldable(for_)
import GHC.IsList(IsList(..))
import Prelude
import System.Environment(getArgs)

import Language.Wasm.Module
import Language.Wasm.Examples

main :: IO ()
main = do
  args <- getArgs

  let
    examples =
      [ ("countTo10", someModule countTo10)
      , ("functions", someModule functions)
      , ("recursion", someModule recursion)
      , ("outOfBOunds", someModule outOfBounds)
      , ("divByZero", someModule divByZero)
      , ("fibonacci", someModule fibonacci)
      , ("factorial", someModule $ factorial 10)
      , ("squareAll", someModule $ squareAll [1 .. 10])
      , ("mutualRecursion", someModule mutualRecursion)
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
        runModule mod
        putStrLn ""
