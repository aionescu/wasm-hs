module Main where

import Data.Foldable(for_)
import GHC.IsList(IsList(..))
import Prelude
import System.Environment(getArgs)

import Language.Wasm.Module(runWasm, someMod)
import Language.Wasm.Examples

main :: IO ()
main = do
  args <- getArgs

  let
    examples =
      [ ("countTo10", someMod countTo10)
      , ("functions", someMod functions)
      , ("recursion", someMod recursion)
      , ("outOfBOunds", someMod outOfBounds)
      , ("divByZero", someMod divByZero)
      , ("fibonacci", someMod fibonacci)
      , ("factorial", someMod $ factorial 10)
      , ("squareAll", someMod $ squareAll [1 .. 10])
      , ("mutualRecursion", someMod mutualRecursion)
      ]

    selected =
      case args of
        [] -> fst <$> examples
        _ -> args

  for_ selected \name ->
    case lookup name examples of
      Nothing -> putStrLn $ "Unknown example: " <> name
      Just mod -> do
        putStrLn $ name <> ": "
        runWasm mod
        putStrLn ""
