module Main where

import Control.Monad((<=<))
import Data.Foldable(traverse_)
import Data.IORef(newIORef)
import Data.Maybe(mapMaybe)
import Data.Vector qualified as V
import Prelude
import System.Environment(getArgs)

import Language.WASM.Module
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
        [] -> snd <$> examples
        _ -> mapMaybe (`lookup` examples) args

  traverse_ (print <=< evalModule) selected
