module Language.WASM.Prelude(module X) where

import Data.IORef as X (IORef)
import Data.Vector as X (Vector)
import GHC.OverloadedLabels as X (fromLabel)
import GHC.Records as X (getField)
import Prelude as X hiding ((>>), const, not, div, mod, drop, and, or, print)

import Data.HList as X
import Language.WASM.Instr as X
import Language.WASM.Module as X
import Language.WASM.Syntax as X

-- This is just a convenience module that re-exports all required items
-- to write and evaluate WASM programs (including the base Prelude).
