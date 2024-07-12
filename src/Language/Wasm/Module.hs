module Language.Wasm.Module where

import Data.IORef(IORef, newIORef)
import Data.Kind(Constraint)
import Data.Vector(Vector)
import GHC.Exts(withDict)
import Prelude

import Language.Wasm.Instr

-- A 'Mod'ule encapsulates a series of Wasm definitions (global variables, segments, and functions).
-- The 'before' constraint represents definitions that are already in scope,
-- and the 'after' constraint represents the definitions that are added by the module.
data Mod (before :: Constraint) (after :: Constraint) where
  MSeq :: Mod c c' -> Mod c' c'' -> Mod c c''

  GlobalVar :: forall v a c. a -> Mod c (Var v a, c)
  GlobalVarRef :: forall v a c. IORef a -> Mod c (Var v a, c)
  -- The 'Ref' constructors are used to initialize global variables with host-shared references.

  GlobalSeg :: forall s a c. Vector a -> Mod c (Seg s a, c)
  GlobalSegRef :: forall s a c. IORef (Vector a) -> Mod c (Seg s a, c)

  Fn :: forall f i o c. ((Return o, Fn f i o, c) => Instr i o) -> Mod c (Fn f i o, c)

-- Conceptually, evalMod takes a constraint as input, and produces an updated
-- constraint as output (plus IO, for allocating variables and executing instructions):
--     evalMod :: Mod c c' -> c -> IO c'
--
-- But since constraints aren't first-class values in Haskell, a CPS conversion is
-- needed (using '=>' instead of '->'):
--     evalMod :: Mod c c' -> (c' => IO ()) -> (c => IO ())
--
-- Finally, the "input" constraint is floated to the left of the signature:
--     evalMod :: c => Mod c c' -> (c' => IO ()) -> IO ()
evalMod :: c => Mod c c' -> (c' => IO ()) -> IO ()
evalMod m k =
  case m of
    MSeq a b -> evalMod a $ evalMod b k

    GlobalVar @v a -> newIORef a >>= \r -> withDict @(Var v _) r k
    GlobalVarRef @v r -> withDict @(Var v _) r k

    GlobalSeg @s v -> newIORef v >>= \r -> withDict @(Seg s _) r k
    GlobalSegRef @s r -> withDict @(Seg s _) r k

    Fn @f @i @o e -> withDict @(Fn f _ _) @((Return o, Fn f i o) => _) (eval e) k

data Module = forall c. (c => Fn "main" '[] '[]) => Module { mod :: Mod () c }

runWasm :: Module -> IO ()
runWasm (Module m) = evalMod m $ evalInstr $ Call @"main" @'[] @'[]
