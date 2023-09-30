module Language.WASM.Module where

import Data.IORef(IORef, newIORef)
import Data.Kind(Constraint)
import Data.Vector(Vector)
import GHC.Exts(withDict)
import GHC.TypeError(TypeError, ErrorMessage(..))
import Prelude

import Language.WASM.Instr

class NoMain where
  dummy :: () -- Needed for 'withDict' to work

instance TypeError (Text "Each WASM module must contain exactly one 'main' definition.") => NoMain where
  dummy = ()

-- A 'Mod'ule encapsulates a series of WASM definitions (global variables, segments, and functions).
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

  -- Since all the other constructors _add_ constraints to the output,
  -- if you have a 'Mod c ()' value, it's guaranteed to end with a 'Main'.
  Main :: (c => NoMain) => (c => Instr '[] '[]) -> Mod c ()

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
    Main e -> evalInstr e

type Module = Mod NoMain ()

-- Since the 'Main' constructor drops the final continuation, and the 'Mod () ()' type guarantees
-- the module ends with a 'Main', the final continuation can simply be 'undefined'.
evalModule :: Module -> IO ()
evalModule m = withDict @NoMain () $ evalMod m undefined
