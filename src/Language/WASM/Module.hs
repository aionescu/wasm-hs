module Language.WASM.Module where

import Data.IORef(IORef, newIORef)
import Data.Kind(Constraint)
import Data.Vector(Vector)
import GHC.Exts(withDict)
import Prelude

import Language.WASM.Instr

-- The 'Ref' constructors are used to allow the host environment to pass
-- shared references to the WASM module, and observe the mutations after
-- executing the module.
data Mod (c :: Constraint) (c' :: Constraint) where
  MSeq :: Mod c c' -> Mod c' c'' -> Mod c c''

  GlobalVar :: forall v a c. a -> Mod c (Var v a, c)
  GlobalVarRef :: forall v a c. IORef a -> Mod c (Var v a, c)

  GlobalSeg :: forall s a c. Vector a -> Mod c (Seg s a, c)
  GlobalSegRef :: forall s a c. IORef (Vector a) -> Mod c (Seg s a, c)

  Fn :: forall f i o c. ((Return o, Fn f i o, c) => Instr i o) -> Mod c (Fn f i o, c)
  Main :: (c => Instr '[] '[]) -> Mod c ()

evalMod :: c => Mod c c' -> (c' => IO [String]) -> IO [String]
evalMod m k =
  case m of
    MSeq a b -> evalMod a $ evalMod b k

    GlobalVar @v a -> newIORef a >>= \r -> withDict @(Var v _) r k
    GlobalVarRef @v r -> withDict @(Var v _) r k

    GlobalSeg @s v -> newIORef v >>= \r -> withDict @(Seg s _) r k
    GlobalSegRef @s r -> withDict @(Seg s _) r k

    Fn @f @i @o e -> withDict @(Fn f _ _) @((Return o, Fn f i o) => _) (eval e) k
    Main e -> evalInstr e

type Module = Mod () ()

-- Since 'Main' drops the final continuation, and the 'Mod () ()' type forces
-- the module to end with a 'Main', the final continuation can simply be 'undefined'.
evalModule :: Module -> IO [String]
evalModule m = evalMod m undefined
