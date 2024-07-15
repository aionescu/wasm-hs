module Language.Wasm.Module where

import Data.IORef(IORef, newIORef, writeIORef)
import Data.Kind(Constraint)
import Data.Vector(Vector)
import GHC.Exts(withDict, WithDict)
import Prelude

import Language.Wasm.Instr

type family All (cs :: [Constraint]) :: Constraint where
  All '[] = ()
  All (c : cs) = (c, All cs)

class Initializable cs where
  initialize :: (All cs => IO r) -> IO r

instance Initializable '[] where
  initialize :: (All '[] => IO r) -> IO r
  initialize k = k

instance (WithDict c (IORef a), Initializable cs) => Initializable (c : cs) where
  initialize :: (All (c : cs) => IO r) -> IO r
  initialize k = do
    r <- newIORef @a $ error "Uninitialized Constraint dict"
    withDict @c r $ initialize @cs k

-- A 'Mod'ule encapsulates a series of Wasm definitions (global variables, segments, and functions).
-- The 'cs' constraint list represents the definitions contained in the module.
data Mod (cs :: [Constraint]) where
  MSeq :: Mod cs -> Mod cs -> Mod cs

  LetGlobal :: forall v a cs. (All cs => Var v a) => a -> Mod cs
  LetGlobalSeg :: forall s a cs. (All cs => Seg s a) => Vector a -> Mod cs

  Fn :: forall f i o cs. (All cs => Fn f i o) => ((Return o, All cs) => Instr i o) -> Mod cs

initMod :: All cs => Mod cs -> IO ()
initMod = \case
  MSeq a b -> initMod a *> initMod b
  LetGlobal @v a -> writeIORef (varRef @v) a
  LetGlobalSeg @s v -> writeIORef (segRef @s) v
  Fn @f e -> writeIORef (fnRef @f) $ FnCont $ eval e

-- 'runWasmWith' executes the 'main' function of a module, then runs the user-provided continuation 'k',
-- which has access to all the definitions of the module. This can be used to e.g. read the values of global
-- variables after the module's execution.
runWasmWith :: forall r cs. (Initializable cs, All cs => Fn "main" '[] '[]) => Mod cs -> (All cs => IO r) -> IO r
runWasmWith m k =
  initialize @cs do
    initMod m
    evalInstr $ Call @"main" @'[] @'[]
    k

-- If you don't need to read its global variables after a module's execution, you can wrap it into a 'SomeMod'
-- to hide the (often quite long) 'cs', then use 'runWasm' to execute it for side-effects.
data SomeMod = forall cs. (Initializable cs, All cs => Fn "main" '[] '[]) => SomeMod (Mod cs)

withSomeMod :: SomeMod -> (forall cs. (Initializable cs, All cs => Fn "main" '[] '[]) => Mod cs -> r) -> r
withSomeMod (SomeMod m) k = k m

someMod :: forall cs. (Initializable cs, All cs => Fn "main" '[] '[]) => Mod cs -> SomeMod
someMod = SomeMod

runWasm :: SomeMod -> IO ()
runWasm (SomeMod m) = runWasmWith m $ pure ()
