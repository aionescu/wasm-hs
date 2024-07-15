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
    r <- newIORef @a $ error "Uninitialized Constraint dictionary"
    withDict @c r $ initialize @cs k

-- A 'Mod'ule encapsulates a series of Wasm definitions (global variables, segments, and functions).
--
-- The 'cs' constraint list represents the signatures of all the definitions contained in the entire module, to allow for
-- definitions to refer to future definitions without forward-declaring them (they are actually forward-declared in 'cs').
--
-- 'is' represents the not-yet-defined signatures _before_ this current constructor.
-- 'os' represents the not-yet-defined signatures _after_ this current constructor.
--
-- Each constructor removes the head element of 'is', and the 'Module' type alias below fixes 'is' and 'os' to ensure
-- that a module defines all the signatures it declares.
--
-- Without this, it'd be possible to refer to undefined signatures and crash during module execution.
data Mod (cs :: [Constraint]) (is :: [Constraint]) (os :: [Constraint]) where
  MSeq :: Mod cs is os -> Mod cs os os' -> Mod cs is os'

  Global :: forall v a cs is. (All cs => Var v a) => a -> Mod cs (Var v a : is) is
  GlobalSeg :: forall s a cs is. (All cs => Seg s a) => Vector a -> Mod cs (Seg s a : is) is

  Fn :: forall f i o cs is. (All cs => Fn f i o) => ((Return o, All cs) => Instr i o) -> Mod cs (Fn f i o : is) is

initMod :: All cs => Mod cs is os -> IO ()
initMod = \case
  MSeq a b -> initMod a *> initMod b
  Global @v a -> writeIORef (varRef @v) a
  GlobalSeg @s v -> writeIORef (segRef @s) v
  Fn @f e -> writeIORef (fnRef @f) $ FnCont $ eval e

type Module cs = Mod cs cs '[]

-- 'runModuleWith' executes the 'main' function of a module, then runs the user-provided continuation 'k',
-- which has access to all the definitions of the module. This can be used to e.g. read the values of global
-- variables after the module's execution.
--
-- To execute a module, first initialize dummy IORefs for each definition (handled by 'initialize' and the 'Initializable'
-- typeclass), then fill all the IORefs with the values of the definitions ('initMod'), and finally call 'main',
-- then run the user-provided continuation (which has access to all the IORefs via 'cs').

runModuleWith :: forall r cs. (Initializable cs, All cs => Fn "main" '[] '[]) => Module cs -> (All cs => IO r) -> IO r
runModuleWith m k =
  initialize @cs do
    initMod m
    evalInstr $ Call @"main" @'[] @'[]
    k

-- If you don't need to read its global variables after a module's execution, you can wrap it into a 'SomeModule'
-- to hide the (often quite long) 'cs', then use 'runModule' to execute it for side-effects.
data SomeModule = forall cs. (Initializable cs, All cs => Fn "main" '[] '[]) => SomeModule (Module cs)

withSomeModule :: SomeModule -> (forall cs. (Initializable cs, All cs => Fn "main" '[] '[]) => Module cs -> r) -> r
withSomeModule (SomeModule m) k = k m

someModule :: forall cs. (Initializable cs, All cs => Fn "main" '[] '[]) => Module cs -> SomeModule
someModule = SomeModule

runModule :: SomeModule -> IO ()
runModule (SomeModule m) = runModuleWith m $ pure ()
