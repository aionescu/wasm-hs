module Language.Wasm.Module where

import Data.IORef(IORef, newIORef, writeIORef)
import Data.Kind(Constraint)
import Data.Vector(Vector)
import GHC.Exts(WithDict(..))
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

-- A 'Mod'ule encapsulates a series of Wasm definitions (global variables, memories, and functions).
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

  Global :: forall s a cs is. (All cs => Local s a) => a -> Mod cs (Local s a : is) is
  GlobalMem :: forall s a cs is. (All cs => Mem s a) => Vector a -> Mod cs (Mem s a : is) is

  Fn :: forall s i o cs is. (All cs => Fn s i o) => ((Ret o, All cs) => Instr i o) -> Mod cs (Fn s i o : is) is

initMod :: All cs => Mod cs is os -> IO ()
initMod = \case
  MSeq a b -> initMod a *> initMod b
  Global @s a -> writeIORef (localRef @s) a
  GlobalMem @s v -> writeIORef (memRef @s) v
  Fn @s e -> writeIORef (fnRef @s) $ FnCont $ eval e

type Module cs = Mod cs cs '[]

-- To execute a module, first initialize dummy IORefs for each definition (handled by 'initialize' and the 'Initializable'
-- typeclass), then fill all the IORefs with the values of the definitions ('initMod'), and finally call 'main'.
runModule :: forall cs. (Initializable cs, All cs => Fn "main" '[] '[]) => Module cs -> IO ()
runModule m =
  initialize @cs do
    initMod m
    evalInstr $ Call @"main" @'[] @'[]

-- In most cases, you want to wrap your modules into 'WasmModule's (using the 'wasm' function defined below), to
-- hide the very long 'cs' list. You can use 'host_global' and 'host_mem' to initialize global variables
-- or memories with pre-existing 'IORef's.
data WasmModule = forall cs. (Initializable cs, All cs => Fn "main" '[] '[]) => WasmModule (Module cs)

wasm :: forall cs. (Initializable cs, All cs => Fn "main" '[] '[]) => Module cs -> WasmModule
wasm = WasmModule

runWasm :: WasmModule -> IO ()
runWasm (WasmModule m) = runModule m
