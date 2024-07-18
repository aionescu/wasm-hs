module Language.Wasm.Instr where

import Data.Bool(bool)
import Data.IORef(IORef, readIORef, modifyIORef', newIORef, writeIORef)
import Data.Kind(Type)
import Data.Vector(Vector)
import Data.Vector qualified as V
import GHC.Exts(withDict)
import GHC.TypeError(ErrorMessage(..), Unsatisfiable)
import GHC.TypeLits(Symbol)
import Prelude

import Data.HList

-- An instance of 'Local s a' means that local variable 's' is in scope
-- and has type 'a'.
class Local (s :: Symbol) a | s -> a where
  localRef :: IORef a

instance Unsatisfiable (Text "Local variable " :<>: ShowType s :<>: Text " is not in scope.") => Local s a where
  localRef :: IORef a
  localRef = undefined

-- An instance of 'Label s i' means that label 's' is in scope
-- and has input stack 'i'.
class Label (s :: Symbol) i | s -> i where
  labelCont :: Cont i

instance Unsatisfiable (Text "Label " :<>: ShowType s :<>: Text " is not in scope.") => Label s i where
  labelCont :: Cont i
  labelCont = undefined

-- An instance of 'Mem s a' means that memory 's' is in scope
-- and stores elements of type 'a'.
class Mem (s :: Symbol) a | s -> a where
  memRef :: IORef (Vector a)

instance Unsatisfiable (Text "Memory " :<>: ShowType s :<>: Text " is not in scope.") => Mem s a where
  memRef :: IORef (Vector a)
  memRef = undefined

-- An instance of 'Ret o' means that the current function
-- has output stack 'o'.
class Ret o where
  retCont :: Cont o

instance Unsatisfiable (Text "Cannot 'return' out of this context.") => Ret o where
  retCont :: Cont o
  retCont = undefined

newtype FnCont i o = FnCont (Ret o => Cont o -> Cont i)

-- An instance of 'Fn s i o' means that function 's' is in scope
-- with input stack 'i' and output stack 'o'.
class Fn (s :: Symbol) i o | s -> i o where
  fnRef :: IORef (FnCont i o)

instance Unsatisfiable (Text "Function " :<>: ShowType s :<>: Text " is not in scope.") => Fn s i o where
  fnRef :: IORef (FnCont i o)
  fnRef = undefined

-- An instance of 'Append a b c' witnesses the fact that (a ++ b) ≡ c.
-- The class methods enable splitting and merging data stacks,
-- and are used for jumps and function calls.
class Append a b c | a b -> c, a c -> b where
  append :: HList a -> HList b -> HList c

  -- unappend :: HList c -> (HList a, HList b)
  -- CPS'ed to avoid allocating tuples
  unappend :: HList c -> (HList a -> HList b -> r) -> r

instance Append '[] b b where
  append :: HList '[] -> HList b -> HList b
  append Nil b = b

  unappend :: HList b -> (HList '[] -> HList b -> r) -> r
  unappend b k = k Nil b

instance Append a b c => Append (x : a) b (x : c) where
  append :: HList (x : a) -> HList b -> HList (x : c)
  append (x :> a) b = x :> append a b

  unappend :: HList (x : c) -> (HList (x : a) -> HList b -> r) -> r
  unappend (x :> c) k = unappend c \a -> k (x :> a)

type Stack = [Type]

-- An 'Instr i o' is a Wasm instruction that accepts an input stack 'i'
-- and produces an output stack 'o'.
data Instr (i :: Stack) (o :: Stack) where
  Nop :: Instr i i
  Unreachable :: Instr i o
  Seq :: Instr i o -> Instr o o' -> Instr i o'

  Const :: a -> Instr i (a : i)
  Drop :: Instr (a : i) i
  Dup :: Instr (a : i) (a : a : i)
  Swap :: Instr (a : b : i) (b : a : i)

  Print :: Show a => Instr (a : i) i

  -- Lift a unary or binary function into an instruction.
  -- If the lifted function returns 'Left err', execution traps with message 'err'.
  UnaryOp :: (a -> Either String b) -> Instr (a : i) (b : i)
  BinaryOp :: (a -> b -> Either String c) -> Instr (b : a : i) (c : i)

  Block :: (Label s o => Instr i o) -> Instr i o
  Loop :: (Label s i => Instr i o) -> Instr i o
  If :: Instr i o -> Instr i o -> Instr (Bool : i) o

  Br :: (Label s i, Append i b i') => Instr i' o
  BrIf :: Label s i => Instr (Bool : i) i

  Let :: (Local s a => Instr i o) -> Instr (a : i) o

  LocalGet :: Local s a => Instr i (a : i)
  LocalSet :: Local s a => Instr (a : i) i
  LocalTee :: Local s a => Instr (a : i) (a : i)

  LetMem :: (Mem s a => Instr i o) -> Instr (a : Int : i) o

  MemLoad :: Mem s a => Instr (Int : i) (a : i)
  MemStore :: Mem s a => Instr (a : Int : i) i
  MemSize :: Mem s a => Instr i (Int : i)
  MemGrow :: Mem s a => Instr (a : Int : i) i

  -- Convenience instruction to print a memory's contents.
  MemPrint :: (Mem s a, Show a) => Instr i i

  Call :: (Fn s i o, Append i b i', Append o b o') => Instr i' o'
  Ret :: (Ret i, Append i b i') => Instr i' o

type Cont i = HList i -> IO ()

-- 'eval' is a function that takes an instruction and an input stack,
-- and produces an output stack in IO (to permit side-effects for mutating variables and printing):
--     eval :: Instr i o -> HList i -> IO (HList o)
--
-- Then, convert it to CPS, to make jump instructions more efficient:
--     eval :: Instr i o -> HList i -> (HList o -> IO ()) -> IO ()
--
-- Then, swap the order of arguments, to make it easier to compose continuations:
--     eval :: Instr i o -> (HList o -> IO ()) -> HList i -> IO ()
--
-- Then, add a type alias for continuations:
--     eval :: Instr i o -> Cont o -> Cont i
--
-- Since 'eval' is called on each instruction exactly once, it could also be seen as
-- as compiling each instruction into a "continuation transformer".
eval :: Instr i o -> Cont o -> Cont i
eval e k =
  case e of
    Nop -> k
    Unreachable -> \_ -> trap "Unreachable was reached"

    Seq a b -> eval a $ eval b k

    Const a -> \i -> k (a :> i)
    Drop -> \(_ :> i) -> k i
    Dup -> \(a :> i) -> k (a :> a :> i)
    Swap -> \(a :> b :> i) -> k (b :> a :> i)

    Print -> \(a :> i) -> print a *> k i

    UnaryOp f -> \(a :> i) -> trapEither k i $ f a
    BinaryOp f -> \(b :> a :> i) -> trapEither k i $ f a b

    Block @s e -> withDict @(Label s _) k $ eval e k
    Loop @s e -> let kl = withDict @(Label s _) kl $ eval e k in kl
    If t f -> \(b :> i) -> eval (bool f t b) k i

    Br @s -> \i -> unappend i \i _ -> labelCont @s i
    BrIf @s -> \(b :> i) -> bool k (labelCont @s) b i

    Let @s e -> \(a :> i) -> newIORef a >>= \r -> withDict @(Local s _) r $ eval e k i

    LocalGet @s -> \i -> readIORef (localRef @s) >>= \a -> k (a :> i)
    LocalSet @s -> \(a :> i) -> writeIORef (localRef @s) a *> k i
    LocalTee @s -> \i@(a :> _) -> writeIORef (localRef @s) a *> k i

    LetMem @s e -> \(a :> n :> i) -> newIORef (V.replicate n a) >>= \r -> withDict @(Mem s _) r $ eval e k i

    MemLoad @s -> \(n :> i) -> readIORef (memRef @s) >>= \v -> checkBounds v n $ k (v V.! n :> i)
    MemStore @s -> \(a :> n :> i) ->
      let r = memRef @s
      in readIORef r >>= \v -> checkBounds v n $ writeIORef r (v V.// [(n, a)]) *> k i

    MemSize @s -> \i -> readIORef (memRef @s) >>= \v -> k (V.length v :> i)
    MemGrow @s -> \(a :> n :> i) -> modifyIORef' (memRef @s) (<> V.replicate n a) *> k i

    MemPrint @s -> \i -> readIORef (memRef @s) >>= \v -> print v *> k i

    Call @s -> \i -> unappend i \i b ->
      let retk o = k (append o b)
      in readIORef (fnRef @s) >>= \(FnCont fnCont) -> withDict @(Ret _) retk $ fnCont retk i

    Ret @i -> \i -> unappend i \i _ -> retCont @i i
  where
    -- Rather than crashing with a Haskell exception, just print an error message
    -- and stop calling the continuation.
    trap :: String -> IO ()
    trap msg = putStrLn ("Execution trapped: " <> msg)

    -- Unwrap an 'Either' and push its contents on the stack, or trap if it's a 'Left'.
    trapEither :: Cont (a : i) -> HList i -> Either String a -> IO ()
    trapEither k i = \case
      Left err -> trap err
      Right a -> k (a :> i)

    checkBounds :: Vector a -> Int -> IO () -> IO ()
    checkBounds v n k
      | n < 0 || n >= V.length v = trap "Memory access out of bounds"
      | otherwise = k

evalInstr :: Instr '[] '[] -> IO ()
evalInstr e = eval e (\_ -> pure ()) Nil
