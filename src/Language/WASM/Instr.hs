module Language.WASM.Instr where

import Data.Bool(bool)
import Data.IORef(IORef, readIORef, writeIORef, newIORef, modifyIORef')
import Data.Kind(Type)
import Data.Vector(Vector)
import Data.Vector qualified as V
import GHC.Exts(withDict)
import GHC.TypeError(ErrorMessage(..), Unsatisfiable)
import Prelude

import Data.HList

-- An instance of 'Var v a' means that variable 'v' is in scope
-- and has type 'a'
class Var v a | v -> a where
  varRef :: IORef a

instance Unsatisfiable (Text "Can't define explicit 'Var' instances") => Var v a where
  varRef = undefined

-- An instance of 'Label l i' means that label 'l' is in scope
-- and has input stack 'i'
class Label l i | l -> i where
  labelCont :: Cont i

instance Unsatisfiable (Text "Can't define explicit 'Label' instances") => Label l i where
  labelCont = undefined

-- An instance of 'Seg s a' means that segment 's' is in scope
-- and stores elements of type 'a'
class Seg s a | s -> a where
  segRef :: IORef (Vector a)

instance Unsatisfiable (Text "Can't define explicit 'Seq' instances") => Seg s a where
  segRef = undefined

-- An instance of 'Return o' means that the current function
-- has output stack 'o'
class Return o where
  returnCont :: Cont o

instance Unsatisfiable (Text "Can't define explicit 'Return' instances") => Return o where
  returnCont = undefined

-- An instance of 'Fn f i o' means that function 'f' is in scope
-- with input stack 'i' and output stack 'o'
class Fn f i o | f -> i o where
  -- Note the 'Fn f i o' constraint on the function's continuation.
  -- This enables self-recursion.
  fnCont :: (Return o, Fn f i o) => Cont o -> Cont i

instance Unsatisfiable (Text "Can't define explicit 'Fn' instances") => Fn f i o where
  fnCont = undefined

-- An instance of 'Append a b c' witnesses the fact that (a ++ b) == c.
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

-- An 'Instr i o' is a WASM instruction that takes an input stack 'i'
-- and produces an output stack 'o'.
data Instr (input :: Stack) (output :: Stack) where
  Nop :: Instr i i
  Unreachable :: Instr i o
  Seq :: Instr i o -> Instr o o' -> Instr i o'

  Const :: a -> Instr i (a : i)
  Drop :: Instr (a : i) i
  Dup :: Instr (a : i) (a : a : i)
  Swap :: Instr (a : b : i) (b : a : i)

  Print :: Show a => Instr (a : i) i

  Neg :: Num a => Instr (a : i) (a : i)
  Add :: Num a => Instr (a : a : i) (a : i)
  Sub :: Num a => Instr (a : a : i) (a : i)
  Mul :: Num a => Instr (a : a : i) (a : i)

  IDiv :: Integral a => Instr (a : a : i) (a : i)
  IRem :: Integral a => Instr (a : a : i) (a : i)

  FDiv :: (Eq a, Fractional a) => Instr (a : a : i) (a : i)

  Eq :: Eq a => Instr (a : a : i) (Bool : i)
  Neq :: Eq a => Instr (a : a : i) (Bool : i)
  Lt :: Ord a => Instr (a : a : i) (Bool : i)
  Lte :: Ord a => Instr (a : a : i) (Bool : i)
  Gt :: Ord a => Instr (a : a : i) (Bool : i)
  Gte :: Ord a => Instr (a : a : i) (Bool : i)

  And :: Instr (Bool : Bool : i) (Bool : i)
  Or :: Instr (Bool : Bool : i) (Bool : i)
  Not :: Instr (Bool : i) (Bool : i)

  Block :: (Label l o => Instr i o) -> Instr i o
  Loop :: (Label l i => Instr i o) -> Instr i o
  If :: (Label l o => Instr i o) -> (Label l o => Instr i o) -> Instr (Bool : i) o

  Br :: (Label l i, Append i b i') => Instr i' o
  BrIf :: Label l i => Instr (Bool : i) i

  Let :: (Var v a => Instr i o) -> Instr (a : i) o

  LocalGet :: Var v a => Instr i (a : i)
  LocalSet :: Var v a => Instr (a : i) i
  LocalTee :: Var v a => Instr (a : i) (a : i)

  LetSeg :: (Seg s a => Instr i o) -> Instr (a : Int : i) o

  SegLoad :: Seg s a => Instr (Int : i) (a : i)
  SegStore :: Seg s a => Instr (a : Int : i) i
  SegSize :: Seg s a => Instr i (Int : i)
  SegGrow :: Seg s a => Instr (a : Int : i) i

  -- Convenience instruction to print the segment's contents
  SegPrint :: (Seg s a, Show a) => Instr i i

  Call :: (Fn f i o, Append i b i', Append o b o') => Instr i' o'
  Ret :: (Return i, Append i b i') => Instr i' o

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

    Neg -> \(a :> i) -> k (-a :> i)
    Add -> \(b :> a :> i) -> k (a + b :> i)
    Sub -> \(b :> a :> i) -> k (a - b :> i)
    Mul -> \(b :> a :> i) -> k (a * b :> i)

    IDiv -> \(b :> a :> i) -> checkZero b $ k (a `div` b :> i)
    IRem -> \(b :> a :> i) -> checkZero b $ k (a `mod` b :> i)

    FDiv -> \(b :> a :> i) -> checkZero b $ k (a / b :> i)

    Eq -> \(b :> a :> i) -> k ((a == b) :> i)
    Neq -> \(b :> a :> i) -> k ((a /= b) :> i)
    Lt -> \(b :> a :> i) -> k ((a < b) :> i)
    Lte -> \(b :> a :> i) -> k ((a <= b) :> i)
    Gt -> \(b :> a :> i) -> k ((a > b) :> i)
    Gte -> \(b :> a :> i) -> k ((a >= b) :> i)

    And -> \(b :> a :> i) -> k ((a && b) :> i)
    Or -> \(b :> a :> i) -> k ((a || b) :> i)
    Not -> \(a :> i) -> k (not a :> i)

    Block @l e -> withDict @(Label l _) k $ eval e k
    Loop @l e -> let kl = withDict @(Label l _) kl $ eval e k in kl
    If @l e1 e2 -> \(b :> i) -> withDict @(Label l _) k $ eval (bool e2 e1 b) k i

    Br @l -> \i -> unappend i \i _ -> labelCont @l i
    BrIf @l -> \(b :> i) -> bool k (labelCont @l) b i

    Let @v e -> \(a :> i) -> newIORef a >>= \r -> withDict @(Var v _) r $ eval e k i

    LocalGet @v -> \i -> readIORef (varRef @v) >>= \a -> k (a :> i)
    LocalSet @v -> \(a :> i) -> writeIORef (varRef @v) a *> k i
    LocalTee @v -> \i@(a :> _) -> writeIORef (varRef @v) a *> k i

    LetSeg @s e -> \(a :> n :> i) -> newIORef (V.replicate n a) >>= \r -> withDict @(Seg s _) r $ eval e k i

    SegLoad @s -> \(n :> i) -> readIORef (segRef @s) >>= \v -> checkBounds v n $ k (v V.! n :> i)
    SegStore @s -> \(a :> n :> i) ->
      let r = segRef @s
      in readIORef r >>= \v -> checkBounds v n $ writeIORef r (v V.// [(n, a)]) *> k i

    SegSize @s -> \i -> readIORef (segRef @s) >>= \v -> k (V.length v :> i)
    SegGrow @s -> \(a :> n :> i) -> modifyIORef' (segRef @s) (<> V.replicate n a) *> k i

    SegPrint @s -> \i -> readIORef (segRef @s) >>= \v -> print v *> k i

    Call @f -> \i -> unappend i \i b ->
      let kf o = k (append o b)
      in withDict @(Return _) kf $ fnCont @f kf i

    Ret @i -> \i -> unappend i \i _ -> returnCont @i i
  where
    -- Rather than crashing with a Haskell exception, just print an error message
    -- and stop calling the continuation.
    trap :: String -> IO ()
    trap msg = putStrLn ("Execution trapped: " <> msg)

    checkZero :: (Eq a, Num a) => a -> IO () -> IO ()
    checkZero 0 _ = trap "Division by zero"
    checkZero _ k = k

    checkBounds :: Vector a -> Int -> IO () -> IO ()
    checkBounds v n k
      | n < 0 || n >= V.length v = trap "Segment access out of bounds"
      | otherwise = k

evalInstr :: Instr '[] '[] -> IO ()
evalInstr e = eval e (\_ -> pure ()) Nil
