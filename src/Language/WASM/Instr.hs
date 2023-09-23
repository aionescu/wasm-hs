module Language.WASM.Instr where

import Data.Bool(bool)
import Data.IORef(IORef, readIORef, writeIORef, newIORef, modifyIORef')
import Data.Kind(Type)
import Data.Vector(Vector)
import Data.Vector qualified as V
import GHC.Exts(withDict)
import Prelude

import Data.HList

-- Type classes used for name resolution (variables, labels etc.)

class Var v a | v -> a where
  varRef :: IORef a

class Label l i | l -> i where
  labelCont :: Cont i

class Seg s a | s -> a where
  segRef :: IORef (Vector a)

class Return o where
  returnCont :: Cont o

class Fn f i o | f -> i o where
  fnCont :: (Return o, Fn f i o) => Cont o -> Cont i

-- Type class for splitting off and merging the top of the data stack,
-- used for jumps and function calls.
class Append a b c | a b -> c, a c -> b where
  append :: HList a -> HList b -> HList c
  unappend :: HList c -> (HList a -> HList b -> r) -> r

instance Append '[] b b where
  append :: HList '[] -> HList b -> HList b
  append Nil b = b

  unappend :: HList b -> (HList '[] -> HList b -> r) -> r
  unappend b k = k Nil b

instance Append a b c => Append (x ': a) b (x ': c) where
  append :: HList (x ': a) -> HList b -> HList (x ': c)
  append (x :> a) b = x :> append a b

  unappend :: HList (x ': c) -> (HList (x ': a) -> HList b -> r) -> r
  unappend (x :> c) k = unappend c $ k . (x :>)

type Stack = [Type]

data Instr (input :: Stack) (output :: Stack) where
  Nop :: Instr i i
  Unreachable :: Instr i o
  Seq :: Instr i o -> Instr o o' -> Instr i o'

  Const :: a -> Instr i (a : i)
  Drop :: Instr (a : i) i
  Dup :: Instr (a : i) (a : a : i)
  Swap :: Instr (a : b : i) (b : a : i)

  Log :: Show a => Instr (a : i) i

  Add :: Num a => Instr (a : a : i) (a : i)
  Sub :: Num a => Instr (a : a : i) (a : i)
  Mul :: Num a => Instr (a : a : i) (a : i)
  Div :: Integral a => Instr (a : a : i) (a : i)
  Mod :: Integral a => Instr (a : a : i) (a : i)
  FDiv :: (Eq a, Fractional a) => Instr (a : a : i) (a : i)
  Neg :: Num a => Instr (a : i) (a : i)

  CmpEq :: Eq a => Instr (a : a : i) (Bool : i)
  CmpNeq :: Eq a => Instr (a : a : i) (Bool : i)
  CmpLt :: Ord a => Instr (a : a : i) (Bool : i)
  CmpLte :: Ord a => Instr (a : a : i) (Bool : i)
  CmpGt :: Ord a => Instr (a : a : i) (Bool : i)
  CmpGte :: Ord a => Instr (a : a : i) (Bool : i)

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
  SegLog :: (Seg s a, Show a) => Instr i i

  Call :: (Fn f i o, Append i b i', Append o b o') => Instr i' o'
  Return :: (Return i, Append i b i') => Instr i' o

type Cont i = HList i -> [String] -> IO [String]

-- eval is a function that takes an instruction and an input stack,
-- and produces the output stack and log results in IO (for mutating variables):
--     eval :: Instr i o -> HList i -> IO (HList o, [String])
--
-- Then, convert it to CPS to make jump instructions efficient, and also CPS the Writer monad used for logging:
--     eval :: Instr i o -> HList i -> [String] -> (HList o -> [String] -> IO [String]) -> IO [String]
--
-- Then, swap the order of arguments, to make it easier to compose continuations:
--     eval :: Instr i o -> (HList o -> [String] -> IO [String]) -> HList i -> [String] -> IO [String]
--
-- Then, add a type alias for continuations:
--     eval :: Instr i o -> Cont o -> Cont i
--
-- Since eval is called on each instruction exactly once, it could also be seen as
-- a compiler that compiles each instruction into a "continuation transformer".
eval :: Instr i o -> Cont o -> Cont i
eval e k =
  case e of
    Nop -> k
    -- Rather than crashing with a Haskell exception, instead just stop calling the continuation.
    -- Otherwise, if a program traps, all previous logging output is lost.
    Unreachable -> \_ -> trap "Unreachable was reached"

    Seq a b -> eval a $ eval b k

    Const a -> \i -> k (a :> i)
    Drop -> \(_ :> i) -> k i
    Dup -> \(a :> i) -> k (a :> a :> i)
    Swap -> \(a :> b :> i) -> k (b :> a :> i)

    Log -> \(a :> i) w -> k i (show a : w)

    Add -> \(b :> a :> i) -> k (a + b :> i)
    Sub -> \(b :> a :> i) -> k (a - b :> i)
    Mul -> \(b :> a :> i) -> k (a * b :> i)
    Div -> \case
      0 :> _ -> trap "Division by zero"
      b :> a :> i -> k (a `div` b :> i)
    Mod -> \case
      0 :> _ -> trap "Division by zero"
      b :> a :> i -> k (a `mod` b :> i)
    FDiv -> \case
      0 :> _ -> trap "Division by zero"
      b :> a :> i -> k (a / b :> i)
    Neg -> \(a :> i) -> k (-a :> i)

    CmpEq -> \(b :> a :> i) -> k ((a == b) :> i)
    CmpNeq -> \(b :> a :> i) -> k ((a /= b) :> i)
    CmpLt -> \(b :> a :> i) -> k ((a < b) :> i)
    CmpLte -> \(b :> a :> i) -> k ((a <= b) :> i)
    CmpGt -> \(b :> a :> i) -> k ((a > b) :> i)
    CmpGte -> \(b :> a :> i) -> k ((a >= b) :> i)

    And -> \(b :> a :> i) -> k ((a && b) :> i)
    Or -> \(b :> a :> i) -> k ((a || b) :> i)
    Not -> \(a :> i) -> k (not a :> i)

    Block @l e -> withDict @(Label l _) k $ eval e k
    Loop @l e -> let kl = withDict @(Label l _) kl $ eval e k in kl
    If @l e1 e2 -> \(b :> i) -> withDict @(Label l _) k $ eval (bool e2 e1 b) k i

    Br @l -> \i -> unappend i \i _ -> labelCont @l i
    BrIf @l -> \(b :> i) -> bool k (labelCont @l) b i

    Let @v e -> \(a :> i) w -> newIORef a >>= \r -> withDict @(Var v _) r $ eval e k i w

    LocalGet @v -> \i w -> readIORef (varRef @v) >>= \a -> k (a :> i) w
    LocalSet @v -> \(a :> i) w -> writeIORef (varRef @v) a *> k i w
    LocalTee @v -> \i@(a :> _) w -> writeIORef (varRef @v) a *> k i w

    LetSeg @s e -> \(a :> n :> i) w -> newIORef (V.replicate n a) >>= \r -> withDict @(Seg s _) r $ eval e k i w

    SegLoad @s -> \(n :> i) w -> readIORef (segRef @s) >>= \v -> boundsCheck v n w $ k (v V.! n :> i) w
    SegStore @s -> \(a :> n :> i) w ->
      let r = segRef @s
      in readIORef r >>= \v -> boundsCheck v n w $ writeIORef r (v V.// [(n, a)]) *> k i w

    SegSize @s -> \i w -> readIORef (segRef @s) >>= \v -> k (V.length v :> i) w
    SegGrow @s -> \(a :> n :> i) w -> modifyIORef' (segRef @s) (<> V.replicate n a) *> k i w

    SegLog @s -> \i w -> readIORef (segRef @s) >>= \v -> k i (show v : w)

    Call @f -> \i -> unappend i \i b ->
      let kf = k . (`append` b)
      in withDict @(Return _) kf $ fnCont @f kf i

    Return @i -> \i -> unappend i \i _ -> returnCont @i i
  where
    trap :: String -> [String] -> IO [String]
    trap msg w = pure ("Execution trapped: " <> msg : w)

    boundsCheck :: Vector a -> Int -> [String] -> IO [String] -> IO [String]
    boundsCheck v n w k
      | n < 0 || n >= V.length v = trap "Segment access out of bounds" w
      | otherwise = k

evalInstr :: Instr '[] '[] -> IO [String]
evalInstr e = eval e (\_ -> pure . reverse) Nil []
