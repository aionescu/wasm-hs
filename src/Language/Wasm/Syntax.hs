{- HLINT IGNORE "Use camelCase" -}
{-# OPTIONS_GHC -Wno-orphans #-} -- The IsLabel instance is technically orphan, due to the '~' trick
module Language.Wasm.Syntax where

import Data.IORef(IORef)
import Data.Vector(Vector)
import GHC.Exts(WithDict(..))
import GHC.TypeLits(KnownSymbol(..), SSymbol)
import GHC.OverloadedLabels(IsLabel(..))
import Prelude hiding ((>>), const, not, div, mod, print)
import Prelude qualified as P

import Language.Wasm.Instr
import Language.Wasm.Module

-- This module contains syntax sugar that improves the ergonomics of the DSL.

instance (KnownSymbol s, ss ~ SSymbol s) => IsLabel s ss where
  fromLabel :: SSymbol s
  fromLabel = symbolSing

class MonadLike m where
  (>>) :: m a b -> m b c -> m a c

infixr 5 >>

instance MonadLike Instr where
  (>>) :: Instr i o -> Instr o o' -> Instr i o'
  (>>) = Seq

instance MonadLike (Mod cs) where
  (>>) :: Mod cs is os -> Mod cs os os' -> Mod cs is os'
  (>>) = MSeq

-- Instructions

nop :: Instr i i
nop = Nop

unreachable :: Instr i o
unreachable = Unreachable

const :: a -> Instr i (a : i)
const = Const

drop :: Instr (a : i) i
drop = Drop

dup :: Instr (a : i) (a : a : i)
dup = Dup

swap :: Instr (a : b : i) (b : a : i)
swap = Swap

print :: Show a => Instr (a : i) i
print = Print

unary' :: (a -> Either String b) -> Instr (a : i) (b : i)
unary' = UnaryOp

binary' :: (a -> b -> Either String c) -> Instr (b : a : i) (c : i)
binary' = BinaryOp

unary :: (a -> b) -> Instr (a : i) (b : i)
unary f = UnaryOp (Right . f)

binary :: (a -> b -> c) -> Instr (b : a : i) (c : i)
binary f = BinaryOp ((Right .) . f)

checkZero :: (Num b, Eq b) => (a -> b -> c) -> (a -> b -> Either String c)
checkZero _ _ 0 = Left "Division by zero"
checkZero f a b = Right $ f a b

neg :: Num a => Instr (a : i) (a : i)
neg = unary negate

add :: Num a => Instr (a : a : i) (a : i)
add = binary (+)

sub :: Num a => Instr (a : a : i) (a : i)
sub = binary (-)

mul :: Num a => Instr (a : a : i) (a : i)
mul = binary (*)

div :: Integral a => Instr (a : a : i) (a : i)
div = binary' $ checkZero P.div

mod :: Integral a => Instr (a : a : i) (a : i)
mod = binary' $ checkZero P.mod

div_f :: (Fractional a, Eq a) => Instr (a : a : i) (a : i)
div_f = binary' $ checkZero (/)

eq :: Eq a => Instr (a : a : i) (Bool : i)
eq = binary (==)

neq :: Eq a => Instr (a : a : i) (Bool : i)
neq = binary (/=)

lt :: Ord a => Instr (a : a : i) (Bool : i)
lt = binary (<)

gt :: Ord a => Instr (a : a : i) (Bool : i)
gt = binary (>)

lte :: Ord a => Instr (a : a : i) (Bool : i)
lte = binary (<=)

gte :: Ord a => Instr (a : a : i) (Bool : i)
gte = binary (>=)

and :: Instr (Bool : Bool : i) (Bool : i)
and = binary (&&)

or :: Instr (Bool : Bool : i) (Bool : i)
or = binary (||)

not :: Instr (Bool : i) (Bool : i)
not = unary P.not

block :: forall i o s. SSymbol s -> (Label s o => Instr i o) -> Instr i o
block _ = Block

loop :: forall i o s. SSymbol s -> (Label s i => Instr i o) -> Instr i o
loop _ = Loop

ifThenElse :: Instr i' (Bool : i) -> Instr i o -> Instr i o -> Instr i' o
ifThenElse c t f = Seq c $ If t f

br :: forall i b i' o l. (Label l i, Append i b i') => SSymbol l -> Instr i' o
br _ = Br @l

br_if :: forall i l. Label l i => SSymbol l -> Instr (Bool : i) i
br_if _ = BrIf @l

let' :: forall a i o s. SSymbol s -> (Local s a => Instr i o) -> Instr (a : i) o
let' _ = Let @s

data Local' s a i =
  Local
  { get :: SSymbol s -> Instr i (a : i)
  , set :: SSymbol s -> Instr (a : i) i
  , tee :: SSymbol s -> Instr (a : i) (a : i)
  }

local :: forall a s i. Local s a => Local' s a i
local = Local (\_ -> LocalGet @s) (\_ -> LocalSet @s) (\_ -> LocalTee @s)

let_mem :: forall a i o s. SSymbol s -> (Mem s a => Instr i o) -> Instr (a : Int : i) o
let_mem _ = LetMem @s

data Mem' s a i o =
  Mem
  { load :: SSymbol s -> Instr (Int : i) (a : i)
  , store :: SSymbol s -> Instr (a : Int : i) i
  , size :: SSymbol s -> Instr i (Int : i)
  , grow :: SSymbol s -> Instr (a : Int : i) i
  , print :: SSymbol s -> Instr i i
  }

-- This over-constrains all the mem.* instructions to require Show, even though only mem.print needs it.
-- Unfortunately we can't move the constraints into the record fields, since GHC doesn't derive HasField instances for higher-rank fields.
-- If you need to use mem.* instructions with a type that doesn't have a Show instance, you can use the Mem* constructors directly.
mem :: forall a s i o. (Mem s a, Show a) => Mem' s a i o
mem = Mem (\_ -> MemLoad @s) (\_ -> MemStore @s) (\_ -> MemSize @s) (\_ -> MemGrow @s) (\_ -> MemPrint @s)

call :: forall i o b i' o' f. (Fn f i o, Append i b i', Append o b o') => SSymbol f -> Instr i' o'
call _ = Call @f

ret :: forall i b i' o. (Ret i, Append i b i') => Instr i' o
ret = Ret @i

-- Modules

global :: forall a s cs is. (All cs => Local s a) => SSymbol s -> a -> Mod cs (Local s a : is) is
global _ = Global @s

global_mem :: forall a s cs is. (All cs => Mem s a) => SSymbol s -> Vector a -> Mod cs (Mem s a : is) is
global_mem _ = GlobalMem @s

fn :: forall i o f cs is. (All cs => Fn f i o) => SSymbol f -> ((Ret o, All cs) => Instr i o) -> Mod cs (Fn f i o : is) is
fn _ = Fn @f

-- Host-shared references

host_global :: forall a r s. SSymbol s -> IORef a -> (Local s a => r) -> r
host_global _ = withDict @(Local s _)

host_mem :: forall a r s. SSymbol s -> IORef (Vector a) -> (Mem s a => r) -> r
host_mem _ = withDict @(Mem s _)
