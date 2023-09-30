{- HLINT IGNORE "Use camelCase" -}
{-# OPTIONS_GHC -Wno-orphans #-} -- The IsLabel instance is technically orphan, due to the '~' trick
module Language.WASM.Syntax where

import Data.IORef(IORef)
import Data.Vector(Vector)
import GHC.TypeLits(KnownSymbol, SSymbol, symbolSing)
import GHC.OverloadedLabels(IsLabel(..))
import Prelude hiding ((>>), const, not, div, mod, print)

import Language.WASM.Instr
import Language.WASM.Module

-- This module contains syntax sugar that improves the ergonomics of the DSL.

-- OverloadedLabels

instance (KnownSymbol s, ss ~ SSymbol s) => IsLabel s ss where
  fromLabel :: SSymbol s
  fromLabel = symbolSing

block :: forall i o s. SSymbol s -> (Label s o => Instr i o) -> Instr i o
block _ = Block

loop :: forall i o s. SSymbol s -> (Label s i => Instr i o) -> Instr i o
loop _ = Loop

br :: forall i b i' o l. (Label l i, Append i b i') => SSymbol l -> Instr i' o
br _ = Br @l

br_if :: forall i l. Label l i => SSymbol l -> Instr (Bool : i) i
br_if _ = BrIf @l

let' :: forall a i o v. SSymbol v -> (Var v a => Instr i o) -> Instr (a : i) o
let' _ = Let

let_seg :: forall a i o s. SSymbol s -> (Seg s a => Instr i o) -> Instr (a : Int : i) o
let_seg _ = LetSeg

call :: forall i o b i' o' f. (Fn f i o, Append i b i', Append o b o') => SSymbol f -> Instr i' o'
call _ = Call @f

fn :: forall i o f c. SSymbol f -> ((Return o, Fn f i o, c) => Instr i o) -> Mod c (Fn f i o, c)
fn _ = Fn

-- RebindableSyntax

class MonadLike m where
  (>>) :: m a b -> m b c -> m a c

infixr 5 >>

instance MonadLike Instr where
  (>>) :: Instr i o -> Instr o o' -> Instr i o'
  (>>) = Seq

instance MonadLike Mod where
  (>>) :: Mod c c' -> Mod c' c'' -> Mod c c''
  (>>) = MSeq

ifThenElse :: SSymbol s -> (Label s o => Instr i o) -> (Label s o => Instr i o) -> Instr (Bool : i) o
ifThenElse _ = If

-- OverloadedRecordDot

data Cmp a i =
  Cmp
  { eq :: Instr (a : a : i) (Bool : i)
  , neq :: Instr (a : a : i) (Bool : i)
  , lt :: Instr (a : a : i) (Bool : i)
  , lte :: Instr (a : a : i) (Bool : i)
  , gt :: Instr (a : a : i) (Bool : i)
  , gte :: Instr (a : a : i) (Bool : i)
  }

-- This over-constrains cmp.eq and cmp.neq to require Ord, when Eq would suffice.
-- See the comment below on 'seg' for more details.
cmp :: Ord a => Cmp a i
cmp = Cmp CmpEq CmpNeq CmpLt CmpLte CmpGt CmpGte

data Local v a i =
  Local
  { get :: SSymbol v -> Instr i (a : i)
  , set :: SSymbol v -> Instr (a : i) i
  , tee :: SSymbol v -> Instr (a : i) (a : i)
  }

local :: forall a v i. Var v a => Local v a i
local = Local (\_ -> LocalGet @v) (\_ -> LocalSet @v) (\_ -> LocalTee @v)

data Seg' s a i o =
  Seg
  { load :: SSymbol s -> Instr (Int : i) (a : i)
  , store :: SSymbol s -> Instr (a : Int : i) i
  , size :: SSymbol s -> Instr i (Int : i)
  , grow :: SSymbol s -> Instr (a : Int : i) i
  , print :: SSymbol s -> Instr i i
  }

-- This over-constrains all the seg.* instructions to require Show, even though only seg.print needs it.
-- Unfortunately we can't move the constraints into the record fields, since GHC doesn't derive HasField instances for higher-rank fields.
-- If you need to use seg.* with a type that doesn't have a Show instance, you can use the Seg* constructors directly.
seg :: forall a s i o. (Seg s a, Show a) => Seg' s a i o
seg = Seg (\_ -> SegLoad @s) (\_ -> SegStore @s) (\_ -> SegSize @s) (\_ -> SegGrow @s) (\_ -> SegPrint @s)

data Global s a c =
  Global
  { var :: SSymbol s -> a -> Mod c (Var s a, c)
  , var_ref :: SSymbol s -> IORef a -> Mod c (Var s a, c)
  , seg :: SSymbol s -> Vector a -> Mod c (Seg s a, c)
  , seg_ref :: SSymbol s -> IORef (Vector a) -> Mod c (Seg s a, c)
  }

global :: forall a s c. Global s a c
global = Global (\_ -> GlobalVar @s) (\_ -> GlobalVarRef @s) (\_ -> GlobalSeg @s) (\_ -> GlobalSegRef @s)

-- Lowercase aliases

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

add :: Num a => Instr (a : a : i) (a : i)
add = Add

sub :: Num a => Instr (a : a : i) (a : i)
sub = Sub

mul :: Num a => Instr (a : a : i) (a : i)
mul = Mul

div :: Integral a => Instr (a : a : i) (a : i)
div = Div

mod :: Integral a => Instr (a : a : i) (a : i)
mod = Mod

div_f :: (Eq a, Fractional a) => Instr (a : a : i) (a : i)
div_f = FDiv

neg :: Num a => Instr (a : i) (a : i)
neg = Neg

and :: Instr (Bool : Bool : i) (Bool : i)
and = And

or :: Instr (Bool : Bool : i) (Bool : i)
or = Or

not :: Instr (Bool : i) (Bool : i)
not = Not

ret :: forall i b i' o. (Return i, Append i b i') => Instr i' o
ret = Return @i

main :: (c => NoMain) => (c => Instr '[] '[]) -> Mod c ()
main = Main
