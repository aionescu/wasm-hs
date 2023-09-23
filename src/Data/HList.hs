module Data.HList where

import Data.Kind(Type)
import Prelude

-- Classic heterogeneous list implementation.
data HList :: [Type] -> Type where
  Nil :: HList '[]
  (:>) :: a -> HList as -> HList (a : as)

infixr 5 :>

deriving stock instance Show (HList '[])
deriving stock instance (Show a, Show (HList as)) => Show (HList (a : as))
