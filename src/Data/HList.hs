module Data.HList where

import Data.Kind(Type)
import GHC.Exts(UnliftedType)

-- Classic heterogeneous list implementation, except it's unlifted (for extra performance).
data HList :: [Type] -> UnliftedType where
  Nil :: HList '[]
  (:>) :: a -> HList as -> HList (a : as)

infixr 5 :>
