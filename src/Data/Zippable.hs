----------------------------------------------------------------------------
-- |
-- Module      :  Data.Zippable
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Data.Zippable where

import qualified Data.List as L
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U

import Data.ConstrainedFunctor

class (ConstrainedFunctor f) => Zippable f where
  zipWith
    :: (ElemConstraints f a, ElemConstraints f b, ElemConstraints f c)
    => (a -> b -> c) -> f a -> f b -> f c
  zipWith3
    :: (ElemConstraints f a, ElemConstraints f b, ElemConstraints f c, ElemConstraints f d)
    => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
  zipWith4
    :: (ElemConstraints f a, ElemConstraints f b, ElemConstraints f c, ElemConstraints f d, ElemConstraints f e)
    => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e

instance Zippable Vector where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith  = V.zipWith
  zipWith3 = V.zipWith3
  zipWith4 = V.zipWith4

instance Zippable [] where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith  = L.zipWith
  zipWith3 = L.zipWith3
  zipWith4 = L.zipWith4

instance Zippable U.Vector where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith  = U.zipWith
  zipWith3 = U.zipWith3
  zipWith4 = U.zipWith4

instance Zippable S.Vector where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith  = S.zipWith
  zipWith3 = S.zipWith3
  zipWith4 = S.zipWith4
