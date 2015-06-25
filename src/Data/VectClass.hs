----------------------------------------------------------------------------
-- |
-- Module      :  Data.VectClass
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.VectClass where

import qualified Control.Monad as L (replicateM)
import qualified Data.List as L
import Data.Traversable (Traversable)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude (Num(..), Floating(..), Eq(..), ($), Int, Monad, error, otherwise)
import qualified Prelude as P -- hiding (map, zipWith, )

class (Traversable v) => Vect v where
  fromList   :: [a] -> v a
  toList     :: v a -> [a]
  map        :: (a -> b) -> v a -> v b
  sum        :: (Num a) => v a -> a
  zipWith    :: (a -> b -> c) -> v a -> v b -> v c
  zipWith3   :: (a -> b -> c -> d) -> v a -> v b -> v c -> v d
  zipWith4   :: (a -> b -> c -> d -> e) -> v a -> v b -> v c -> v d -> v e
  foldr      :: (a -> b -> b) -> b -> v a -> b
  foldr1     :: (a -> a -> a) -> v a -> a
  empty      :: v a
  cons       :: a -> v a -> v a
  head       :: v a -> a
  tail       :: v a -> v a
  reverse    :: v a -> v a
  length     :: v a -> Int
  replicateM :: (Monad m) => Int -> m a -> m (v a)
  dot        :: (Floating a) => v a -> v a -> a
  dot xs ys
    | length xs /= length ys =
      error "cannot take dot products for vectors of different length"
    | otherwise =
      foldr (+) 0 $ zipWith (*) xs ys


instance Vect Vector where
  {-# INLINABLE fromList #-}
  fromList   = V.fromList
  {-# INLINABLE toList #-}
  toList     = V.toList
  {-# INLINABLE map #-}
  map        = V.map
  {-# INLINABLE sum #-}
  sum        = V.sum
  {-# INLINABLE zipWith #-}
  zipWith    = V.zipWith
  {-# INLINABLE zipWith3 #-}
  zipWith3   = V.zipWith3
  {-# INLINABLE zipWith4 #-}
  zipWith4   = V.zipWith4
  {-# INLINABLE foldr #-}
  foldr      = V.foldr
  {-# INLINABLE foldr1 #-}
  foldr1     = V.foldr1
  {-# INLINABLE empty #-}
  empty      = V.empty
  {-# INLINABLE cons #-}
  cons       = V.cons
  {-# INLINABLE head #-}
  head       = V.head
  {-# INLINABLE tail #-}
  tail       = V.tail
  {-# INLINABLE reverse #-}
  reverse    = V.reverse
  {-# INLINABLE length #-}
  length     = V.length
  {-# INLINABLE replicateM #-}
  replicateM = V.replicateM

instance Vect [] where
  {-# INLINABLE fromList #-}
  fromList   = P.id
  {-# INLINABLE toList #-}
  toList     = P.id
  {-# INLINABLE map #-}
  map        = L.map
  {-# INLINABLE sum #-}
  sum        = L.sum
  {-# INLINABLE zipWith #-}
  zipWith    = L.zipWith
  {-# INLINABLE zipWith3 #-}
  zipWith3   = L.zipWith3
  {-# INLINABLE zipWith4 #-}
  zipWith4   = L.zipWith4
  {-# INLINABLE foldr #-}
  foldr      = L.foldr
  {-# INLINABLE foldr1 #-}
  foldr1     = L.foldr1
  {-# INLINABLE empty #-}
  empty      = []
  {-# INLINABLE cons #-}
  cons       = (:)
  {-# INLINABLE head #-}
  head       = L.head
  {-# INLINABLE tail #-}
  tail       = L.tail
  {-# INLINABLE reverse #-}
  reverse    = L.reverse
  {-# INLINABLE length #-}
  length     = L.length
  {-# INLINABLE replicateM #-}
  replicateM = L.replicateM
