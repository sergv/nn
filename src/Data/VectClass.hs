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
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude (Num(..), Eq(..), ($), Int, Monad, error, otherwise, (.))
import qualified Prelude as P

import Util
import Util.Zippable

class (Zippable v) => Vect v where
  fromList   :: [a] -> v a
  toList     :: v a -> [a]
  replicate  :: Int -> a -> v a
  map        :: (a -> b) -> v a -> v b
  sum        :: (Num a) => v a -> a
  (.+.)      :: (Num a) => v a -> v a -> v a
  foldr      :: (a -> b -> b) -> b -> v a -> b
  foldr1     :: (a -> a -> a) -> v a -> a
  empty      :: v a
  reverse    :: v a -> v a
  length     :: v a -> Int
  replicateM :: (Monad m) => Int -> m a -> m (v a)
  {-# INLINABLE dot #-}
  dot :: (Num a) => v a -> v a -> a
  dot xs ys
    | length xs /= length ys =
      error "cannot take dot products for vectors of different length"
    | otherwise =
      foldr (+) 0 $ zipWith (*) xs ys

instance Vect Vector where
  {-# INLINABLE fromList   #-}
  {-# INLINABLE toList     #-}
  {-# INLINABLE replicate  #-}
  {-# INLINABLE map        #-}
  {-# INLINABLE sum        #-}
  {-# INLINABLE (.+.)      #-}
  {-# INLINABLE foldr      #-}
  {-# INLINABLE foldr1     #-}
  {-# INLINABLE empty      #-}
  {-# INLINABLE reverse    #-}
  {-# INLINABLE length     #-}
  {-# INLINABLE replicateM #-}
  fromList   = V.fromList
  toList     = V.toList
  replicate  = V.replicate
  map        = V.map
  sum        = V.sum
  (.+.)      = zipWith (+!)
  foldr      = V.foldr
  foldr1     = V.foldr1
  empty      = V.empty
  reverse    = V.reverse
  length     = V.length
  replicateM = V.replicateM

instance Vect [] where
  {-# INLINABLE fromList   #-}
  {-# INLINABLE toList     #-}
  {-# INLINABLE replicate  #-}
  {-# INLINABLE map        #-}
  {-# INLINABLE sum        #-}
  {-# INLINABLE (.+.)      #-}
  {-# INLINABLE foldr      #-}
  {-# INLINABLE foldr1     #-}
  {-# INLINABLE empty      #-}
  {-# INLINABLE reverse    #-}
  {-# INLINABLE length     #-}
  {-# INLINABLE replicateM #-}
  fromList   = P.id
  toList     = P.id
  replicate  = L.replicate
  map        = L.map
  sum        = L.sum
  (.+.)      = zipWith (+!)
  foldr      = L.foldr
  foldr1     = L.foldr1
  empty      = []
  reverse    = L.reverse
  length     = L.length
  replicateM = L.replicateM

{-# INLINABLE normL2Square #-}
normL2Square :: (Vect v, Num a) => v a -> a
normL2Square = sum . map (\x -> x * x)
