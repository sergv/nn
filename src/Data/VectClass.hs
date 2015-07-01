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

{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Data.VectClass where

import qualified Control.Monad as L (replicateM)
import qualified Data.List as L
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Prelude (Num(..), Eq(..), ($), Int, Monad, error, otherwise, (.))
import qualified Prelude as P

import Util
import Util.ConstrainedFunctor
import Util.Zippable

class (Zippable k v) => Vect k v | v -> k where
  fromList   :: (ElemConstraints k a) => [a] -> v a
  toList     :: (ElemConstraints k a) => v a -> [a]
  replicate  :: (ElemConstraints k a) => Int -> a -> v a
  map        :: (ElemConstraints k a, ElemConstraints k b) => (a -> b) -> v a -> v b
  sum        :: (ElemConstraints k a, Num a) => v a -> a
  (.+.)      :: (ElemConstraints k a, Num a) => v a -> v a -> v a
  -- foldr      :: (ElemConstraints k a, ElemConstraints k b) => (a -> b -> b) -> b -> v a -> b
  monoFoldr  :: (ElemConstraints k a, ElemConstraints k b) => (a -> b -> b) -> b -> v a -> b
  foldr1     :: (ElemConstraints k a) => (a -> a -> a) -> v a -> a
  empty      :: (ElemConstraints k a) => v a
  reverse    :: (ElemConstraints k a) => v a -> v a
  length     :: (ElemConstraints k a) => v a -> Int
  replicateM :: (ElemConstraints k a, Monad m) => Int -> m a -> m (v a)
  {-# INLINABLE dot #-}
  dot :: (ElemConstraints k a, Num a) => v a -> v a -> a
  dot xs ys
    | length xs /= length ys =
      error "cannot take dot products for vectors of different length"
    | otherwise =
      monoFoldr (+) 0 $ zipWith (*) xs ys

instance Vect NoConstraints Vector where
  {-# INLINABLE fromList   #-}
  {-# INLINABLE toList     #-}
  {-# INLINABLE replicate  #-}
  {-# INLINABLE map        #-}
  {-# INLINABLE sum        #-}
  {-# INLINABLE (.+.)      #-}
  {-# INLINABLE monoFoldr  #-}
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
  monoFoldr  = V.foldr
  foldr1     = V.foldr1
  empty      = V.empty
  reverse    = V.reverse
  length     = V.length
  replicateM = V.replicateM

instance Vect NoConstraints [] where
  {-# INLINABLE fromList   #-}
  {-# INLINABLE toList     #-}
  {-# INLINABLE replicate  #-}
  {-# INLINABLE map        #-}
  {-# INLINABLE sum        #-}
  {-# INLINABLE (.+.)      #-}
  {-# INLINABLE monoFoldr  #-}
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
  monoFoldr  = L.foldr
  foldr1     = L.foldr1
  empty      = []
  reverse    = L.reverse
  length     = L.length
  replicateM = L.replicateM

instance Vect UnboxConstraint U.Vector where
  {-# INLINABLE fromList   #-}
  {-# INLINABLE toList     #-}
  {-# INLINABLE replicate  #-}
  {-# INLINABLE map        #-}
  {-# INLINABLE sum        #-}
  {-# INLINABLE (.+.)      #-}
  {-# INLINABLE monoFoldr  #-}
  {-# INLINABLE foldr1     #-}
  {-# INLINABLE empty      #-}
  {-# INLINABLE reverse    #-}
  {-# INLINABLE length     #-}
  {-# INLINABLE replicateM #-}
  fromList   = U.fromList
  toList     = U.toList
  replicate  = U.replicate
  map        = U.map
  sum        = U.sum
  (.+.)      = zipWith (+!)
  monoFoldr  = U.foldr
  foldr1     = U.foldr1
  empty      = U.empty
  reverse    = U.reverse
  length     = U.length
  replicateM = U.replicateM

{-# INLINABLE normL2Square #-}
normL2Square :: (Vect k v, Num a, ElemConstraints k a) => v a -> a
normL2Square = sum . map (\x -> x *! x)
