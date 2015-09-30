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
{-# LANGUAGE DefaultSignatures      #-}
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
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import Prelude (Num(..), Eq(..), ($), Int, Monad, error, otherwise)
import qualified Prelude as P

import Data.ConstrainedFunctor
import Data.Zippable
import Util

class (Zippable v) => Vect v where
  {-# INLINABLE addScaled #-}
  {-# INLINABLE dot       #-}
  fromList   :: (ElemConstraints v a) => [a] -> v a
  toList     :: (ElemConstraints v a) => v a -> [a]
  singleton  :: (ElemConstraints v a) => a -> v a
  replicate  :: (ElemConstraints v a) => Int -> a -> v a
  map        :: (ElemConstraints v a, ElemConstraints v b) => (a -> b) -> v a -> v b
  sum        :: (ElemConstraints v a, Num a) => v a -> a
  (.+.)      :: (ElemConstraints v a, Num a) => v a -> v a -> v a
  addScaled  :: (ElemConstraints v a, Num a) => v a -> a -> v a -> v a
  default addScaled
    :: (ElemConstraints v a, Num a, ConstrainedFunctor v)
    => v a -> a -> v a -> v a
  addScaled xs c ys = xs .+. cfmap (*! c) ys
  -- foldr      :: (ElemConstraints v a, ElemConstraints v b) => (a -> b -> b) -> b -> v a -> b
  monoFoldr  :: (ElemConstraints v a, ElemConstraints v b) => (a -> b -> b) -> b -> v a -> b
  foldr1     :: (ElemConstraints v a) => (a -> a -> a) -> v a -> a
  empty      :: (ElemConstraints v a) => v a
  reverse    :: (ElemConstraints v a) => v a -> v a
  length     :: (ElemConstraints v a) => v a -> Int
  replicateM :: (ElemConstraints v a, Monad m) => Int -> m a -> m (v a)
  dot :: (ElemConstraints v a, Num a) => v a -> v a -> a
  dot xs ys
    | length xs /= length ys =
      error "cannot take dot products for vectors of different length"
    | otherwise =
      monoFoldr (+) 0 $ zipWith (*) xs ys

class (ConstrainedFunctor v) => TransposableVector v where
  transpose :: (ElemConstraints v a, ElemConstraints v (v a)) => v (v a) -> v (v a)

instance Vect Vector where
  {-# INLINABLE fromList   #-}
  {-# INLINABLE toList     #-}
  {-# INLINABLE singleton  #-}
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
  singleton  = V.singleton
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
  dot xs ys  = V.sum $ V.zipWith (*!) xs ys

instance Vect [] where
  {-# INLINABLE fromList   #-}
  {-# INLINABLE toList     #-}
  {-# INLINABLE singleton  #-}
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
  singleton  = (:[])
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
  dot xs ys  = L.sum $ L.zipWith (*!) xs ys

instance Vect U.Vector where
  {-# INLINABLE fromList   #-}
  {-# INLINABLE toList     #-}
  {-# INLINABLE singleton  #-}
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
  singleton  = U.singleton
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
  dot xs ys  = U.sum $ U.zipWith (*!) xs ys

instance Vect S.Vector where
  {-# INLINABLE fromList   #-}
  {-# INLINABLE toList     #-}
  {-# INLINABLE singleton  #-}
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
  fromList   = S.fromList
  toList     = S.toList
  singleton  = S.singleton
  replicate  = S.replicate
  map        = S.map
  sum        = S.sum
  (.+.)      = zipWith (+!)
  monoFoldr  = S.foldr
  foldr1     = S.foldr1
  empty      = S.empty
  reverse    = S.reverse
  length     = S.length
  replicateM = S.replicateM
  dot xs ys  = S.sum $ S.zipWith (*!) xs ys

instance TransposableVector Vector where
  transpose xss
    | V.null xss = xss
    | otherwise  =
      V.fromList $ map (\n -> V.map (V.! n) xss) [0..V.length (V.head xss) - 1]

instance TransposableVector [] where
  transpose = L.transpose

{-# INLINABLE normL2Square #-}
normL2Square :: (Vect v, Num a, ElemConstraints v a) => v a -> a
normL2Square xs = dot xs xs
