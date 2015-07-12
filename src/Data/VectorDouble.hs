----------------------------------------------------------------------------
-- |
-- Module      :  Data.VectorDouble
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.VectorDouble
  ( VectorDouble
  , concatMap
  , takeBy
  , fromList
  , backpermute
  , unsafeBackpermute
  )
where

import Prelude hiding (concatMap, zipWith, zipWith3)
import Control.DeepSeq
import qualified Data.Vector.Unboxed as U
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.VectClass (Vect, (.+.))
import qualified Data.VectClass as VC
import Util
import Util.ConstrainedFunctor
import Util.Zippable

newtype VectorDouble a = VectorDouble { getVectorDouble :: U.Vector Double }
  deriving (NFData)

instance Pretty (VectorDouble a) where
  pretty = pretty . U.toList . getVectorDouble

instance ConstrainedFunctor IsDoubleConstraint VectorDouble where
  {-# INLINABLE cfmap #-}
  cfmap f = VectorDouble . U.map f . getVectorDouble

instance Zippable IsDoubleConstraint VectorDouble where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith f (VectorDouble xs) (VectorDouble ys) = VectorDouble $ U.zipWith f xs ys
  zipWith3 f (VectorDouble xs) (VectorDouble ys) (VectorDouble zs) = VectorDouble $ U.zipWith3 f xs ys zs
  zipWith4 f (VectorDouble xs) (VectorDouble ys) (VectorDouble zs) (VectorDouble ws) = VectorDouble $ U.zipWith4 f xs ys zs ws

instance Vect IsDoubleConstraint VectorDouble where
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
  fromList           = VectorDouble . U.fromList
  toList             = U.toList . getVectorDouble
  replicate n        = VectorDouble . U.replicate n
  map f              = VectorDouble . U.map f . getVectorDouble
  sum                = U.sum . getVectorDouble
  (.+.)              = zipWith (+!)
  monoFoldr f acc    = U.foldr f acc . getVectorDouble
  foldr1 f           = U.foldr1 f . getVectorDouble
  empty              = VectorDouble U.empty
  reverse            = VectorDouble . U.reverse . getVectorDouble
  length             = U.length . getVectorDouble
  replicateM n       = fmap VectorDouble . U.replicateM n

{-# INLINABLE concatMap #-}
concatMap
  :: (ElemConstraints IsDoubleConstraint a, ElemConstraints IsDoubleConstraint b)
  => (a -> VectorDouble b)
  -> VectorDouble a
  -> VectorDouble b
concatMap f = VectorDouble . U.concatMap (getVectorDouble . f) . getVectorDouble

{-# INLINABLE takeBy #-}
takeBy
  :: (ElemConstraints IsDoubleConstraint a)
  => Int
  -> Int
  -> VectorDouble a
  -> [VectorDouble a]
takeBy rows cols (VectorDouble vs) =
  map (\r -> VectorDouble $ {-U.unsafeSlice-} U.slice (r *! cols) cols vs) [0..rows -! 1]

{-# INLINABLE fromList #-}
fromList
  :: (ElemConstraints IsDoubleConstraint a)
  => [a]
  -> VectorDouble a
fromList = VectorDouble . U.fromList

{-# INLINABLE backpermute #-}
backpermute
  :: (ElemConstraints IsDoubleConstraint a)
  => VectorDouble a
  -> U.Vector Int
  -> VectorDouble a
backpermute (VectorDouble xs) = VectorDouble . U.backpermute xs

{-# INLINABLE unsafeBackpermute #-}
unsafeBackpermute
  :: (ElemConstraints IsDoubleConstraint a)
  => VectorDouble a
  -> U.Vector Int
  -> VectorDouble a
unsafeBackpermute (VectorDouble xs) = VectorDouble . U.unsafeBackpermute xs
