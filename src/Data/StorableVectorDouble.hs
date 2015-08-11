----------------------------------------------------------------------------
-- |
-- Module      :  Data.StorableVectorDouble
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

module Data.StorableVectorDouble
  ( StorableVectorDouble(..)
  , concat
  , concatMap
  , takeBy
  , fromList
  , backpermute
  , unsafeBackpermute
  , unsafeWith
  )
where

import Prelude hiding (concat, concatMap, zipWith, zipWith3)
import Control.DeepSeq
import qualified Data.Vector.Storable as S
import Foreign (Ptr)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.Aligned.Double
import Data.ConstrainedConvert (Convert)
import qualified Data.ConstrainedConvert as Conv
import Data.ConstrainedFunctor
import Data.VectClass (Vect, (.+.))
import qualified Data.VectClass as VC
import Data.Zippable
import Util hiding (takeBy)

newtype StorableVectorDouble a = StorableVectorDouble
  { getStorableVectorDouble :: S.Vector AlignedDouble }
  deriving (Show, Eq, Ord, NFData)

instance Pretty (StorableVectorDouble a) where
  pretty = pretty . S.toList . getStorableVectorDouble

instance ConstrainedFunctor IsAlignedDoubleConstraint StorableVectorDouble where
  {-# INLINABLE cfmap #-}
  cfmap f = StorableVectorDouble . S.map f . getStorableVectorDouble

instance Zippable IsAlignedDoubleConstraint StorableVectorDouble where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith f (StorableVectorDouble xs) (StorableVectorDouble ys) = StorableVectorDouble $ S.zipWith f xs ys
  zipWith3 f (StorableVectorDouble xs) (StorableVectorDouble ys) (StorableVectorDouble zs) = StorableVectorDouble $ S.zipWith3 f xs ys zs
  zipWith4 f (StorableVectorDouble xs) (StorableVectorDouble ys) (StorableVectorDouble zs) (StorableVectorDouble ws) = StorableVectorDouble $ S.zipWith4 f xs ys zs ws

instance Convert IsAlignedDoubleConstraint StorableConstraint StorableVectorDouble S.Vector where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = getStorableVectorDouble
  convertFrom = StorableVectorDouble

instance Vect IsAlignedDoubleConstraint StorableVectorDouble where
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
  fromList        = StorableVectorDouble . S.fromList
  toList          = S.toList . getStorableVectorDouble
  replicate n     = StorableVectorDouble . S.replicate n
  map f           = StorableVectorDouble . S.map f . getStorableVectorDouble
  sum             = S.sum . getStorableVectorDouble
  (.+.)           = zipWith (+!)
  monoFoldr f acc = S.foldr f acc . getStorableVectorDouble
  foldr1 f        = S.foldr1 f . getStorableVectorDouble
  empty           = StorableVectorDouble S.empty
  reverse         = StorableVectorDouble . S.reverse . getStorableVectorDouble
  length          = S.length . getStorableVectorDouble
  replicateM n    = fmap StorableVectorDouble . S.replicateM n
  dot (StorableVectorDouble xs) (StorableVectorDouble ys) = S.sum $ S.zipWith (*!) xs ys

{-# INLINABLE concat #-}
concat
  :: (ElemConstraints IsAlignedDoubleConstraint a, ElemConstraints IsAlignedDoubleConstraint b)
  => [StorableVectorDouble a]
  -> StorableVectorDouble a
concat = StorableVectorDouble . S.concat . map getStorableVectorDouble

{-# INLINABLE concatMap #-}
concatMap
  :: (ElemConstraints IsAlignedDoubleConstraint a, ElemConstraints IsAlignedDoubleConstraint b)
  => (a -> StorableVectorDouble b)
  -> StorableVectorDouble a
  -> StorableVectorDouble b
concatMap f = StorableVectorDouble . S.concatMap (getStorableVectorDouble . f) . getStorableVectorDouble

{-# INLINABLE takeBy #-}
takeBy
  :: (ElemConstraints IsAlignedDoubleConstraint a)
  => Int
  -> Int
  -> StorableVectorDouble a
  -> [StorableVectorDouble a]
takeBy rows cols (StorableVectorDouble vs) =
  map (\r -> StorableVectorDouble $ S.unsafeSlice {-S.slice-} (r *! cols) cols vs) [0..rows -! 1]

{-# INLINABLE fromList #-}
fromList
  :: (ElemConstraints IsAlignedDoubleConstraint a)
  => [a]
  -> StorableVectorDouble a
fromList = StorableVectorDouble . S.fromList

{-# INLINABLE backpermute #-}
backpermute
  :: (ElemConstraints IsAlignedDoubleConstraint a)
  => StorableVectorDouble a
  -> S.Vector Int
  -> StorableVectorDouble a
backpermute (StorableVectorDouble xs) = StorableVectorDouble . S.backpermute xs

{-# INLINABLE unsafeBackpermute #-}
unsafeBackpermute
  :: (ElemConstraints IsAlignedDoubleConstraint a)
  => StorableVectorDouble a
  -> S.Vector Int
  -> StorableVectorDouble a
unsafeBackpermute (StorableVectorDouble xs) = StorableVectorDouble . S.unsafeBackpermute xs

{-# INLINABLE unsafeWith #-}
unsafeWith
  :: (ElemConstraints IsAlignedDoubleConstraint a)
  => StorableVectorDouble a
  -> (Ptr a -> IO b)
  -> IO b
unsafeWith (StorableVectorDouble xs) f = S.unsafeWith xs f
