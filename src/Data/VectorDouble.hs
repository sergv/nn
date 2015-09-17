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
  , concat
  , concatMap
  , takeBy
  , fromList
  , backpermute
  , unsafeBackpermute
  )
where

import Prelude hiding (concat, concatMap, zipWith, zipWith3)
import Control.DeepSeq
import qualified Data.Vector.Unboxed as U
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.ConstrainedFunctor
import Data.VectClass (Vect, (.+.))
import qualified Data.VectClass as VC
import Data.Zippable
import Util hiding (takeBy)

newtype VectorDouble a = VectorDouble { getVectorDouble :: U.Vector Double }
  deriving (Show, Eq, Ord, NFData)

instance Pretty (VectorDouble a) where
  pretty = pretty . U.toList . getVectorDouble

instance ConstrainedFunctor VectorDouble where
  type ElemConstraints VectorDouble = (~) Double
  {-# INLINABLE cfmap #-}
  cfmap f = VectorDouble . U.map f . getVectorDouble

instance Zippable VectorDouble where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith f (VectorDouble xs) (VectorDouble ys) = VectorDouble $ U.zipWith f xs ys
  zipWith3 f (VectorDouble xs) (VectorDouble ys) (VectorDouble zs) = VectorDouble $ U.zipWith3 f xs ys zs
  zipWith4 f (VectorDouble xs) (VectorDouble ys) (VectorDouble zs) (VectorDouble ws) = VectorDouble $ U.zipWith4 f xs ys zs ws

instance Vect VectorDouble where
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
  fromList           = VectorDouble . U.fromList
  toList             = U.toList . getVectorDouble
  singleton          = VectorDouble . U.singleton
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

{-# INLINABLE concat #-}
concat
  :: (ElemConstraints VectorDouble a, ElemConstraints VectorDouble b)
  => [VectorDouble a]
  -> VectorDouble a
concat = VectorDouble . U.concat . map getVectorDouble

{-# INLINABLE concatMap #-}
concatMap
  :: (ElemConstraints VectorDouble a, ElemConstraints VectorDouble b)
  => (a -> VectorDouble b)
  -> VectorDouble a
  -> VectorDouble b
concatMap f = VectorDouble . U.concatMap (getVectorDouble . f) . getVectorDouble

{-# INLINABLE takeBy #-}
takeBy
  :: (ElemConstraints VectorDouble a)
  => Int
  -> Int
  -> VectorDouble a
  -> [VectorDouble a]
takeBy rows cols (VectorDouble vs) =
  map (\r -> VectorDouble $ U.unsafeSlice {-U.slice-} (r *! cols) cols vs) [0..rows -! 1]

{-# INLINABLE fromList #-}
fromList
  :: (ElemConstraints VectorDouble a)
  => [a]
  -> VectorDouble a
fromList = VectorDouble . U.fromList

{-# INLINABLE backpermute #-}
backpermute
  :: (ElemConstraints VectorDouble a)
  => VectorDouble a
  -> U.Vector Int
  -> VectorDouble a
backpermute (VectorDouble xs) = VectorDouble . U.backpermute xs

{-# INLINABLE unsafeBackpermute #-}
unsafeBackpermute
  :: (ElemConstraints VectorDouble a)
  => VectorDouble a
  -> U.Vector Int
  -> VectorDouble a
unsafeBackpermute (VectorDouble xs) = VectorDouble . U.unsafeBackpermute xs
