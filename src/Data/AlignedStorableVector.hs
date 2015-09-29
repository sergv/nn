----------------------------------------------------------------------------
-- |
-- Module      :  Data.AlignedStorableVector
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
{-# LANGUAGE UndecidableInstances       #-}

module Data.AlignedStorableVector
  ( AlignedStorableVector(..)
  , concat
  , concatMap
  , takeBy
  , fromList
  , backpermute
  , unsafeBackpermute
  , unsafeWith
  )
where

import Prelude hiding (concat, concatMap, zipWith, zipWith3, exp)
import Control.DeepSeq
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Foreign (Ptr, Storable)
import Text.PrettyPrint.Leijen.Text (Pretty(..))
import System.IO.Unsafe

import qualified Data.Aligned as Aligned
import Data.ConstrainedConvert (Convert)
import qualified Data.ConstrainedConvert as Conv
import Data.ConstrainedFunctor
import Data.Nonlinearity
import Data.SpecialisedFunction
import Data.VectClass (Vect, (.+.))
import qualified Data.VectClass as VC
import Data.Zippable
import Util hiding (takeBy)

newtype AlignedStorableVector a = AlignedStorableVector
  { getAlignedStorableVector :: S.Vector a }
  deriving (Show, Eq, Ord, NFData)

instance (Pretty a, Storable a) => Pretty (AlignedStorableVector a) where
  pretty = pretty . S.toList . getAlignedStorableVector

instance ConstrainedFunctor AlignedStorableVector where
  type ElemConstraints AlignedStorableVector = Aligned.Aligned
  {-# INLINABLE cfmap #-}
  cfmap f = AlignedStorableVector . S.map f . getAlignedStorableVector

instance Zippable AlignedStorableVector where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith f (AlignedStorableVector xs) (AlignedStorableVector ys) = AlignedStorableVector $ S.zipWith f xs ys
  zipWith3 f (AlignedStorableVector xs) (AlignedStorableVector ys) (AlignedStorableVector zs) = AlignedStorableVector $ S.zipWith3 f xs ys zs
  zipWith4 f (AlignedStorableVector xs) (AlignedStorableVector ys) (AlignedStorableVector zs) (AlignedStorableVector ws) = AlignedStorableVector $ S.zipWith4 f xs ys zs ws

instance Convert AlignedStorableVector S.Vector where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = getAlignedStorableVector
  convertFrom = AlignedStorableVector

instance Vect AlignedStorableVector where
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
  {-# INLINABLE dot        #-}
  fromList        = AlignedStorableVector . S.fromList
  toList          = S.toList . getAlignedStorableVector
  singleton       = AlignedStorableVector . S.singleton
  replicate n     = AlignedStorableVector . S.replicate n
  map f           = AlignedStorableVector . S.map f . getAlignedStorableVector
  sum             = S.sum . getAlignedStorableVector
  (.+.) (AlignedStorableVector xs) (AlignedStorableVector ys) =
    -- | S.length xs /= S.length ys =
    --   error "AlignedStorableVector: arguments of different length to .+."
    -- | otherwise =
    unsafePerformIO $ do
      result <- SM.unsafeNew n
      S.unsafeWith xs $ \xsPtr ->
        S.unsafeWith ys $ \ysPtr ->
          SM.unsafeWith result $ \resultPtr ->
            Aligned.addVectors
              n
              xsPtr
              ysPtr
              resultPtr
      AlignedStorableVector <$> S.freeze result
    where
      n  = S.length xs
  addScaled (AlignedStorableVector xs) c (AlignedStorableVector ys) =
    -- | S.length xs /= S.length ys =
    --   error "AlignedStorableVector: arguments of different length to .+."
    -- | otherwise =
    unsafePerformIO $ do
      result <- SM.unsafeNew n
      S.unsafeWith xs $ \xsPtr ->
        S.unsafeWith ys $ \ysPtr ->
          SM.unsafeWith result $ \resultPtr ->
            Aligned.addVectorsScaled n xsPtr c ysPtr resultPtr
      AlignedStorableVector <$> S.freeze result
    where
      n  = S.length xs
  monoFoldr f acc = S.foldr f acc . getAlignedStorableVector
  foldr1 f        = S.foldr1 f . getAlignedStorableVector
  empty           = AlignedStorableVector S.empty
  reverse         = AlignedStorableVector . S.reverse . getAlignedStorableVector
  length          = S.length . getAlignedStorableVector
  replicateM n    = fmap AlignedStorableVector . S.replicateM n
  dot (AlignedStorableVector xs) (AlignedStorableVector ys) =
    -- | S.length xs /= S.length ys =
    --   error "AlignedStorableVector: arguments of different length to dot"
    -- | otherwise =
    unsafePerformIO $
      S.unsafeWith xs $ \xsPtr ->
        S.unsafeWith ys $ \ysPtr ->
          Aligned.dotProduct (S.length xs) xsPtr ysPtr

{-# INLINABLE concat #-}
concat
  :: (ElemConstraints AlignedStorableVector a)
  => [AlignedStorableVector a]
  -> AlignedStorableVector a
concat = AlignedStorableVector . S.concat . map getAlignedStorableVector

{-# INLINABLE concatMap #-}
concatMap
  :: (ElemConstraints AlignedStorableVector a, ElemConstraints AlignedStorableVector b)
  => (a -> AlignedStorableVector b)
  -> AlignedStorableVector a
  -> AlignedStorableVector b
concatMap f = AlignedStorableVector . S.concatMap (getAlignedStorableVector . f) . getAlignedStorableVector

{-# INLINABLE takeBy #-}
takeBy
  :: (ElemConstraints AlignedStorableVector a)
  => Int
  -> Int
  -> AlignedStorableVector a
  -> [AlignedStorableVector a]
takeBy rows cols (AlignedStorableVector vs) =
  map (\r -> AlignedStorableVector $ S.unsafeSlice {-S.slice-} (r *! cols) cols vs) [0..rows -! 1]

{-# INLINABLE fromList #-}
fromList
  :: (ElemConstraints AlignedStorableVector a)
  => [a]
  -> AlignedStorableVector a
fromList = AlignedStorableVector . S.fromList

{-# INLINABLE backpermute #-}
backpermute
  :: (ElemConstraints AlignedStorableVector a)
  => AlignedStorableVector a
  -> S.Vector Int
  -> AlignedStorableVector a
backpermute (AlignedStorableVector xs) = AlignedStorableVector . S.backpermute xs

{-# INLINABLE unsafeBackpermute #-}
unsafeBackpermute
  :: (ElemConstraints AlignedStorableVector a)
  => AlignedStorableVector a
  -> S.Vector Int
  -> AlignedStorableVector a
unsafeBackpermute (AlignedStorableVector xs) = AlignedStorableVector . S.unsafeBackpermute xs

{-# INLINABLE unsafeWith #-}
unsafeWith
  :: (ElemConstraints AlignedStorableVector a)
  => AlignedStorableVector a
  -> (Ptr a -> IO b)
  -> IO b
unsafeWith (AlignedStorableVector xs) f = S.unsafeWith xs f
