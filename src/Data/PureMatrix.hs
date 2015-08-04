----------------------------------------------------------------------------
-- |
-- Module      :  Data.PureMatrix
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.PureMatrix (PureMatrix(..)) where

import Prelude hiding (zipWith, zipWith3, map)
import Control.DeepSeq
import qualified Data.List as L
import Data.Monoid
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.ConstrainedConvert (Convert)
import qualified Data.ConstrainedConvert as Conv
import Data.MatrixClass
import Data.VectClass (Vect, TransposableVector)
import qualified Data.VectClass as VC

import Data.ConstrainedFunctor
import Data.Zippable
import Util

data PureMatrix v a = PureMatrix
  { pmRows    :: {-# UNPACK #-} !Int
  , pmColumns :: {-# UNPACK #-} !Int
  , pmData    :: !(v (v a))
  }
  deriving (Functor, Foldable, Traversable)

deriving instance (Show (v (v a))) => Show (PureMatrix v a)
deriving instance (Eq (v (v a))) => Eq (PureMatrix v a)
deriving instance (Ord (v (v a))) => Ord (PureMatrix v a)

instance forall k v a. (Vect k v, ElemConstraints k a, ElemConstraints k (v a), Pretty a) => Pretty (PureMatrix v a) where
  pretty (PureMatrix rows cols xss) =
    "Matrix " <> PP.int rows <> "x" <> PP.int cols PP.<$>
    PP.vsep (L.map showRow $ VC.toList xss)
    where
      showRow :: v a -> Doc
      showRow = PP.hcat . PP.punctuate PP.comma . L.map pretty . VC.toList

instance (NFData (v (v a))) => NFData (PureMatrix v a) where
  rnf (PureMatrix rows cols xss) = rnf rows `seq` rnf cols `seq` rnf xss

instance (Functor v) => ConstrainedFunctor NoConstraints (PureMatrix v) where
  {-# INLINABLE cfmap #-}
  cfmap = fmap

instance (Functor v, Zippable NoConstraints v) => Zippable NoConstraints (PureMatrix v) where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith f (PureMatrix xRows xCols xss) (PureMatrix yRows yCols yss)
    | xRows == yRows && xCols == yCols =
      PureMatrix xRows xCols $ zipWith (zipWith f) xss yss
    | otherwise = error "PureMatrix.zipWith: cannot zip matrices of different shapes"
  zipWith3 f (PureMatrix xRows xCols xss) (PureMatrix yRows yCols yss) (PureMatrix zRows zCols zss)
    | xRows == yRows && yRows == zRows && xCols == yCols && yCols == zCols =
      PureMatrix xRows xCols $ zipWith3 (zipWith3 f) xss yss zss
    | otherwise = error "PureMatrix.zipWith3: cannot zip matrices of different shapes"
  zipWith4 f (PureMatrix xRows xCols xss) (PureMatrix yRows yCols yss) (PureMatrix zRows zCols zss) (PureMatrix wRows wCols wss)
    | xRows == yRows && yRows == zRows && zRows == wRows && xCols == yCols && yCols == zCols && zCols == wCols =
      PureMatrix xRows xCols $ zipWith4 (zipWith4 f) xss yss zss wss
    | otherwise = error "PureMatrix.zipWith4: cannot zip matrices of different shapes"

instance Convert NoConstraints NoConstraints (PureMatrix v) (PureMatrix v) where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = id
  convertFrom = id

instance (ConstrainedFunctor NoConstraints v, Vect NoConstraints v, TransposableVector NoConstraints v) => Matrix NoConstraints (PureMatrix v) v where
  {-# INLINABLE rows         #-}
  {-# INLINABLE columns      #-}
  {-# INLINABLE outerProduct #-}
  {-# INLINABLE vecMulRight  #-}
  {-# INLINABLE transpose    #-}
  fromList [] =
    error "PureMatrix.fromList: cannot create PureMatrix from empty list of rows"
  fromList wss@(ws:_)
    | columns > 0 && all (== columns) (L.map length wss) =
      PureMatrix
        { pmRows    = rows
        , pmColumns = columns
        , pmData    = VC.fromList $ L.map VC.fromList wss
        }
    | otherwise =
      error $ "PureMatrix.fromList: cannot create PureMatrix from list " ++ show wss
    where
      rows    = length wss
      columns = length ws
  toList (PureMatrix _ _ xss) = L.map VC.toList $ VC.toList xss
  rows    = pmRows
  columns = pmColumns
  replicateM rows cols action =
    PureMatrix rows cols <$> VC.replicateM rows (VC.replicateM cols action)
  outerProduct columnVec rowVec =
    PureMatrix (VC.length columnVec) (VC.length rowVec) $
    VC.map (\c -> cfmap (c *!) rowVec) columnVec
  vecMulRight (PureMatrix _ _ xss) ys = cfmap (\xs -> VC.dot xs ys) xss
  transpose (PureMatrix rows cols xss) =
    PureMatrix cols rows $ VC.transpose xss

-- instance Matrix (PureMatrix Vector) Vector where
--   map f (PureMatrix rows cols xss) = PureMatrix rows cols $ fmap (fmap f) xss
--   zipWith f (PureMatrix xss) (PureMatrix yss) = PureMatrix $ V.zipWith (V.zipWith f) xss yss

