----------------------------------------------------------------------------
-- |
-- Module      :  Data.StorableMatrixWithTranspose
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.StorableMatrixWithTranspose (StorableMatrixWithTranspose(..), mkMatrixWithTranspose) where

import Prelude hiding (zipWith, zipWith3)
import Control.DeepSeq
import Data.Monoid
import qualified Data.List as L
import qualified Data.Vector.Storable as S
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.ConstrainedIsomorphism (ConstrainedIsomorphism(..))
import Data.ConstrainedFunctor
import Data.MatrixClass
import qualified Data.VectClass as VC
import Data.Zippable
import Util

-- Some benchmarking data for further reference, 10000 examples & 10 iterations
-- benchmarking rprop generic - UnboxMatrix
-- time                 11.54 s    (11.25 s .. NaN s)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 11.21 s    (11.07 s .. 11.32 s)
-- std dev              165.5 ms   (0.0 s .. 185.1 ms)
-- variance introduced by outliers: 19% (moderately inflated)
--
-- benchmarking rprop generic - StorableMatrixWithTranspose
-- time                 9.482 s    (8.817 s .. 10.17 s)
--                      0.999 R²   (0.997 R² .. 1.000 R²)
-- mean                 9.687 s    (9.554 s .. 9.923 s)
-- std dev              204.5 ms   (0.0 s .. 212.9 ms)
-- variance introduced by outliers: 19% (moderately inflated)

data StorableMatrixWithTranspose a = StorableMatrixWithTranspose
  { smtRows    :: {-# UNPACK #-} !Int
  , smtColumns :: {-# UNPACK #-} !Int
  , smtData    :: !(S.Vector a)
  -- | Deliberately left non-strict in order to not pay cost if
  -- no transpose operations were applied.
  , smtDataT   :: S.Vector a
  }
  deriving (Show, Eq, Ord)

unboxedMatrixWithTransposeToList :: (ElemConstraints StorableMatrixWithTranspose a) => StorableMatrixWithTranspose a -> [[a]]
unboxedMatrixWithTransposeToList (StorableMatrixWithTranspose _ cols xs _) = takeBy cols $ VC.toList xs

instance (ElemConstraints StorableMatrixWithTranspose a, Pretty a) => Pretty (StorableMatrixWithTranspose a) where
  pretty um@(StorableMatrixWithTranspose rows cols _ _) =
    "Matrix " <> PP.int rows <> "x" <> PP.int cols <> " (double)" PP.<$>
    PP.vsep (L.map showRow $ unboxedMatrixWithTransposeToList um)
    where
      showRow :: [a] -> Doc
      showRow = PP.hcat . PP.punctuate PP.comma . L.map pretty

instance (ElemConstraints StorableMatrixWithTranspose a) => NFData (StorableMatrixWithTranspose a) where
  rnf (StorableMatrixWithTranspose rows cols xs ys) = rnf rows `seq` rnf cols `seq` rnf xs `seq` rnf ys

instance ConstrainedFunctor StorableMatrixWithTranspose where
  type ElemConstraints StorableMatrixWithTranspose = S.Storable
  {-# INLINABLE cfmap #-}
  cfmap f (StorableMatrixWithTranspose rows cols xs _) =
    mkMatrixWithTranspose rows cols $ cfmap f xs

instance Zippable StorableMatrixWithTranspose where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith f (StorableMatrixWithTranspose xRows xCols xs _) (StorableMatrixWithTranspose yRows yCols ys _)
    | xRows == yRows && xCols == yCols =
      mkMatrixWithTranspose xRows xCols $ zipWith f xs ys
    | otherwise = error "StorableMatrixWithTranspose.zipWith: cannot zip matrices of different shapes"
  zipWith3 f (StorableMatrixWithTranspose xRows xCols xs _) (StorableMatrixWithTranspose yRows yCols ys _) (StorableMatrixWithTranspose zRows zCols zs _)
    | xRows == yRows && yRows == zRows && xCols == yCols && yCols == zCols =
      mkMatrixWithTranspose xRows xCols $ zipWith3 f xs ys zs
    | otherwise = error "StorableMatrixWithTranspose.zipWith3: cannot zip matrices of different shapes"
  zipWith4 f (StorableMatrixWithTranspose xRows xCols xs _) (StorableMatrixWithTranspose yRows yCols ys _) (StorableMatrixWithTranspose zRows zCols zs _) (StorableMatrixWithTranspose wRows wCols ws _)
    | xRows == yRows && yRows == zRows && zRows == wRows && xCols == yCols && yCols == zCols && zCols == wCols =
      mkMatrixWithTranspose xRows xCols $ zipWith4 f xs ys zs ws
    | otherwise = error "StorableMatrixWithTranspose.zipWith4: cannot zip matrices of different shapes"

instance ConstrainedIsomorphism StorableMatrixWithTranspose StorableMatrixWithTranspose where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = id
  convertFrom = id

instance Matrix StorableMatrixWithTranspose S.Vector where
  {-# INLINABLE rows         #-}
  {-# INLINABLE columns      #-}
  {-# INLINABLE outerProduct #-}
  {-# INLINABLE vecMulRight  #-}
  {-# INLINABLE transpose    #-}
  {-# INLINABLE matrixMult   #-}
  {-# INLINABLE (|+|)        #-}
  {-# INLINABLE sumColumns   #-}
  fromList [] =
    error "StorableMatrixWithTranspose.fromList: cannot create PureMatrix from empty list of rows"
  fromList wss@(ws:_)
    | cols /= 0 && all (== cols) (L.map length wss) =
      mkMatrixWithTranspose rows cols matrixData
    | otherwise =
      error $ "StorableMatrixWithTranspose.fromList: cannot create PureMatrix from list " ++ show wss
    where
      rows        = length wss
      cols        = length ws
      matrixData  = VC.fromList $ L.concat wss
  toList (StorableMatrixWithTranspose _ cols xs _) = takeBy cols $ VC.toList xs
  rows    = smtRows
  columns = smtColumns
  replicateM rows cols action =
    mkMatrixWithTranspose rows cols <$> VC.replicateM (rows *! cols) action
  outerProduct columnVec rowVec =
    mkMatrixWithTranspose rows cols matrixData
    where
      rows = VC.length columnVec
      cols = VC.length rowVec
      matrixData = S.concatMap (\c -> cfmap (c *!) rowVec) columnVec
  vecMulRight (StorableMatrixWithTranspose rows cols xs _) ys =
    VC.fromList $ L.map (\zs -> VC.dot zs ys) $ svecTakeBy rows cols xs
  transpose (StorableMatrixWithTranspose rows cols xs xsT) =
    StorableMatrixWithTranspose cols rows xsT xs
  matrixMult (StorableMatrixWithTranspose xRows xCols xs _) (StorableMatrixWithTranspose yRows yCols ys _)
    -- This check is needed for optimization of using VC.foldr1 instead of
    -- VC.monoFoldr. Also it's somewhat meaningless to have matrices with any
    -- dimension equal to zero.
    | xCols == 0     =
      error "StorableMatrix.matrixMult: number of columns for right matrix is zero"
    | xCols /= yRows =
      error $ "StorableMatrix.matrixMult: number of columns for left matrix and " ++
        "rows for right matrix mismatch: xCols = " ++ show xCols ++
        ", yRows = " ++ show yRows
    | otherwise      =
      mkMatrixWithTranspose xRows yCols matrixData
    where
      matrixData = S.concat
                 $ map (\xs -> VC.foldr1 (VC..+.) $ zipWith (\x ys -> cfmap (x *!) ys) xs yss) xss
      xss = map VC.toList $ svecTakeBy xRows xCols xs
      yss = svecTakeBy yRows yCols ys
  (|+|) left@(StorableMatrixWithTranspose xRows xCols xs _) right@(StorableMatrixWithTranspose yRows yCols ys _)
    | xRows /= yRows || xCols /= yCols =
      error $ "Cannot add matrices of different size: " ++ showMatrixSize left ++
        " and " ++ showMatrixSize right
    | otherwise =
      mkMatrixWithTranspose xRows xCols $ zipWith (+!) xs ys
  sumColumns (StorableMatrixWithTranspose rows cols xs _) =
    VC.fromList $ cfmap VC.sum $ svecTakeBy rows cols xs
  sum (StorableMatrixWithTranspose _ _ xs _) = VC.sum xs

{-# INLINABLE mkMatrixWithTranspose #-}
mkMatrixWithTranspose
  :: (ElemConstraints StorableMatrixWithTranspose a)
  => Int
  -> Int
  -> S.Vector a
  -> StorableMatrixWithTranspose a
mkMatrixWithTranspose rows cols matrixData =
  StorableMatrixWithTranspose
    { smtRows    = rows
    , smtColumns = cols
    , smtData    = matrixData
    , smtDataT   = transposeMatrixData rows cols matrixData
    }

{-# INLINABLE transposeMatrixData #-}
transposeMatrixData
  :: (ElemConstraints StorableMatrixWithTranspose a)
  => Int
  -> Int
  -> S.Vector a
  -> S.Vector a
transposeMatrixData rows cols xs =
  S.unsafeBackpermute xs $ S.fromList
    [ c + cols * r
    | c <- [0..cols - 1]
    , r <- [0..rows - 1]
    ]

{-# INLINABLE svecTakeBy #-}
svecTakeBy
  :: (ElemConstraints StorableMatrixWithTranspose a)
  => Int
  -> Int
  -> S.Vector a
  -> [S.Vector a]
svecTakeBy rows cols vs =
  map (\r -> S.unsafeSlice (r *! cols) cols vs) [0..rows -! 1]

