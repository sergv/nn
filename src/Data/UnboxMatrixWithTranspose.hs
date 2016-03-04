----------------------------------------------------------------------------
-- |
-- Module      :  Data.UnboxMatrixWithTranspose
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

module Data.UnboxMatrixWithTranspose (UnboxMatrixWithTranspose, mkMatrixWithTranspose) where

import Prelude hiding (zipWith, zipWith3)
import Control.DeepSeq
import Data.Monoid
import qualified Data.List as L
import qualified Data.Vector.Unboxed as U
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
-- benchmarking rprop generic - UnboxMatrixWithTranspose
-- time                 9.482 s    (8.817 s .. 10.17 s)
--                      0.999 R²   (0.997 R² .. 1.000 R²)
-- mean                 9.687 s    (9.554 s .. 9.923 s)
-- std dev              204.5 ms   (0.0 s .. 212.9 ms)
-- variance introduced by outliers: 19% (moderately inflated)

data UnboxMatrixWithTranspose a = UnboxMatrixWithTranspose
  { umtRows    :: {-# UNPACK #-} !Int
  , umtColumns :: {-# UNPACK #-} !Int
  , umtData    :: !(U.Vector a)
  -- | Deliberately left non-strict in order to not pay cost if
  -- no transpose operations were applied.
  , umtDataT   :: U.Vector a
  }
  deriving (Show, Eq, Ord)

unboxedMatrixWithTransposeToList :: (ElemConstraints UnboxMatrixWithTranspose a) => UnboxMatrixWithTranspose a -> [[a]]
unboxedMatrixWithTransposeToList (UnboxMatrixWithTranspose _ cols xs _) = takeBy cols $ VC.toList xs

instance (ElemConstraints UnboxMatrixWithTranspose a, Pretty a) => Pretty (UnboxMatrixWithTranspose a) where
  pretty um@(UnboxMatrixWithTranspose rows cols _ _) =
    "Matrix " <> PP.int rows <> "x" <> PP.int cols <> " (double)" PP.<$>
    PP.vsep (L.map showRow $ unboxedMatrixWithTransposeToList um)
    where
      showRow :: [a] -> Doc
      showRow = PP.hcat . PP.punctuate PP.comma . L.map pretty

instance (ElemConstraints UnboxMatrixWithTranspose a) => NFData (UnboxMatrixWithTranspose a) where
  rnf (UnboxMatrixWithTranspose rows cols xs ys) = rnf rows `seq` rnf cols `seq` rnf xs `seq` rnf ys

instance ConstrainedFunctor UnboxMatrixWithTranspose where
  type ElemConstraints UnboxMatrixWithTranspose = U.Unbox
  {-# INLINABLE cfmap #-}
  cfmap f (UnboxMatrixWithTranspose rows cols xs _) =
    mkMatrixWithTranspose rows cols $ cfmap f xs

instance Zippable UnboxMatrixWithTranspose where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith f (UnboxMatrixWithTranspose xRows xCols xs _) (UnboxMatrixWithTranspose yRows yCols ys _)
    | xRows == yRows && xCols == yCols =
      mkMatrixWithTranspose xRows xCols $ zipWith f xs ys
    | otherwise = error "UnboxMatrixWithTranspose.zipWith: cannot zip matrices of different shapes"
  zipWith3 f (UnboxMatrixWithTranspose xRows xCols xs _) (UnboxMatrixWithTranspose yRows yCols ys _) (UnboxMatrixWithTranspose zRows zCols zs _)
    | xRows == yRows && yRows == zRows && xCols == yCols && yCols == zCols =
      mkMatrixWithTranspose xRows xCols $ zipWith3 f xs ys zs
    | otherwise = error "UnboxMatrixWithTranspose.zipWith3: cannot zip matrices of different shapes"
  zipWith4 f (UnboxMatrixWithTranspose xRows xCols xs _) (UnboxMatrixWithTranspose yRows yCols ys _) (UnboxMatrixWithTranspose zRows zCols zs _) (UnboxMatrixWithTranspose wRows wCols ws _)
    | xRows == yRows && yRows == zRows && zRows == wRows && xCols == yCols && yCols == zCols && zCols == wCols =
      mkMatrixWithTranspose xRows xCols $ zipWith4 f xs ys zs ws
    | otherwise = error "UnboxMatrixWithTranspose.zipWith4: cannot zip matrices of different shapes"

instance ConstrainedIsomorphism UnboxMatrixWithTranspose UnboxMatrixWithTranspose where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = id
  convertFrom = id

instance Matrix UnboxMatrixWithTranspose U.Vector where
  {-# INLINABLE rows         #-}
  {-# INLINABLE columns      #-}
  {-# INLINABLE outerProduct #-}
  {-# INLINABLE vecMulRight  #-}
  {-# INLINABLE transpose    #-}
  {-# INLINABLE matrixMult   #-}
  {-# INLINABLE (|+|)        #-}
  {-# INLINABLE sumColumns   #-}
  fromList [] =
    error "UnboxMatrixWithTranspose.fromList: cannot create UnboxMatrixWithTranspose from empty list of rows"
  fromList wss@(ws:_)
    | cols /= 0 && all (== cols) (L.map length wss) =
      mkMatrixWithTranspose rows cols matrixData
    | otherwise =
      error $ "UnboxMatrixWithTranspose.fromList: cannot create UnboxMatrixWithTranspose from list " ++ show wss
    where
      rows        = length wss
      cols        = length ws
      matrixData  = VC.fromList $ L.concat wss
  toList (UnboxMatrixWithTranspose _ cols xs _) = takeBy cols $ VC.toList xs
  rows    = umtRows
  columns = umtColumns
  replicateM rows cols action =
    mkMatrixWithTranspose rows cols <$> VC.replicateM (rows *! cols) action
  outerProduct columnVec rowVec =
    mkMatrixWithTranspose rows cols matrixData
    where
      rows = VC.length columnVec
      cols = VC.length rowVec
      matrixData = U.concatMap (\c -> cfmap (c *!) rowVec) columnVec
  vecMulRight (UnboxMatrixWithTranspose rows cols xs _) ys =
    VC.fromList $ L.map (\zs -> VC.dot zs ys) $ uvecTakeBy rows cols xs
    where

  transpose (UnboxMatrixWithTranspose rows cols xs xsT) =
    UnboxMatrixWithTranspose cols rows xsT xs
  matrixMult (UnboxMatrixWithTranspose xRows xCols xs _) (UnboxMatrixWithTranspose yRows yCols ys _)
    -- This check is needed for optimization of using VC.foldr1 instead of
    -- VC.monoFoldr. Also it's somewhat meaningless to have matrices with any
    -- dimension equal to zero.
    | xCols == 0     =
      error "UnboxMatrix.matrixMult: number of columns for right matrix is zero"
    | xCols /= yRows =
      error $ "UnboxMatrix.matrixMult: number of columns for left matrix and " ++
        "rows for right matrix mismatch: xCols = " ++ show xCols ++
        ", yRows = " ++ show yRows
    | otherwise      =
      mkMatrixWithTranspose xRows yCols matrixData
    where
      matrixData = U.concat
                 $ map (\xs -> VC.foldr1 (VC..+.) $ zipWith (\x ys -> cfmap (x *!) ys) xs yss) xss
      xss = map VC.toList $ uvecTakeBy xRows xCols xs
      yss = uvecTakeBy yRows yCols ys
  (|+|) left@(UnboxMatrixWithTranspose xRows xCols xs _) right@(UnboxMatrixWithTranspose yRows yCols ys _)
    | xRows /= yRows || xCols /= yCols =
      error $ "Cannot add matrices of different size: " ++ showMatrixSize left ++
        " and " ++ showMatrixSize right
    | otherwise =
      mkMatrixWithTranspose xRows xCols $ zipWith (+!) xs ys
  sumColumns (UnboxMatrixWithTranspose rows cols xs _) =
    VC.fromList $ cfmap VC.sum $ uvecTakeBy rows cols xs
  sum (UnboxMatrixWithTranspose _ _ xs _) = VC.sum xs

{-# INLINABLE mkMatrixWithTranspose #-}
mkMatrixWithTranspose
  :: (ElemConstraints UnboxMatrixWithTranspose a)
  => Int
  -> Int
  -> U.Vector a
  -> UnboxMatrixWithTranspose a
mkMatrixWithTranspose rows cols matrixData =
  UnboxMatrixWithTranspose
    { umtRows    = rows
    , umtColumns = cols
    , umtData    = matrixData
    , umtDataT   = transposeMatrixData rows cols matrixData
    }

{-# INLINABLE transposeMatrixData #-}
transposeMatrixData
  :: (ElemConstraints UnboxMatrixWithTranspose a)
  => Int
  -> Int
  -> U.Vector a
  -> U.Vector a
transposeMatrixData rows cols xs =
  U.unsafeBackpermute xs $ U.fromList
    [ c + cols * r
    | c <- [0..cols - 1]
    , r <- [0..rows - 1]
    ]

{-# INLINABLE uvecTakeBy #-}
uvecTakeBy
  :: (ElemConstraints UnboxMatrixWithTranspose a)
  => Int
  -> Int
  -> U.Vector a
  -> [U.Vector a]
uvecTakeBy rows cols vs =
  map (\r -> U.unsafeSlice (r *! cols) cols vs) [0..rows -! 1]

