----------------------------------------------------------------------------
-- |
-- Module      :  Data.OpenBlasMatrix
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DeriveFoldable           #-}
{-# LANGUAGE DeriveTraversable        #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module Data.OpenBlasMatrix (OpenBlasMatrix) where

import Prelude hiding (zipWith, zipWith3)
import Control.DeepSeq
import Data.Monoid
import qualified Data.List as L
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP
import System.IO.Unsafe

import Data.ConstrainedConvert (Convert)
import qualified Data.ConstrainedConvert as Conv
import Data.ConstrainedFunctor
import Data.OpenBlasMatrix.Foreign
import Data.StorableMatrixWithTranspose (StorableMatrixWithTranspose(..))
import Data.StorableVectorDouble (StorableVectorDouble(..))
import qualified Data.StorableVectorDouble as SVD
import Data.MatrixClass
import qualified Data.VectClass as VC
import Data.Zippable
import Util

-- import Debug.Trace

data OpenBlasMatrix a = OpenBlasMatrix
  { obmRows    :: {-# UNPACK #-} !Int
  , obmColumns :: {-# UNPACK #-} !Int
  , obmData    :: !(S.Vector a)
  -- | Deliberately left non-strict in order to not pay cost if
  -- no transpose operations were applied.
  , obmDataT   :: S.Vector a
  }
  deriving (Show, Eq, Ord)

unboxedMatrixWithTransposeToList :: (ElemConstraints IsDoubleConstraint a) => OpenBlasMatrix a -> [[a]]
unboxedMatrixWithTransposeToList (OpenBlasMatrix _ cols xs _) = takeBy cols $ VC.toList xs

instance (ElemConstraints IsDoubleConstraint a, Pretty a) => Pretty (OpenBlasMatrix a) where
  pretty um@(OpenBlasMatrix rows cols _ _) =
    "Matrix " <> PP.int rows <> "x" <> PP.int cols <> " (double)" PP.<$>
    PP.vsep (L.map showRow $ unboxedMatrixWithTransposeToList um)
    where
      showRow :: [a] -> Doc
      showRow = PP.hcat . PP.punctuate PP.comma . L.map pretty

instance (ElemConstraints IsDoubleConstraint a) => NFData (OpenBlasMatrix a) where
  rnf (OpenBlasMatrix rows cols xs ys) = rnf rows `seq` rnf cols `seq` rnf xs `seq` rnf ys

instance ConstrainedFunctor IsDoubleConstraint OpenBlasMatrix where
  {-# INLINABLE cfmap #-}
  cfmap f (OpenBlasMatrix rows cols xs _) =
    mkMatrixWithTranspose rows cols $ cfmap f xs

instance Zippable IsDoubleConstraint OpenBlasMatrix where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith f (OpenBlasMatrix xRows xCols xs _) (OpenBlasMatrix yRows yCols ys _)
    | xRows == yRows && xCols == yCols =
      mkMatrixWithTranspose xRows xCols $ zipWith f xs ys
    | otherwise = error "OpenBlasMatrix.zipWith: cannot zip matrices of different shapes"
  zipWith3 f (OpenBlasMatrix xRows xCols xs _) (OpenBlasMatrix yRows yCols ys _) (OpenBlasMatrix zRows zCols zs _)
    | xRows == yRows && yRows == zRows && xCols == yCols && yCols == zCols =
      mkMatrixWithTranspose xRows xCols $ zipWith3 f xs ys zs
    | otherwise = error "OpenBlasMatrix.zipWith3: cannot zip matrices of different shapes"
  zipWith4 f (OpenBlasMatrix xRows xCols xs _) (OpenBlasMatrix yRows yCols ys _) (OpenBlasMatrix zRows zCols zs _) (OpenBlasMatrix wRows wCols ws _)
    | xRows == yRows && yRows == zRows && zRows == wRows && xCols == yCols && yCols == zCols && zCols == wCols =
      mkMatrixWithTranspose xRows xCols $ zipWith4 f xs ys zs ws
    | otherwise = error "OpenBlasMatrix.zipWith4: cannot zip matrices of different shapes"

instance Convert IsDoubleConstraint StorableConstraint OpenBlasMatrix StorableMatrixWithTranspose where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo (OpenBlasMatrix wRows wCols ws ws') =
    StorableMatrixWithTranspose wRows wCols ws ws'
  convertFrom (StorableMatrixWithTranspose wRows wCols ws ws') =
    OpenBlasMatrix wRows wCols ws ws'

instance Matrix IsDoubleConstraint OpenBlasMatrix StorableVectorDouble where
  {-# INLINABLE rows         #-}
  {-# INLINABLE columns      #-}
  {-# INLINABLE outerProduct #-}
  {-# INLINABLE vecMulRight  #-}
  {-# INLINABLE transpose    #-}
  {-# INLINABLE matrixMult   #-}
  {-# INLINABLE (|+|)        #-}
  {-# INLINABLE sumColumns   #-}
  fromList [] =
    error "OpenBlasMatrix.fromList: cannot create PureMatrix from empty list of rows"
  fromList wss@(ws:_)
    | cols /= 0 && all (== cols) (L.map length wss) =
      mkMatrixWithTranspose rows cols matrixData
    | otherwise =
      error $ "OpenBlasMatrix.fromList: cannot create PureMatrix from list " ++ show wss
    where
      rows        = length wss
      cols        = length ws
      matrixData  = VC.fromList $ L.concat wss
  toList (OpenBlasMatrix _ cols xs _) = takeBy cols $ VC.toList xs
  rows    = obmRows
  columns = obmColumns
  replicateM rows cols action =
    mkMatrixWithTranspose rows cols <$> VC.replicateM (rows *! cols) action
  outerProduct columnVec rowVec =
    -- mkMatrixWithTranspose rows cols matrixData
    -- where
    --   rows  = VC.length columnVec
    --   cols  = VC.length rowVec
    --   matrixData = S.concatMap (\c -> getStorableVectorDouble $ cfmap (c *!) rowVec) $ getStorableVectorDouble columnVec
    unsafePerformIO $ do
      matrixData <- SM.replicate (rows *! cols) 0
      SVD.unsafeWith columnVec $ \columnVecPtr ->
        SVD.unsafeWith rowVec $ \rowVecPtr ->
          SM.unsafeWith matrixData $ \resultPtr ->
            dger
              rowMajorOrder
              rows'
              cols'
              1.0
              columnVecPtr
              incx
              rowVecPtr
              incy
              resultPtr
              cols'
      mkMatrixWithTranspose rows cols <$> S.freeze matrixData
    where
      rows  = VC.length columnVec
      cols  = VC.length rowVec
      rows' = Size $ fromIntegral rows
      cols' = Size $ fromIntegral cols
      incx  = BlasInt 1
      incy  = incx
  vecMulRight (OpenBlasMatrix rows cols xs _) ys =
    -- VC.fromList $ L.map (\zs -> VC.dot zs ys) $ vecTakeBy rows cols xs
    -- trace ("rows = " ++ show rows ++ ", cols = " ++ show cols ++ ", xs = " ++ show xs ++ ", ys = " ++ show ys) $
    unsafePerformIO $ do
      zs <- SM.unsafeNew rows
      S.unsafeWith xs $ \matrixPtr ->
        SVD.unsafeWith ys $ \vectorPtr ->
          SM.unsafeWith zs $ \resultPtr ->
            dgemv
              rowMajorOrder
              noTranspose
              rows'       -- m
              cols'       -- n
              1           -- alpha
              matrixPtr   -- A
              cols'       -- lda >= max(1, m)
              vectorPtr   -- x
              incx        -- incx
              0           -- beta
              resultPtr   -- y
              incy        -- incy
      StorableVectorDouble <$> S.freeze zs
    where
      rows' = Size $ fromIntegral rows
      cols' = Size $ fromIntegral cols
      incx  = BlasInt 1
      incy  = incx
  transpose (OpenBlasMatrix rows cols xs xsT) =
    OpenBlasMatrix cols rows xsT xs
  matrixMult (OpenBlasMatrix xRows xCols xs _) (OpenBlasMatrix _ yCols ys _) =
    -- -- This check is needed for optimization of using VC.foldr1 instead of
    -- -- VC.monoFoldr. Also it's somewhat meaningless to have matrices with any
    -- -- dimension equal to zero.
    -- | xCols == 0     =
    --   error "PureMatrix.matrixMult: number of columns for right matrix is zero"
    -- | xCols /= yRows =
    --   error $ "PureMatrix.matrixMult: number of columns for left matrix and " ++
    --     "rows for right matrix mismatch: xCols = " ++ show xCols ++
    --     ", yRows = " ++ show yRows
    -- | otherwise      =
    unsafePerformIO $ do
      zs <- SM.unsafeNew (xRows * yCols)
      S.unsafeWith xs $ \leftMatrixPtr ->
        S.unsafeWith ys $ \rightMatrixPtr ->
          SM.unsafeWith zs $ \resultPtr ->
            dgemm
              rowMajorOrder
              noTranspose    -- TransA
              noTranspose    -- TransB
              xRows'         -- M
              yCols'         -- N
              xCols'         -- K
              1              -- alpha
              leftMatrixPtr  -- A
              xCols'         -- lda
              rightMatrixPtr -- B
              yCols'         -- ldb
              0              -- beta
              resultPtr      -- C
              yCols'         -- ldc
      mkMatrixWithTranspose xRows yCols <$> S.freeze zs
    where
      xRows' = Size $ fromIntegral xRows
      yCols' = Size $ fromIntegral yCols
      xCols' = Size $ fromIntegral xCols
  (|+|) left@(OpenBlasMatrix xRows xCols xs xsT) right@(OpenBlasMatrix yRows yCols ys ysT)
    | xRows /= yRows || xCols /= yCols =
      error $ "Cannot add matrices of different size: " ++ showMatrixSize left ++
        " and " ++ showMatrixSize right
    | otherwise =
      OpenBlasMatrix xRows xCols (zipWith (+!) xs ys) (zipWith (+!) xsT ysT)
  sumColumns (OpenBlasMatrix rows cols xs _) =
    VC.fromList $ cfmap VC.sum $ svecTakeBy rows cols xs
  sum (OpenBlasMatrix _ _ xs _) = VC.sum xs
  matrixMultByTransposedLeft (OpenBlasMatrix xRows xCols xs _) (OpenBlasMatrix _ yCols ys _) =
    unsafePerformIO $ do
      zs <- SM.unsafeNew (xCols * yCols)
      S.unsafeWith xs $ \leftMatrixPtr ->
        S.unsafeWith ys $ \rightMatrixPtr ->
          SM.unsafeWith zs $ \resultPtr ->
            dgemm
              rowMajorOrder
              transposed     -- TransA
              noTranspose    -- TransB
              xCols'         -- M
              yCols'         -- N
              xRows'         -- K
              1              -- alpha
              leftMatrixPtr  -- A
              xCols'         -- lda
              rightMatrixPtr -- B
              yCols'         -- ldb
              0              -- beta
              resultPtr      -- C
              yCols'         -- ldc
      mkMatrixWithTranspose xCols yCols <$> S.freeze zs
    where
      xRows' = Size $ fromIntegral xRows
      yCols' = Size $ fromIntegral yCols
      xCols' = Size $ fromIntegral xCols
  matrixMultByTransposedRight (OpenBlasMatrix xRows xCols xs _) (OpenBlasMatrix yRows yCols ys _) =
    unsafePerformIO $ do
      zs <- SM.unsafeNew (xRows * yRows)
      S.unsafeWith xs $ \leftMatrixPtr ->
        S.unsafeWith ys $ \rightMatrixPtr ->
          SM.unsafeWith zs $ \resultPtr ->
            dgemm
              rowMajorOrder
              noTranspose    -- TransA
              transposed     -- TransB
              xRows'         -- M
              yRows'         -- N
              xCols'         -- K
              1              -- alpha
              leftMatrixPtr  -- A
              xCols'         -- lda
              rightMatrixPtr -- B
              yCols'         -- ldb
              0              -- beta
              resultPtr      -- C
              yRows'         -- ldc
      mkMatrixWithTranspose xRows yRows <$> S.freeze zs
    where
      xRows' = Size $ fromIntegral xRows
      xCols' = Size $ fromIntegral xCols
      yRows' = Size $ fromIntegral yRows
      yCols' = Size $ fromIntegral yCols


{-# INLINABLE mkMatrixWithTranspose #-}
mkMatrixWithTranspose
  :: (ElemConstraints IsDoubleConstraint a)
  => Int
  -> Int
  -> S.Vector a
  -> OpenBlasMatrix a
mkMatrixWithTranspose rows cols matrixData =
  OpenBlasMatrix
    { obmRows    = rows
    , obmColumns = cols
    , obmData    = matrixData
    , obmDataT   = transposeMatrixData rows cols matrixData
    }

{-# INLINABLE transposeMatrixData #-}
transposeMatrixData
  :: (ElemConstraints IsDoubleConstraint a)
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
  :: (ElemConstraints IsDoubleConstraint a)
  => Int
  -> Int
  -> S.Vector a
  -> [S.Vector a]
svecTakeBy rows cols vs =
  map (\r -> S.unsafeSlice (r *! cols) cols vs) [0..rows -! 1]
