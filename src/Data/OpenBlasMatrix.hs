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

import Data.Aligned
import qualified Data.Aligned as Aligned
import Data.AlignedStorableVector (AlignedStorableVector(..))
import qualified Data.AlignedStorableVector as ASV
import Data.ConstrainedConvert (Convert)
import qualified Data.ConstrainedConvert as Conv
import Data.ConstrainedFunctor
import Data.OpenBlasEnums
import Data.StorableMatrixWithTranspose (StorableMatrixWithTranspose(..))
import Data.MatrixClass
import qualified Data.VectClass as VC
import Data.Zippable
import Util

data OpenBlasMatrix a = OpenBlasMatrix
  { obmRows    :: {-# UNPACK #-} !Int
  , obmColumns :: {-# UNPACK #-} !Int
  , _obmData   :: !(S.Vector a)
  }
  deriving (Show, Eq, Ord)

unboxedMatrixWithTransposeToList :: (ElemConstraints AlignedConstraint a) => OpenBlasMatrix a -> [[a]]
unboxedMatrixWithTransposeToList (OpenBlasMatrix _ cols xs) = takeBy cols $ VC.toList xs

instance (ElemConstraints AlignedConstraint a, Pretty a) => Pretty (OpenBlasMatrix a) where
  pretty um@(OpenBlasMatrix rows cols _) =
    "Matrix " <> PP.int rows <> "x" <> PP.int cols PP.<$>
    PP.vsep (L.map showRow $ unboxedMatrixWithTransposeToList um)
    where
      showRow :: [a] -> Doc
      showRow = PP.hcat . PP.punctuate PP.comma . L.map pretty

instance (ElemConstraints AlignedConstraint a) => NFData (OpenBlasMatrix a) where
  rnf (OpenBlasMatrix rows cols xs) = rnf rows `seq` rnf cols `seq` rnf xs

instance ConstrainedFunctor AlignedConstraint OpenBlasMatrix where
  {-# INLINABLE cfmap #-}
  cfmap f (OpenBlasMatrix rows cols xs) = OpenBlasMatrix rows cols $ cfmap f xs

instance Zippable AlignedConstraint OpenBlasMatrix where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith f (OpenBlasMatrix xRows xCols xs) (OpenBlasMatrix yRows yCols ys)
    | xRows == yRows && xCols == yCols =
      OpenBlasMatrix xRows xCols $ zipWith f xs ys
    | otherwise = error "OpenBlasMatrix.zipWith: cannot zip matrices of different shapes"
  zipWith3 f (OpenBlasMatrix xRows xCols xs) (OpenBlasMatrix yRows yCols ys) (OpenBlasMatrix zRows zCols zs)
    | xRows == yRows && yRows == zRows && xCols == yCols && yCols == zCols =
      OpenBlasMatrix xRows xCols $ zipWith3 f xs ys zs
    | otherwise = error "OpenBlasMatrix.zipWith3: cannot zip matrices of different shapes"
  zipWith4 f (OpenBlasMatrix xRows xCols xs) (OpenBlasMatrix yRows yCols ys) (OpenBlasMatrix zRows zCols zs) (OpenBlasMatrix wRows wCols ws)
    | xRows == yRows && yRows == zRows && zRows == wRows && xCols == yCols && yCols == zCols && zCols == wCols =
      OpenBlasMatrix xRows xCols $ zipWith4 f xs ys zs ws
    | otherwise = error "OpenBlasMatrix.zipWith4: cannot zip matrices of different shapes"

instance Convert AlignedConstraint StorableConstraint OpenBlasMatrix StorableMatrixWithTranspose where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo (OpenBlasMatrix wRows wCols ws) =
    StorableMatrixWithTranspose wRows wCols ws (error "transpose")
  convertFrom (StorableMatrixWithTranspose wRows wCols ws _) =
    OpenBlasMatrix wRows wCols ws

instance Matrix AlignedConstraint OpenBlasMatrix AlignedStorableVector where
  {-# INLINABLE rows         #-}
  {-# INLINABLE columns      #-}
  {-# INLINABLE outerProduct #-}
  {-# INLINABLE vecMulRight  #-}
  {-# INLINABLE transpose    #-}
  {-# INLINABLE matrixMult   #-}
  {-# INLINABLE (|+|)        #-}
  {-# INLINABLE addScaled    #-}
  {-# INLINABLE sumColumns   #-}
  fromList [] =
    error "OpenBlasMatrix.fromList: cannot create PureMatrix from empty list of rows"
  fromList wss@(ws:_)
    | cols /= 0 && all (== cols) (L.map length wss) =
      OpenBlasMatrix rows cols matrixData
    | otherwise =
      error $ "OpenBlasMatrix.fromList: cannot create PureMatrix from list " ++ show wss
    where
      rows        = length wss
      cols        = length ws
      matrixData  = VC.fromList $ L.concat wss
  toList (OpenBlasMatrix _ cols xs) = takeBy cols $ VC.toList xs
  rows    = obmRows
  columns = obmColumns
  replicateM rows cols action =
    OpenBlasMatrix rows cols <$> VC.replicateM (rows *! cols) action
  outerProduct columnVec rowVec =
    -- OpenBlasMatrix rows cols matrixData
    -- where
    --   rows  = VC.length columnVec
    --   cols  = VC.length rowVec
    --   matrixData = S.concatMap (\c -> getAlignedStorableVector $ cfmap (c *!) rowVec) $ getAlignedStorableVector columnVec
    unsafePerformIO $ do
      matrixData <- SM.replicate (rows *! cols) 0
      ASV.unsafeWith columnVec $ \columnVecPtr ->
        ASV.unsafeWith rowVec $ \rowVecPtr ->
          SM.unsafeWith matrixData $ \resultPtr ->
            ger
              rowMajorOrder
              rows'
              cols'
              1
              columnVecPtr
              incx
              rowVecPtr
              incy
              resultPtr
              cols'
      OpenBlasMatrix rows cols <$> S.freeze matrixData
    where
      rows  = VC.length columnVec
      cols  = VC.length rowVec
      rows' = Size $ fromIntegral rows
      cols' = Size $ fromIntegral cols
      incx  = BlasInt 1
      incy  = incx
  vecMulRight (OpenBlasMatrix rows cols xs) ys =
    unsafePerformIO $ do
      zs <- SM.unsafeNew rows
      S.unsafeWith xs $ \matrixPtr ->
        ASV.unsafeWith ys $ \vectorPtr ->
          SM.unsafeWith zs $ \resultPtr ->
            gemv
              rowMajorOrder
              noTranspose
              rows'     -- m
              cols'     -- n
              1         -- alpha
              matrixPtr -- A
              cols'     -- lda >= max(1, m)
              vectorPtr -- x
              incx      -- incx
              0         -- beta
              resultPtr -- y
              incy      -- incy
      AlignedStorableVector <$> S.freeze zs
    where
      rows' = Size $ fromIntegral rows
      cols' = Size $ fromIntegral cols
      incx  = BlasInt 1
      incy  = incx
  transpose (OpenBlasMatrix rows cols xs) =
    OpenBlasMatrix cols rows $ transposeMatrixData rows cols xs
  matrixMult (OpenBlasMatrix xRows xCols xs) (OpenBlasMatrix _ yCols ys) =
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
            gemm
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
      OpenBlasMatrix xRows yCols <$> S.freeze zs
    where
      xRows' = Size $ fromIntegral xRows
      yCols' = Size $ fromIntegral yCols
      xCols' = Size $ fromIntegral xCols
  (|+|) _left@(OpenBlasMatrix xRows xCols xs) _right@(OpenBlasMatrix _yRows _yCols ys) =
    -- | xRows /= yRows || xCols /= yCols =
    --   error $ "Cannot add matrices of different size: " ++ showMatrixSize left ++
    --     " and " ++ showMatrixSize right
    -- | otherwise =
    unsafePerformIO $ do
      zs <- SM.unsafeNew size
      S.unsafeWith xs $ \leftMatrixPtr ->
        S.unsafeWith ys $ \rightMatrixPtr ->
          SM.unsafeWith zs $ \resultPtr ->
            addVectors
              size
              leftMatrixPtr
              rightMatrixPtr
              resultPtr
      OpenBlasMatrix xRows xCols <$> S.freeze zs
    where
      size = xRows * xCols
  addScaled _left@(OpenBlasMatrix xRows xCols xs) c _right@(OpenBlasMatrix _yRows _yCols ys) =
    -- | xRows /= yRows || xCols /= yCols =
    --   error $ "Cannot add matrices of different size: " ++ showMatrixSize left ++
    --     " and " ++ showMatrixSize right
    -- | otherwise =
    unsafePerformIO $ do
      zs <- SM.unsafeNew size
      S.unsafeWith xs $ \leftMatrixPtr ->
        S.unsafeWith ys $ \rightMatrixPtr ->
          SM.unsafeWith zs $ \resultPtr ->
            addVectorsScaled
              size
              leftMatrixPtr
              c
              rightMatrixPtr
              resultPtr
      OpenBlasMatrix xRows xCols <$> S.freeze zs
    where
      size = xRows * xCols
    -- OpenBlasMatrix xRows xCols (zipWith f xs ys) (zipWith f xsT ysT)
    --   where
    --     f x y = x +! c *! y
  sumColumns (OpenBlasMatrix rows cols xs) =
    VC.fromList $ cfmap VC.sum $ svecTakeBy rows cols xs
  sum (OpenBlasMatrix _ _ xs) = VC.sum xs
  matrixMultByTransposedLeft (OpenBlasMatrix xRows xCols xs) (OpenBlasMatrix _ yCols ys) =
    unsafePerformIO $ do
      zs <- SM.unsafeNew (xCols * yCols)
      S.unsafeWith xs $ \leftMatrixPtr ->
        S.unsafeWith ys $ \rightMatrixPtr ->
          SM.unsafeWith zs $ \resultPtr ->
            gemm
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
      OpenBlasMatrix xCols yCols <$> S.freeze zs
    where
      xRows' = Size $ fromIntegral xRows
      yCols' = Size $ fromIntegral yCols
      xCols' = Size $ fromIntegral xCols
  matrixMultByTransposedRight (OpenBlasMatrix xRows xCols xs) (OpenBlasMatrix yRows yCols ys) =
    unsafePerformIO $ do
      zs <- SM.unsafeNew (xRows * yRows)
      S.unsafeWith xs $ \leftMatrixPtr ->
        S.unsafeWith ys $ \rightMatrixPtr ->
          SM.unsafeWith zs $ \resultPtr ->
            gemm
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
      OpenBlasMatrix xRows yRows <$> S.freeze zs
    where
      xRows' = Size $ fromIntegral xRows
      xCols' = Size $ fromIntegral xCols
      yRows' = Size $ fromIntegral yRows
      yCols' = Size $ fromIntegral yCols
  normL2Square (OpenBlasMatrix _ _ xs) = VC.dot xs' xs'
    where
      xs' = AlignedStorableVector xs
  exp (OpenBlasMatrix rows cols xs) =
    unsafePerformIO $ do
      result <- SM.unsafeNew n
      S.unsafeWith xs $ \xsPtr ->
        SM.unsafeWith result $ \resultPtr ->
          Aligned.mapExp n xsPtr resultPtr
      OpenBlasMatrix rows cols <$> S.freeze result
    where
      n = rows *! cols

{-# INLINABLE transposeMatrixData #-}
transposeMatrixData
  :: (ElemConstraints AlignedConstraint a)
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
  :: (ElemConstraints AlignedConstraint a)
  => Int
  -> Int
  -> S.Vector a
  -> [S.Vector a]
svecTakeBy rows cols vs =
  map (\r -> S.unsafeSlice (r *! cols) cols vs) [0..rows -! 1]
