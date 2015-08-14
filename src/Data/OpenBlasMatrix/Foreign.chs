----------------------------------------------------------------------------
-- |
-- Module      :  Data.OpenBlasMatrix.Foreign
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Data.OpenBlasMatrix.Foreign where

import Foreign
import Foreign.C.Types

import Data.Aligned.Double

#include <cblas.h>

{#context lib = "openblas"#}

{#enum CBLAS_ORDER as Order
  { CblasRowMajor as RowMajor
  , CblasColMajor as ColMajor
  }
  deriving (Eq, Ord)
 #}

{#enum CBLAS_TRANSPOSE as Transpose
  { CblasNoTrans     as NoTranspose
  , CblasTrans       as Transpose
  , CblasConjTrans   as ConjTranspose
  , CblasConjNoTrans as ConjNoTranspose
  }
  deriving (Eq, Ord)
 #}

newtype BlasOrder = BlasOrder ({#type CBLAS_ORDER#}) deriving (Show, Eq, Ord)

rowMajorOrder :: BlasOrder
rowMajorOrder = BlasOrder $ fromIntegral $ fromEnum RowMajor

newtype BlasTranspose = BlasTranspose ({#type CBLAS_TRANSPOSE#}) deriving (Show, Eq, Ord)

noTranspose :: BlasTranspose
noTranspose = BlasTranspose $ fromIntegral $ fromEnum NoTranspose

transposed :: BlasTranspose
transposed = BlasTranspose $ fromIntegral $ fromEnum Transpose

-- For increment integter arguments.
newtype BlasInt = BlasInt ({#type blasint#}) deriving (Show, Eq, Ord)
-- For size integer arguments.
newtype Size    = Size ({#type blasint#}) deriving (Show, Eq, Ord)

-- Matrix-vector multiplication
-- void cblas_dgemv(
--         OPENBLAS_CONST enum CBLAS_ORDER order,
--         OPENBLAS_CONST enum CBLAS_TRANSPOSE trans,
--         OPENBLAS_CONST blasint m,
--         OPENBLAS_CONST blasint n,
--         OPENBLAS_CONST double alpha,
--         OPENBLAS_CONST double  *a,
--         OPENBLAS_CONST blasint lda,
--         OPENBLAS_CONST double  *x,
--         OPENBLAS_CONST blasint incx,
--         OPENBLAS_CONST double beta,
--         double  *y,
--         OPENBLAS_CONST blasint incy);
foreign import ccall unsafe "cblas_dgemv" dgemv
  :: BlasOrder
  -> BlasTranspose
  -> Size
  -> Size
  -> Double
  -> Ptr AlignedDouble
  -> Size
  -> Ptr AlignedDouble
  -> BlasInt
  -> Double
  -> Ptr AlignedDouble
  -> BlasInt
  -> IO ()

-- Outer product
-- void cblas_dger(
--         OPENBLAS_CONST enum CBLAS_ORDER order,
--         OPENBLAS_CONST blasint M,
--         OPENBLAS_CONST blasint N,
--         OPENBLAS_CONST double  alpha,
--         OPENBLAS_CONST double *X,
--         OPENBLAS_CONST blasint incX,
--         OPENBLAS_CONST double *Y,
--         OPENBLAS_CONST blasint incY,
--         double *A,
--         OPENBLAS_CONST blasint lda);
foreign import ccall unsafe "cblas_dger" dger
  :: BlasOrder
  -> Size
  -> Size
  -> Double
  -> Ptr AlignedDouble
  -> BlasInt
  -> Ptr AlignedDouble
  -> BlasInt
  -> Ptr AlignedDouble
  -> Size
  -> IO ()

-- Matrix multiplication
-- void cblas_dgemm(
--         OPENBLAS_CONST enum CBLAS_ORDER Order,
--         OPENBLAS_CONST enum CBLAS_TRANSPOSE TransA,
--         OPENBLAS_CONST enum CBLAS_TRANSPOSE TransB,
--         OPENBLAS_CONST blasint M,
--         OPENBLAS_CONST blasint N,
--         OPENBLAS_CONST blasint K,
--         OPENBLAS_CONST double alpha,
--         OPENBLAS_CONST double *A,
--         OPENBLAS_CONST blasint lda,
--         OPENBLAS_CONST double *B,
--         OPENBLAS_CONST blasint ldb,
--         OPENBLAS_CONST double beta,
--         double *C,
--         OPENBLAS_CONST blasint ldc);
foreign import ccall unsafe "cblas_dgemm" dgemm
  :: BlasOrder
  -> BlasTranspose
  -> BlasTranspose
  -> Size
  -> Size
  -> Size
  -> Double
  -> Ptr AlignedDouble
  -> Size
  -> Ptr AlignedDouble
  -> Size
  -> Double
  -> Ptr AlignedDouble
  -> Size
  -> IO ()

foreign import ccall unsafe "add" addVectors
  :: Int
  -> Ptr AlignedDouble
  -> Ptr AlignedDouble
  -> Ptr AlignedDouble
  -> IO ()

foreign import ccall unsafe "addScaled" addVectorsScaled
  :: Int
  -> Ptr AlignedDouble
  -> Double
  -> Ptr AlignedDouble
  -> Ptr AlignedDouble
  -> IO ()

foreign import ccall unsafe "dot" dotProduct
  :: Int
  -> Ptr AlignedDouble
  -> Ptr AlignedDouble
  -> IO Double
