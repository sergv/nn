----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aligned.Double.Foreign
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

module Data.Aligned.Double.Foreign where

import Foreign
import Foreign.C.Types

import Data.Aligned.Double
import Data.OpenBlasEnums

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

