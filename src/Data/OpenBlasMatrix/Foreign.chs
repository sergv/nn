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

newtype BlasOrder     = BlasOrder ({#type CBLAS_ORDER#}) deriving (Show, Eq, Ord)
newtype BlasTranspose = BlasTranspose ({#type CBLAS_TRANSPOSE#}) deriving (Show, Eq, Ord)
newtype BlasInt       = BlasInt ({#type blasint#}) deriving (Show, Eq, Ord)
newtype Size          = Size ({#type blasint#}) deriving (Show, Eq, Ord)

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
  -> Ptr Double
  -> Size
  -> Ptr Double
  -> BlasInt
  -> Double
  -> Ptr Double
  -> BlasInt
  -> IO ()
