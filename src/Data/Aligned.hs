----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aligned
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module Data.Aligned where

import Foreign (Storable)
import Foreign.Ptr (Ptr)

import Data.OpenBlasEnums
import Data.Aligned.Double
import qualified Data.Aligned.Double.Foreign as DF
import Data.Aligned.Float
import qualified Data.Aligned.Float.Foreign as FF

import Data.ConstrainedFunctor

data AlignedConstraint
type instance ElemConstraints AlignedConstraint = Aligned

class (Storable a) => Aligned a where
  -- | Matrix-vector multiplication
  -- y = alpha * A * x + beta * y
  gemv
    :: BlasOrder
    -> BlasTranspose
    -> Size    -- ^ M
    -> Size    -- ^ N
    -> a       -- ^ alpha
    -> Ptr a   -- ^ A
    -> Size    -- ^ lda
    -> Ptr a   -- ^ x
    -> BlasInt -- ^ incX
    -> a       -- ^ beta
    -> Ptr a   -- ^ y
    -> BlasInt -- ^ incY
    -> IO ()
  -- | Outer product
  -- A = alpha * x * y' + A
  ger
    :: BlasOrder
    -> Size    -- ^ M, x length
    -> Size    -- ^ N, y length
    -> a       -- ^ alpha
    -> Ptr a   -- ^ x, vector
    -> BlasInt -- ^ incX
    -> Ptr a   -- ^ y, vector
    -> BlasInt -- ^ incY
    -> Ptr a   -- ^ A, matrix
    -> Size    -- ^ lda
    -> IO ()
  -- | Matrix multiplication
  gemm
    :: BlasOrder
    -> BlasTranspose
    -> BlasTranspose
    -> Size  -- ^ M
    -> Size  -- ^ N
    -> Size  -- ^ K
    -> a     -- ^ alpha
    -> Ptr a -- ^ A
    -> Size  -- ^ lda
    -> Ptr a -- ^ B
    -> Size  -- ^ ldb
    -> a     -- ^ beta
    -> Ptr a -- ^ C
    -> Size  -- ^ ldc
    -> IO ()
  -- | z = x + y
  addVectors
    :: Int
    -> Ptr a -- ^ x
    -> Ptr a -- ^ y
    -> Ptr a -- ^ z
    -> IO ()
  -- | z = x + c * y
  addVectorsScaled
    :: Int
    -> Ptr a -- ^ x
    -> a     -- ^ c
    -> Ptr a -- ^ y
    -> Ptr a -- ^ z
    -> IO ()
  dotProduct
    :: Int
    -> Ptr a
    -> Ptr a
    -> IO a

instance Aligned AlignedFloat where
  {-# INLINABLE gemv             #-}
  {-# INLINABLE ger              #-}
  {-# INLINABLE gemm             #-}
  {-# INLINABLE addVectors       #-}
  {-# INLINABLE addVectorsScaled #-}
  {-# INLINABLE dotProduct       #-}
  gemv ord trans m n (AlignedFloat alpha) a lda x incx (AlignedFloat beta) y incy =
    FF.sgemv ord trans m n alpha a lda x incx beta y incy
  ger ord m n (AlignedFloat alpha) x incx y incy a lda =
    FF.sger ord m n alpha x incx y incy a lda
  gemm ord trans trans' m n k (AlignedFloat alpha) a lda b ldb (AlignedFloat beta) c ldc =
    FF.sgemm ord trans trans' m n k alpha a lda b ldb beta c ldc
  addVectors = FF.addVectorsf
  addVectorsScaled n x (AlignedFloat c) y z = FF.addVectorsScaledf n x c y z
  dotProduct n x y = AlignedFloat <$> FF.dotProductf n x y

instance Aligned AlignedDouble where
  gemv ord trans m n (AlignedDouble alpha) a lda x incx (AlignedDouble beta) y incy =
    DF.dgemv ord trans m n alpha a lda x incx beta y incy
  ger ord m n (AlignedDouble alpha) x incx y incy a lda =
    DF.dger ord m n alpha x incx y incy a lda
  gemm ord trans trans' m n k (AlignedDouble alpha) a lda b ldb (AlignedDouble beta) c ldc =
    DF.dgemm ord trans trans' m n k alpha a lda b ldb beta c ldc
  addVectors = DF.addVectors
  addVectorsScaled n x (AlignedDouble c) y z = DF.addVectorsScaled n x c y z
  dotProduct n x y = AlignedDouble <$> DF.dotProduct n x y
