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

module Data.Aligned (Aligned(..)) where

import Foreign (Storable)
import Foreign.C.Types (CUInt(..))
import Foreign.Ptr (Ptr)

import Data.OpenBlasEnums
import Data.Aligned.Double
import qualified Data.Aligned.Double.Foreign as DF
import Data.Aligned.Float
import qualified Data.Aligned.Float.Foreign as FF

-- | Storable values that support BLAS operations.
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
  -- | Input and output must be aligned to 32 bit and must not overlap.
  mapExp
    :: Int
    -> Ptr a -- ^ input
    -> Ptr a -- ^ output
    -> IO ()
  -- | Input and output must be aligned to 32 bit and must not overlap.
  mapSigmoid
    :: Int
    -> Ptr a -- ^ input
    -> Ptr a -- ^ output
    -> IO ()
  -- | Input and output must be aligned to 32 bit and must not overlap.
  mapSigmoidDeriv
    :: Int
    -> Ptr a -- ^ input
    -> Ptr a -- ^ output
    -> IO ()
  -- | Input and output must be aligned to 32 bit and must not overlap.
  mapTanh
    :: Int
    -> Ptr a -- ^ input
    -> Ptr a -- ^ output
    -> IO ()
  -- | Input and output must be aligned to 32 bit and must not overlap.
  mapTanhDeriv
    :: Int
    -> Ptr a -- ^ input
    -> Ptr a -- ^ output
    -> IO ()
  -- | Input and outputs must be aligned to 32 bit and must not overlap.
  mapSigmoidWithDeriv
    :: Int
    -> Ptr a -- ^ input
    -> Ptr a -- ^ nonlinearity output
    -> Ptr a -- ^ derivative output
    -> IO ()
  -- | Input and outputs must be aligned to 32 bit and must not overlap.
  mapTanhWithDeriv
    :: Int
    -> Ptr a -- ^ input
    -> Ptr a -- ^ nonlinearity output
    -> Ptr a -- ^ derivative output
    -> IO ()

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
  addVectors n = FF.addVectorsf (cuint n)
  addVectorsScaled n x (AlignedFloat c) y z = FF.addVectorsScaledf (cuint n) x c y z
  dotProduct n x y = AlignedFloat <$> FF.dotProductf (cuint n) x y
  mapExp n x y                = FF.mapExp              (cuint n) x y
  mapSigmoid n x y            = FF.mapSigmoid          (cuint n) x y
  mapSigmoidDeriv n x y       = FF.mapSigmoidDeriv     (cuint n) x y
  mapTanh n x y               = FF.mapTanh             (cuint n) x y
  mapTanhDeriv n x y          = FF.mapTanhDeriv        (cuint n) x y
  mapSigmoidWithDeriv n x y z = FF.mapSigmoidWithDeriv (cuint n) x y z
  mapTanhWithDeriv n x y z    = FF.mapTanhWithDeriv    (cuint n) x y z

instance Aligned AlignedDouble where
  gemv ord trans m n (AlignedDouble alpha) a lda x incx (AlignedDouble beta) y incy =
    DF.dgemv ord trans m n alpha a lda x incx beta y incy
  ger ord m n (AlignedDouble alpha) x incx y incy a lda =
    DF.dger ord m n alpha x incx y incy a lda
  gemm ord trans trans' m n k (AlignedDouble alpha) a lda b ldb (AlignedDouble beta) c ldc =
    DF.dgemm ord trans trans' m n k alpha a lda b ldb beta c ldc
  addVectors n = DF.addVectors (cuint n)
  addVectorsScaled n x (AlignedDouble c) y z = DF.addVectorsScaled (cuint n) x c y z
  dotProduct n x y = AlignedDouble <$> DF.dotProduct (cuint n) x y
  mapExp n x y                = DF.mapExp              (cuint n) x y
  mapSigmoid n x y            = DF.mapSigmoid          (cuint n) x y
  mapSigmoidDeriv n x y       = DF.mapSigmoidDeriv     (cuint n) x y
  mapTanh n x y               = DF.mapTanh             (cuint n) x y
  mapTanhDeriv n x y          = DF.mapTanhDeriv        (cuint n) x y
  mapSigmoidWithDeriv n x y z = DF.mapSigmoidWithDeriv (cuint n) x y z
  mapTanhWithDeriv n x y z    = DF.mapTanhWithDeriv    (cuint n) x y z

{-# INLINE cuint #-}
cuint :: Int -> CUInt
cuint = CUInt . fromIntegral
