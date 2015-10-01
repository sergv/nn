----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aligned.Float.Foreign
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

module Data.Aligned.Float.Foreign where

import Foreign
import Foreign.C.Types

import Data.Aligned.Float
import Data.OpenBlasEnums

-- Matrix-vector multiplication
foreign import ccall unsafe "cblas_sgemv" sgemv
  :: BlasOrder
  -> BlasTranspose
  -> Size
  -> Size
  -> Float
  -> Ptr AlignedFloat
  -> Size
  -> Ptr AlignedFloat
  -> BlasInt
  -> Float
  -> Ptr AlignedFloat
  -> BlasInt
  -> IO ()

-- Outer product
foreign import ccall unsafe "cblas_sger" sger
  :: BlasOrder
  -> Size
  -> Size
  -> Float
  -> Ptr AlignedFloat
  -> BlasInt
  -> Ptr AlignedFloat
  -> BlasInt
  -> Ptr AlignedFloat
  -> Size
  -> IO ()

-- Matrix multiplication
foreign import ccall unsafe "cblas_sgemm" sgemm
  :: BlasOrder
  -> BlasTranspose
  -> BlasTranspose
  -> Size
  -> Size
  -> Size
  -> Float
  -> Ptr AlignedFloat
  -> Size
  -> Ptr AlignedFloat
  -> Size
  -> Float
  -> Ptr AlignedFloat
  -> Size
  -> IO ()

foreign import ccall unsafe "addf" addVectorsf
  :: CUInt
  -> Ptr AlignedFloat
  -> Ptr AlignedFloat
  -> Ptr AlignedFloat
  -> IO ()

foreign import ccall unsafe "addScaledf" addVectorsScaledf
  :: CUInt
  -> Ptr AlignedFloat
  -> Float
  -> Ptr AlignedFloat
  -> Ptr AlignedFloat
  -> IO ()

foreign import ccall unsafe "dotf" dotProductf
  :: CUInt
  -> Ptr AlignedFloat
  -> Ptr AlignedFloat
  -> IO Float

foreign import ccall unsafe "map_expf" mapExp
  :: CUInt
  -> Ptr AlignedFloat
  -> Ptr AlignedFloat
  -> IO ()

foreign import ccall unsafe "map_sigmoidf" mapSigmoid
  :: CUInt
  -> Ptr AlignedFloat
  -> Ptr AlignedFloat
  -> IO ()

foreign import ccall unsafe "map_sigmoid_derivf" mapSigmoidDeriv
  :: CUInt
  -> Ptr AlignedFloat
  -> Ptr AlignedFloat
  -> IO ()

foreign import ccall unsafe "map_tanhf" mapTanh
  :: CUInt
  -> Ptr AlignedFloat
  -> Ptr AlignedFloat
  -> IO ()

foreign import ccall unsafe "map_tanh_derivf" mapTanhDeriv
  :: CUInt
  -> Ptr AlignedFloat
  -> Ptr AlignedFloat
  -> IO ()


foreign import ccall unsafe "map_sigmoid_with_derivf" mapSigmoidWithDeriv
  :: CUInt
  -> Ptr AlignedFloat
  -> Ptr AlignedFloat
  -> Ptr AlignedFloat
  -> IO ()

foreign import ccall unsafe "map_tanh_with_derivf" mapTanhWithDeriv
  :: CUInt
  -> Ptr AlignedFloat
  -> Ptr AlignedFloat
  -> Ptr AlignedFloat
  -> IO ()
