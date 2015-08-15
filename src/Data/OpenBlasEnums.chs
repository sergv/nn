module Data.OpenBlasEnums where

import Foreign.C

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
