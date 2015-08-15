----------------------------------------------------------------------------
-- |
-- Module      :  MatrixTests
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module MatrixTests (tests) where

import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit

import Data.Aligned.Double
import Data.Aligned.Float
import Data.MatrixDouble (MatrixDouble)
import Data.OpenBlasMatrix (OpenBlasMatrix)
import Data.PureMatrix (PureMatrix)
import Data.UnboxMatrix (UnboxMatrix)
import Data.UnboxMatrixWithTranspose (UnboxMatrixWithTranspose)
import Data.ConstrainedFunctor
import Data.MatrixClass (Matrix)
import qualified Data.MatrixClass as MC
import Data.VectClass (Vect)
import qualified Data.VectClass as VC

tests :: TestTree
tests = testGroup "Matrix tests"
  [ matrixTests "PureMatrix" pureMatrixProxy doubleProxy
  , matrixTests "MatrixDouble" matrixDoubleProxy doubleProxy
  , matrixTests "UnboxMatrix" unboxMatrixProxy doubleProxy
  , matrixTests "UnboxMatrixWithTranspose" unboxMatrixWithTransposeProxy doubleProxy
  , matrixTests "OpenBlasMatrix, Double" openBlasMatrixProxy alignedDoubleProxy
  , matrixTests "OpenBlasMatrix, Float" openBlasMatrixProxy alignedFloatProxy
  ]

matrixTests
  :: forall k w v a. (Vect k v, Matrix k w v)
  => (ElemConstraints k a, Show (w a), Eq (w a), Show (v a), Eq (v a), Show a, Eq a, Num a)
  => String
  -> Proxy w
  -> Proxy a
  -> TestTree
matrixTests name _ _ = testGroup name
  [ testCase "rows" $
    MC.rows testMatrix @?= 3
  , testCase "columns" $
    MC.columns testMatrix @?= 2
    --- | 1 |                | 1 * 10, 1 * 20 |   | 10, 20 |
    --- | 2 | * [ 10, 20 ] = | 2 * 10, 2 * 20 | = | 20, 40 |
    --- | 3 |                | 3 * 10, 3 * 20 |   | 30, 60 |
  , testCase "outer product" $
    MC.outerProduct (ivec [1, 2, 3]) (ivec [10, 20]) @?= (imat [[10, 20], [20, 40], [30, 60]] :: w a)
  , testCase "vecMulRight #1" $
    MC.vecMulRight testMatrix (ivec [1, 2]) @?= ivec [5, 11, 17]
  , testCase "vecMulRight #2" $
    MC.vecMulRight testMatrix2 (ivec [1, 2, 3]) @?= ivec [22, 28]
  , testCase "vecMulRight #3" $
    MC.vecMulRight testMatrix3 (ivec [1, 2, 3]) @?= (ivec [14, 32, 50] :: v a)
  , testCase "transpose #1" $
    MC.transpose testMatrix @?= testMatrix2
  , testCase "transpose #2" $
    MC.transpose testMatrix2 @?= testMatrix
  , testCase "transpose #3" $
    MC.transpose testMatrix3 @?= imat [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
  , testCase "matrix scaled addition" $
    MC.addScaled testMatrix 2 testMatrix @?= imat [[3, 6], [9, 12], [15, 18]]
  , testCase "matrix addition" $
    testMatrix MC.|+| testMatrix @?= imat [[2, 4], [6, 8], [10, 12]]
  , testCase "normL2Square" $
    MC.normL2Square testMatrix @?= 91

  -- | 1 2 |   | 1 4 5 |   | 5  10 17 |
  -- | 3 4 | * | 2 3 6 | = | 11 24 39 |
  -- | 5 6 |               | 17 38 61 |
  , matrixMultiplicationTests
      "matrix multiplication #1"
      testMatrix
      testMatrix2'
      (imat [[5, 10, 17], [11, 24, 39], [17, 38, 61]])
  -- | 1 |               | 1 1 1 |
  -- | 2 |               | 2 2 2 |
  -- | 3 | * | 1 1 1 | = | 3 3 3 |
  -- | 4 |               | 4 4 4 |
  -- | 5 |               | 5 5 5 |
  , matrixMultiplicationTests
      "matrix multiplication #2"
      (imat [[1], [2], [3], [4], [5]] :: w a)
      (imat [[1, 1, 1]])
      (imat [ [ fromIntegral r :: a | _ <- [1..3 :: Int]]
            | r <- [1..5 :: Int]
            ])
  -- | 1 |                      | 1 1 1 1 1 1 1 |
  -- | 2 |                      | 2 2 2 2 2 2 2 |
  -- | 3 | * | 1 1 1 1 1 1 1| = | 3 3 3 3 3 3 3 |
  -- | 4 |                      | 4 4 4 4 4 4 4 |
  -- | 5 |                      | 5 5 5 5 5 5 5 |
  , matrixMultiplicationTests
      "matrix multiplication #3"
      (imat [[1], [2], [3], [4], [5]] :: w a)
      (imat [[1, 1, 1, 1, 1, 1, 1]])
      (imat [ [ fromIntegral r :: a | _ <- [1..7 :: Int]]
            | r <- [1..5 :: Int]
            ])
  , testCase "sum columns" $
    MC.sumColumns testMatrix @?= ivec [3, 7, 11]
  ]
  where
    testMatrix :: w a
    testMatrix = imat [[1, 2], [3, 4], [5, 6]]
    testMatrix2 :: w a
    testMatrix2 = imat [[1, 3, 5], [2, 4, 6]]
    testMatrix2' :: w a
    testMatrix2' = imat [[1, 4, 5], [2, 3, 6]]
    testMatrix3 :: w a
    testMatrix3 = imat [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

matrixMultiplicationTests
  :: (Matrix k w v, ElemConstraints k a, Num a, Eq (w a), Show (w a))
  => String
  -> w a
  -> w a
  -> w a
  -> TestTree
matrixMultiplicationTests name x y z = testGroup name
  [ testCase "Vanilla by vanilla" $
    MC.matrixMult x y @?= z
  , testCase "Vanilla by transposed" $
    MC.matrixMultByTransposedLeft (MC.transpose x) y @?= z
  , testCase "Transposed by vanilla" $
    MC.matrixMultByTransposedRight x (MC.transpose y) @?= z
  ]

doubleProxy :: Proxy Double
doubleProxy = Proxy

alignedDoubleProxy :: Proxy AlignedDouble
alignedDoubleProxy = Proxy

alignedFloatProxy :: Proxy AlignedFloat
alignedFloatProxy = Proxy

pureMatrixProxy :: Proxy (PureMatrix [])
pureMatrixProxy = Proxy

matrixDoubleProxy :: Proxy MatrixDouble
matrixDoubleProxy = Proxy

unboxMatrixProxy :: Proxy UnboxMatrix
unboxMatrixProxy = Proxy

unboxMatrixWithTransposeProxy :: Proxy UnboxMatrixWithTranspose
unboxMatrixWithTransposeProxy = Proxy

openBlasMatrixProxy :: Proxy OpenBlasMatrix
openBlasMatrixProxy = Proxy

ivec :: (ElemConstraints k a, Vect k v) => [a] -> v a
ivec = VC.fromList

imat :: (ElemConstraints k a, Matrix k w v, Show a) => [[a]] -> w a
imat = MC.fromList
