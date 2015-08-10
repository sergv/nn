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
  [ matrixTests "PureMatrix" pureMatrixProxy
  , matrixTests "MatrixDouble" matrixDoubleProxy
  , matrixTests "UnboxMatrix" unboxMatrixProxy
  , matrixTests "UnboxMatrixWithTranspose" unboxMatrixWithTransposeProxy
  , matrixTests "OpenBlasMatrix" openBlasMatrixProxy
  ]

matrixTests
  :: forall k w v. (Vect k v, Matrix k w v)
  => (ElemConstraints k Double, Show (w Double), Eq (w Double), Show (v Double), Eq (v Double))
  => String
  -> Proxy w
  -> TestTree
matrixTests name _ = testGroup name
  [ testCase "rows" $
    MC.rows testMatrix @?= 3
  , testCase "columns" $
    MC.columns testMatrix @?= 2
    --- | 1 |                | 1 * 10, 1 * 20 |   | 10, 20 |
    --- | 2 | * [ 10, 20 ] = | 2 * 10, 2 * 20 | = | 20, 40 |
    --- | 3 |                | 3 * 10, 3 * 20 |   | 30, 60 |
  , testCase "outer product" $
    MC.outerProduct (ivec [1, 2, 3]) (ivec [10, 20]) @?= (imat [[10, 20], [20, 40], [30, 60]] :: w Double)
  , testCase "vecMulRight #1" $
    MC.vecMulRight testMatrix (ivec [1, 2]) @?= ivec [5, 11, 17]
  , testCase "vecMulRight #2" $
    MC.vecMulRight testMatrix2 (ivec [1, 2, 3]) @?= ivec [22, 28]
  , testCase "vecMulRight #3" $
    MC.vecMulRight testMatrix3 (ivec [1, 2, 3]) @?= (ivec [14, 32, 50] :: v Double)
  , testCase "transpose #1" $
    MC.transpose testMatrix @?= testMatrix2
  , testCase "transpose #2" $
    MC.transpose testMatrix2 @?= testMatrix
  , testCase "transpose #3" $
    MC.transpose testMatrix3 @?= imat [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
  , testCase "matrix addition" $
    testMatrix MC.|+| testMatrix @?= imat [[2, 4], [6, 8], [10, 12]]

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
      (imat [[1], [2], [3], [4], [5]] :: w Double)
      (imat [[1, 1, 1]])
      (imat [ [ fromIntegral r :: Double | _ <- [1..3 :: Int]]
            | r <- [1..5 :: Int]
            ])
  -- | 1 |                      | 1 1 1 1 1 1 1 |
  -- | 2 |                      | 2 2 2 2 2 2 2 |
  -- | 3 | * | 1 1 1 1 1 1 1| = | 3 3 3 3 3 3 3 |
  -- | 4 |                      | 4 4 4 4 4 4 4 |
  -- | 5 |                      | 5 5 5 5 5 5 5 |
  , matrixMultiplicationTests
      "matrix multiplication #3"
      (imat [[1], [2], [3], [4], [5]] :: w Double)
      (imat [[1, 1, 1, 1, 1, 1, 1]])
      (imat [ [ fromIntegral r :: Double | _ <- [1..7 :: Int]]
            | r <- [1..5 :: Int]
            ])
  , testCase "sum columns" $
    MC.sumColumns testMatrix @?= ivec [3, 7, 11]
  ]
  where
    testMatrix :: w Double
    testMatrix = imat [[1, 2], [3, 4], [5, 6]]
    testMatrix2 :: w Double
    testMatrix2 = imat [[1, 3, 5], [2, 4, 6]]
    testMatrix2' :: w Double
    testMatrix2' = imat [[1, 4, 5], [2, 3, 6]]
    testMatrix3 :: w Double
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

ivec :: (ElemConstraints k Double, Vect k v) => [Double] -> v Double
ivec = VC.fromList

imat :: (ElemConstraints k Double, Matrix k w v) => [[Double]] -> w Double
imat = MC.fromList
