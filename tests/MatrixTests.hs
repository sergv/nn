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

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module MatrixTests (tests) where

import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit

import Data.ConstrainedFunctor
import Data.MatrixClass (Matrix)
import qualified Data.MatrixClass as MC
import Data.Nonlinearity
import Data.OpenBlasMatrix (OpenBlasMatrix)
import Data.PureMatrix (PureMatrix)
import Data.SpecialisedFunction
import Data.UnboxMatrix (UnboxMatrix)
import Data.UnboxMatrixWithTranspose (UnboxMatrixWithTranspose)
import Data.VectClass (Vect)

import TestUtils

tests :: TestTree
tests = testGroup "Matrix tests"
  [ matrixTests "PureMatrix" pureMatrixProxy doubleProxy
  , matrixTests "UnboxMatrix" unboxMatrixProxy doubleProxy
  , matrixTests "UnboxMatrixWithTranspose" unboxMatrixWithTransposeProxy doubleProxy
  , matrixTests "OpenBlasMatrix, Double" openBlasMatrixProxy alignedDoubleProxy
  , matrixTests "OpenBlasMatrix, Float" openBlasMatrixProxy alignedFloatProxy
  ]

matrixTests
  :: forall w v a. (Vect v, Matrix w v)
  => (ElemConstraints v a, Show (w a), Eq (w a), Show (v a), Eq (v a))
  => (Show a, Eq a, Num a, Floating a)
  => (ConstrainedFunctor w, ElemConstraints w (ApproxRelEqFloating a))
  => (Eq (w (ApproxRelEqFloating a)), Show (w (ApproxRelEqFloating a)))
  => (SpecialisedFunction Exp (w a) (w a))
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

  , testCase "exp #1" $
    sfmap expProxy testMatrix @?~ cfmap exp testMatrix
  , testCase "exp #2" $
    sfmap expProxy testMatrix2 @?~ cfmap exp testMatrix2
  , testCase "exp #3" $
    sfmap expProxy testMatrix3 @?~ cfmap exp testMatrix3
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
  :: (Matrix w v, ElemConstraints w a, Num a, Eq (w a), Show (w a))
  => String
  -> w a
  -> w a
  -> w a
  -> TestTree
matrixMultiplicationTests name x y z = testGroup name
  [ testCase "Vanilla by vanilla" $
    MC.matrixMult x y @?= z
  , testCase "Transposed by vanilla" $
    MC.matrixMultByTransposedLeft (MC.transpose x) y @?= z
  , testCase "Vanilla by transposed" $
    MC.matrixMultByTransposedRight x (MC.transpose y) @?= z
  ]

pureMatrixProxy :: Proxy (PureMatrix [])
pureMatrixProxy = Proxy

unboxMatrixProxy :: Proxy UnboxMatrix
unboxMatrixProxy = Proxy

unboxMatrixWithTransposeProxy :: Proxy UnboxMatrixWithTranspose
unboxMatrixWithTransposeProxy = Proxy

openBlasMatrixProxy :: Proxy OpenBlasMatrix
openBlasMatrixProxy = Proxy
