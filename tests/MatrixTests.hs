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

module MatrixTests (tests) where

import qualified Data.Vector.Unboxed as U
import Test.Tasty
import Test.Tasty.HUnit

import Data.PureMatrix (PureMatrix(..))
import Data.UnboxMatrix (UnboxMatrix(..))
import qualified Data.MatrixClass as MC

tests :: TestTree
tests = testGroup "Matrix tests"
  [ pureMatrixTests
  , unboxMatrixTests
  ]

pureMatrixTests :: TestTree
pureMatrixTests = testGroup "PureMatrix"
  [ --- | 1 |                | 1 * 10, 1 * 20 |   | 10, 20 |
    --- | 2 | * [ 10, 20 ] = | 2 * 10, 2 * 20 | = | 20, 40 |
    --- | 3 |                | 3 * 10, 3 * 20 |   | 30, 60 |
    testCase "outer product #1" $
    MC.outerProduct [1 :: Int, 2, 3] [10, 20] @?= PureMatrix 3 2 [[10, 20], [20, 40], [30, 60]]
  , testCase "vecMulRight #1" $
    MC.vecMulRight testMatrix [1, 2] @?= [5, 11, 17]
  , testCase "transpose #1" $
    MC.transpose testMatrix @?= testMatrix2
  , testCase "transpose #2" $
    MC.transpose testMatrix2 @?= testMatrix
  , testCase "transpose #3" $
    MC.transpose testMatrix3 @?= PureMatrix 3 3 [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
  ]
  where
    testMatrix :: PureMatrix [] Int
    testMatrix = PureMatrix 3 2 [[1, 2], [3, 4], [5, 6]]
    testMatrix2 :: PureMatrix [] Int
    testMatrix2 = PureMatrix 2 3 [[1, 3, 5], [2, 4, 6]]

    testMatrix3 :: PureMatrix [] Int
    testMatrix3 = PureMatrix 3 3 [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

unboxMatrixTests :: TestTree
unboxMatrixTests = testGroup "UnboxMatrix"
  [ testCase "outer product #1" $
    MC.outerProduct (vec [1 :: Int, 2, 3]) (vec [10, 20]) @?= UnboxMatrix 3 2 (vec [10, 20, 20, 40, 30, 60])
  , testCase "vecMulRight #1" $
    MC.vecMulRight testMatrix (vec [1, 2]) @?= (vec [5, 11, 17])
  , testCase "transpose #1" $
    MC.transpose testMatrix @?= testMatrix2
  , testCase "transpose #2" $
    MC.transpose testMatrix2 @?= testMatrix
  , testCase "transpose #3" $
    MC.transpose testMatrix3 @?= UnboxMatrix 3 3 (vec [1, 4, 7, 2, 5, 8, 3, 6, 9])
  ]
  where
    vec = U.fromList
    testMatrix :: UnboxMatrix Int
    testMatrix = UnboxMatrix 3 2 $ vec [1, 2, 3, 4, 5, 6]
    testMatrix2 :: UnboxMatrix Int
    testMatrix2 = UnboxMatrix 2 3 $ vec [1, 3, 5, 2, 4, 6]

    testMatrix3 :: UnboxMatrix Int
    testMatrix3 = UnboxMatrix 3 3 $ vec [1, 2, 3, 4, 5, 6, 7, 8, 9]

