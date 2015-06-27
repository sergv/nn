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

import Test.Tasty
import Test.Tasty.HUnit

import Data.PureMatrix (PureMatrix(..))
import qualified Data.MatrixClass as MC

tests :: TestTree
tests = testGroup "PureMatrix tests"
  [ --- | 1 |                | 1 * 10, 1 * 20 |   | 10, 20 |
    --- | 2 | * [ 10, 20 ] = | 2 * 10, 2 * 20 | = | 20, 40 |
    --- | 3 |                | 3 * 10, 3 * 20 |   | 30, 60 |
    testCase "outer product #1" $
    MC.outerProduct [1 :: Int, 2, 3] [10, 20] @=? PureMatrix 3 2 [[10, 20], [20, 40], [30, 60]]
  , testCase "vecMulRight #1" $
    MC.vecMulRight testMatrix [1, 2] @=? [5, 11, 17]
  , testCase "vecMulLeft #1" $
    MC.vecMulLeft [1, 2, 3] testMatrix @=? [22, 28]
  ]
  where
    testMatrix :: PureMatrix [] Int
    testMatrix = PureMatrix 3 2 [[1, 2], [3, 4], [5, 6]]

