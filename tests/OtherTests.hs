----------------------------------------------------------------------------
-- |
-- Module      :  OtherTests
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedLists #-}

module OtherTests (tests) where

import Data.Vector ()
import Test.Tasty
import Test.Tasty.HUnit

import Util (vecTakeBy)

tests :: TestTree
tests = testGroup "Other tests"
  [ vecTakeByTests
  ]

vecTakeByTests :: TestTree
vecTakeByTests = testGroup "vecTakeBy"
  [ testCase "take by 1" $
    vecTakeBy 1 xs @?= [[1], [2], [3], [4], [5]]
  , testCase "take by 2" $
    vecTakeBy 2 xs @?= [[1, 2], [3, 4], [5]]
  , testCase "take by 3" $
    vecTakeBy 3 xs @?= [[1, 2, 3], [4, 5]]
  , testCase "take by 4" $
    vecTakeBy 4 xs @?= [[1, 2, 3, 4], [5]]
  , testCase "take by 5" $
    vecTakeBy 5 xs @?= [[1, 2, 3, 4, 5]]
  , testCase "take by 6" $
    vecTakeBy 6 xs @?= [[1, 2, 3, 4, 5]]
  ]
  where
    xs = [1, 2, 3, 4, 5]
