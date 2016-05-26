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

import Data.Vector (Vector)
import Test.Tasty
import Test.Tasty.HUnit

import Util (splitVec)

tests :: TestTree
tests = testGroup "Other tests"
  [ vecTakeByTests
  ]

vecTakeByTests :: TestTree
vecTakeByTests = testGroup "splitVec"
  [ testCase "take by 1" $
    splitVec 1 xs @?= ([[1], [2], [3], [4]], [5])
  , testCase "take by 2" $
    splitVec 2 xs @?= ([[1, 2], [3, 4]], [5])
  , testCase "take by 3" $
    splitVec 3 xs @?= ([[1, 2, 3]], [4, 5])
  , testCase "take by 4" $
    splitVec 4 xs @?= ([[1, 2, 3, 4]], [5])
  , testCase "take by 5" $
    splitVec 5 xs @?= ([], [1, 2, 3, 4, 5])
  , testCase "take by 6" $
    splitVec 6 xs @?= ([], [1, 2, 3, 4, 5])
  ]
  where
    xs :: Vector Int
    xs = [1, 2, 3, 4, 5]
