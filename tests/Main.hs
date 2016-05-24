----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Main (main) where

import Test.Tasty

import qualified MatrixTests
import qualified NNTests
import qualified OtherTests
import qualified VectorTests

default (Int)

allTests :: TestTree
allTests = testGroup "all tests"
  [ NNTests.tests
  , MatrixTests.tests
  , VectorTests.tests
  , OtherTests.tests
  ]

main :: IO ()
main = defaultMain allTests

