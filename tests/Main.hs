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

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Arrow
import Control.Monad.State
import Data.Random.Source.PureMT (PureMT, pureMT)
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified NN as NN
import qualified NN.Specific as S
import qualified NN.Generic as G
import Nonlinearity
import Util

import Test.Tasty
import Test.Tasty.HUnit

newtype ApproxEq = ApproxEq Double

machineEps :: Double
machineEps = 1.11022302462516e-16

eps :: Double
eps = sqrt $ sqrt machineEps

instance Eq ApproxEq where
  ApproxEq x == ApproxEq y = abs (x - y) <= eps

instance Show ApproxEq where
  show (ApproxEq x) = show x

tests :: TestTree
tests = testGroup "Neural network tests"
  [ nnTests
  ]

nnTests :: TestTree
nnTests = testGroup "NN.Specific tests"
  [ compareSpecificGradients
      "compare gradients from ad package and from numerical calculation by central differences #1"
      (\k -> ([2 * k], [k^2]))
      1
      []
      1
  , compareSpecificGradients
      "compare gradients from ad package and from numerical calculation by central differences #2"
      (\k -> ([2 * k], [k^2]))
      1
      [1]
      1
  , compareSpecificGradients
      "compare gradients from ad package and from numerical calculation by central differences #3"
      (\k -> ([2 * k], [k^2]))
      1
      [2]
      1
  , compareSpecificGradients
      "compare gradients from ad package and from numerical calculation by central differences #4"
      (\k -> ([2 * k], [k^2]))
      1
      [1, 1]
      1
  , compareSpecificGradients
      "compare gradients from ad package and from numerical calculation by central differences #5"
      (\k -> ([k, 2 * k], [10 * k, k^2]))
      2
      [4, 4]
      2
  , compareSpecificGradients
      "compare gradients from ad package and from numerical calculation by central differences #6"
      (\k -> ([k, k ** 0.5, k ** 0.3, k ** 0.25, k ** 0.2], [ k^n | n <- [1..5]]))
      5
      [10, 10]
      5
  ]


compareSpecificGradients
  :: String
  -> (Double -> ([Double], [Double]))
  -> Int
  -> [Int]
  -> Int
  -> TestTree
compareSpecificGradients name mkInput inputLayerSize hiddenLayers finalLayerSize =
  testGroup name
    [ compareGradients
        "Specific"
        mkVectorInput
        (makeSpecificNN inputLayerSize hiddenLayers finalLayerSize)
        S.targetFunctionGrad
        S.targetFunctionGradNumerical
    , testGroup "Generic"
        [ compareGradients
            "Vector"
            mkVectorInput
            (makeGenericVectorNN inputLayerSize hiddenLayers finalLayerSize)
            G.targetFunctionGrad
            G.targetFunctionGradNumerical
        , compareGradients
            "List"
            mkInput
            (makeGenericListNN inputLayerSize hiddenLayers finalLayerSize)
            G.targetFunctionGrad
            G.targetFunctionGradNumerical
        ]
    ]
  where
    mkVectorInput :: Double -> (Vector Double, Vector Double)
    mkVectorInput = ((V.fromList *** V.fromList) . mkInput)
    makeSpecificNN :: Int -> [Int] -> Int -> State PureMT (S.NN Sigmoid Nonlinear Double)
    makeSpecificNN = S.makeNN
    makeGenericVectorNN :: Int -> [Int] -> Int -> State PureMT (G.NN Vector Sigmoid Nonlinear Double)
    makeGenericVectorNN = G.makeNN
    makeGenericListNN :: Int -> [Int] -> Int -> State PureMT (G.NN [] Sigmoid Nonlinear Double)
    makeGenericListNN = G.makeNN

compareGradients
  :: forall nn v. (Functor nn, Eq (nn ApproxEq), Show (nn ApproxEq))
  => String
  -> (Double -> (v Double, v Double))
  -> (State PureMT (nn Double))
  -> (Vector (v Double, v Double) -> nn Double -> (Double, Grad nn Double))
  -> (Double -> Vector (v Double, v Double) -> nn Double -> (Double, Grad nn Double))
  -> TestTree
compareGradients name mkInput mkNN targetFuncGrad targetFuncGradNumerical =
  testCase name $ do
    assertEqual "target function values do not match" x x'
    assertEqual "target function gradients do not match" grad grad'
  where
    nn :: nn Double
    nn = evalState mkNN mt
    mt :: PureMT
    mt = pureMT 0
    dataset :: Vector (v Double, v Double)
    dataset = V.fromList $ map mkInput [1..10]
    x, x' :: ApproxEq
    grad, grad' :: Grad nn ApproxEq
    (x, grad)   = (ApproxEq *** fmap ApproxEq) $ targetFuncGrad dataset nn
    (x', grad') = (ApproxEq *** fmap ApproxEq) $ targetFuncGradNumerical 1e-6 dataset nn


main :: IO ()
main = do
  defaultMain tests
