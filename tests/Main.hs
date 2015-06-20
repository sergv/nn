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
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified NN as NN
import qualified NN.Specific as S
import qualified NN.Generic as G
import Nonlinearity
import Util

import Test.Tasty
import Test.Tasty.HUnit

import Data.Random.Distribution.Normal (stdNormal)
import Data.Random.Sample (sample)
import Data.Random.Source (MonadRandom)
import Data.Random.Source.PureMT (PureMT, pureMT)


default (Int)

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
  [ compareAdVsNumericalGradients
      "compare gradients from ad package and from numerical calculation by central differences #1"
      mkInput_1_to_1
      1
      []
      1
  , compareAdVsNumericalGradients
      "compare gradients from ad package and from numerical calculation by central differences #2"
      mkInput_1_to_1
      1
      [1]
      1
  , compareAdVsNumericalGradients
      "compare gradients from ad package and from numerical calculation by central differences #3"
      mkInput_1_to_1
      1
      [2]
      1
  , compareAdVsNumericalGradients
      "compare gradients from ad package and from numerical calculation by central differences #4"
      mkInput_1_to_1
      1
      [1, 1]
      1
  , compareAdVsNumericalGradients
      "compare gradients from ad package and from numerical calculation by central differences #5"
      mkInput_2_to_2
      2
      [4, 4]
      2

  , compareAdVsBackpropGradients
      "compare gradients from ad package and from backpropagation #1"
      mkInput_1_to_1
      1
      []
      1
  , compareAdVsBackpropGradients
      "compare gradients from ad package and from backpropagation #2"
      mkInput_1_to_1
      1
      [1]
      1
  , compareAdVsBackpropGradients
      "compare gradients from ad package and from backpropagation #3"
      mkInput_1_to_1
      1
      [2]
      1
  , compareAdVsBackpropGradients
      "compare gradients from ad package and from backpropagation #4"
      mkInput_1_to_1
      1
      [1, 1]
      1
  , compareAdVsBackpropGradients
      "compare gradients from ad package and from backpropagation #5"
      mkInput_2_to_2
      2
      [4, 4]
      2
  , compareAdVsBackpropGradients
      "compare gradients from ad package and from backpropagation #6"
      mkInput_5_to_5
      5
      []
      5
  , compareAdVsBackpropGradients
      "compare gradients from ad package and from backpropagation #7"
      mkInput_5_to_5
      5
      [10, 10]
      5
  ]
  where
    mkInput_1_to_1 k = ([2 * k], [k^2])
    mkInput_2_to_2 k = ([k, 2 * k], [10 * k, k^2])
    mkInput_5_to_5 k = ([k, k ** 0.5, k ** 0.3, k ** 0.25, k ** 0.2], [ k^n | n <- [1..5]])


compareAdVsNumericalGradients
  :: String
  -> (Double -> ([Double], [Double]))
  -> Int
  -> [Int]
  -> Int
  -> TestTree
compareAdVsNumericalGradients name mkInput inputLayerSize hiddenLayers finalLayerSize =
  testGroup name
    [ testGroup "ad vs numerical"
        [ compareGradients
            "Specific"
            mkVectorInput
            (makeSpecificNN inputLayerSize hiddenLayers finalLayerSize)
            S.targetFunctionGrad
            (S.targetFunctionGradNumerical epsilon)
        , testGroup "Generic"
            [ compareGradients
                "Vector"
                mkVectorInput
                (makeGenericVectorNN inputLayerSize hiddenLayers finalLayerSize)
                G.targetFunctionGrad
                (G.targetFunctionGradNumerical epsilon)
            , compareGradients
                "List"
                mkInput
                (makeGenericListNN inputLayerSize hiddenLayers finalLayerSize)
                G.targetFunctionGrad
                (G.targetFunctionGradNumerical epsilon)
            ]
        ]
    ]
  where
    epsilon = 1e-6
    mkVectorInput :: Double -> (Vector Double, Vector Double)
    mkVectorInput = ((V.fromList *** V.fromList) . mkInput)

compareAdVsBackpropGradients
  :: String
  -> (Double -> ([Double], [Double]))
  -> Int
  -> [Int]
  -> Int
  -> TestTree
compareAdVsBackpropGradients name mkInput inputLayerSize hiddenLayers finalLayerSize =
  testGroup "ad vs backpropagation"
    [ compareGradients
        name
        mkVectorInput
        (makeSpecificNN inputLayerSize hiddenLayers finalLayerSize)
        S.targetFunctionGrad -- (S.targetFunctionGradNumerical 1e-6)
        S.backprop
    ]
  where
    mkVectorInput :: Double -> (Vector Double, Vector Double)
    mkVectorInput = ((V.fromList *** V.fromList) . mkInput)

makeSpecificNN :: Int -> [Int] -> Int -> State PureMT (S.NN HyperbolicTangent Nonlinear Double)
makeSpecificNN inputLayerSize hiddenLayerSizes finalLayerSize =
  S.makeNN inputLayerSize hiddenLayerSizes finalLayerSize (sample stdNormal)
makeGenericVectorNN :: Int -> [Int] -> Int -> State PureMT (G.NN Vector HyperbolicTangent Nonlinear Double)
makeGenericVectorNN inputLayerSize hiddenLayerSizes finalLayerSize =
  G.makeNN inputLayerSize hiddenLayerSizes finalLayerSize (sample stdNormal)
makeGenericListNN :: Int -> [Int] -> Int -> State PureMT (G.NN [] HyperbolicTangent Nonlinear Double)
makeGenericListNN inputLayerSize hiddenLayerSizes finalLayerSize =
  G.makeNN inputLayerSize hiddenLayerSizes finalLayerSize (sample stdNormal)

compareGradients
  :: forall nn v. (Functor nn, Eq (nn ApproxEq), Show (nn ApproxEq))
  => String
  -> (Double -> (v Double, v Double))
  -> (State PureMT (nn Double))
  -> (Vector (v Double, v Double) -> nn Double -> (Double, Grad nn Double))
  -> (Vector (v Double, v Double) -> nn Double -> (Double, Grad nn Double))
  -> TestTree
compareGradients name mkInput mkNN targetFuncGrad targetFuncGrad' =
  testCase name $ do
    assertEqual "target function values do not match" x x'
    assertEqual "target function gradients do not match" grad grad'
  where
    nn :: nn Double
    nn = evalState mkNN mt
    mt :: PureMT
    mt = pureMT 0
    dataset :: Vector (v Double, v Double)
    dataset = V.fromList $ map mkInput [1] -- [1..10]
    x, x' :: ApproxEq
    grad, grad' :: Grad nn ApproxEq
    (x, grad)   = (ApproxEq *** fmap ApproxEq) $ targetFuncGrad dataset nn
    (x', grad') = (ApproxEq *** fmap ApproxEq) $ targetFuncGrad' dataset nn


main :: IO ()
main = do
  defaultMain tests
