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

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Arrow
import Control.Monad.State
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Text.PrettyPrint.Leijen.Text (Pretty(..))
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.MatrixDouble (MatrixDouble)
import Data.OpenBlasMatrix (OpenBlasMatrix)
import Data.PureMatrix (PureMatrix)
import Data.StorableVectorDouble (StorableVectorDouble)
import qualified Data.StorableVectorDouble as SVD
import Data.VectorDouble (VectorDouble)
import Data.UnboxMatrix (UnboxMatrix)
import Data.UnboxMatrixWithTranspose (UnboxMatrixWithTranspose)
import qualified Data.VectorDouble as VD
import qualified Data.Text.Lazy as T
import Data.ConstrainedFunctor
import NN (NNVectorLike)
import qualified NN
import qualified NN.Specific as S
import qualified NN.Generic as G
import Nonlinearity
import Util

import Test.Tasty
import Test.Tasty.HUnit

import Data.Random.Distribution.Normal (stdNormal)
import Data.Random.Sample (sample)
import Data.Random.Source.PureMT (PureMT, pureMT)

import qualified MatrixTests
import qualified OtherTests

default (Int)

newtype ApproxEq = ApproxEq Double

machineEps :: Double
machineEps = 1.11022302462516e-16

eps :: Double
eps = sqrt $ sqrt machineEps

chunkSize :: Int
chunkSize = 2

instance Eq ApproxEq where
  ApproxEq x == ApproxEq y = abs (x - y) <= eps

instance Show ApproxEq where
  show (ApproxEq x) = show x

instance Pretty ApproxEq where
  pretty (ApproxEq x) = pretty x

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

  , compareGradientsFromDifferentSources
      "compare gradients from different sources #1"
      mkInput_1_to_1
      1
      []
      1
  , compareGradientsFromDifferentSources
      "compare gradients from different sources #2"
      mkInput_1_to_1
      1
      [1]
      1
  , compareGradientsFromDifferentSources
      "compare gradients from different sources #3"
      mkInput_1_to_1
      1
      [2]
      1
  , compareGradientsFromDifferentSources
      "compare gradients from different sources #4"
      mkInput_1_to_1
      1
      [1, 1]
      1
  , compareGradientsFromDifferentSources
      "compare gradients from different sources #5"
      mkInput_2_to_2
      2
      [4, 4]
      2
  , compareGradientsFromDifferentSources
      "compare gradients from different sources #6"
      mkInput_5_to_5
      5
      []
      5
  , compareGradientsFromDifferentSources
      "compare gradients from different sources #7"
      mkInput_5_to_5
      5
      [10, 10]
      5
  , compareGradientsFromDifferentSources
      "compare gradients from different sources #8"
      mkInput_5_to_5
      5
      (replicate 10 10)
      5
  ]
  where
    -- mkInput_1_to_1 k = ([2 * k], [k^2])
    mkInput_1_to_1 k = ([k], [k])
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
            (mkVectorInput mkInput)
            (makeSpecificNN inputLayerSize hiddenLayers finalLayerSize)
            S.targetFunctionGrad
            (S.targetFunctionGradNumerical epsilon)
        , testGroup "Generic"
            [ compareGradients
                "Vector"
                (mkVectorInput mkInput)
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

compareGradientsFromDifferentSources
  :: String
  -> (Double -> ([Double], [Double]))
  -> Int
  -> [Int]
  -> Int
  -> TestTree
compareGradientsFromDifferentSources name mkInput inputLayerSize hiddenLayers finalLayerSize =
  testGroup name
  [ testGroup "ad vs backpropagation, same NN type"
      [ compareGradients
          "Specific"
          (mkVectorInput mkInput)
          (makeSpecificNN inputLayerSize hiddenLayers finalLayerSize)
          S.targetFunctionGrad
          S.backprop
      , testGroup "Generic"
          [ compareGradients
              "Vector, PureMatrix, chunkSize = 1"
              (mkVectorInput mkInput)
              (makeGenericVectorNN inputLayerSize hiddenLayers finalLayerSize)
              -- (G.targetFunctionGradNumerical epsilon)
              G.targetFunctionGrad
              (G.backprop 1)
          , compareGradients
              ("Vector, PureMatrix, chunkSize = " ++ show chunkSize)
              (mkVectorInput mkInput)
              (makeGenericVectorNN inputLayerSize hiddenLayers finalLayerSize)
              -- (G.targetFunctionGradNumerical epsilon)
              G.targetFunctionGrad
              (G.backprop chunkSize)
          , compareGradients
              "List, PureMatrix"
              mkInput
              (makeGenericListNN inputLayerSize hiddenLayers finalLayerSize)
              -- (G.targetFunctionGradNumerical epsilon)
              G.targetFunctionGrad
              (G.backprop chunkSize)
          ]
      ]
  , testGroup "backpropagation, different nn types"
      [ compareNNGradients
          "Specific vs Generic Vector"
          (mkVectorInput mkInput)
          (makeSpecificNN inputLayerSize hiddenLayers finalLayerSize)
          S.backprop
          (mkVectorInput mkInput)
          (makeGenericVectorNN inputLayerSize hiddenLayers finalLayerSize)
          (G.backprop chunkSize)
      , compareNNGradients
          "Specific vs Generic MatrixDouble"
          (mkVectorInput mkInput)
          (makeSpecificNN inputLayerSize hiddenLayers finalLayerSize)
          S.backprop
          (mkVectorDoubleInput mkInput)
          (makeUnboxedDoubleNN inputLayerSize hiddenLayers finalLayerSize)
          (G.backprop chunkSize)
      , compareNNGradients
          "Specific vs Generic UnboxMatrix"
          (mkVectorInput mkInput)
          (makeSpecificNN inputLayerSize hiddenLayers finalLayerSize)
          S.backprop
          (mkUnboxedVectorInput mkInput)
          (makeUnboxMatrixNN inputLayerSize hiddenLayers finalLayerSize)
          (G.backprop chunkSize)
      , compareNNGradients
          "Specific vs Generic UnboxMatrixWithTranspose"
          (mkVectorInput mkInput)
          (makeSpecificNN inputLayerSize hiddenLayers finalLayerSize)
          S.backprop
          (mkUnboxedVectorInput mkInput)
          (makeUnboxMatrixWithTransposeNN inputLayerSize hiddenLayers finalLayerSize)
          (G.backprop chunkSize)
      , compareNNGradients
          "Specific vs Generic OpenBlasMatrix"
          (mkVectorInput mkInput)
          (makeSpecificNN inputLayerSize hiddenLayers finalLayerSize)
          S.backprop
          (mkStorableVectorDoubleInput mkInput)
          (makeOpenBlasMatrixNN inputLayerSize hiddenLayers finalLayerSize)
          (G.backprop chunkSize)
      -- , compareNNGradients
      --     "Specific vs Generic MatrixDouble, specialized backprop"
      --     (mkVectorInput mkInput)
      --     (makeSpecificNN inputLayerSize hiddenLayers finalLayerSize)
      --     S.backprop
      --     (mkVectorDoubleInput mkInput)
      --     (makeUnboxedDoubleNN inputLayerSize hiddenLayers finalLayerSize)
      --     G.backprop'
      ]
  ]

epsilon :: Double
epsilon = 1e-6

makeSpecificNN :: Int -> [Int] -> Int -> State PureMT (S.NN HyperbolicTangent Nonlinear Double)
makeSpecificNN inputLayerSize hiddenLayerSizes finalLayerSize =
  S.makeNN inputLayerSize hiddenLayerSizes finalLayerSize (sample stdNormal)
makeGenericVectorNN :: Int -> [Int] -> Int -> State PureMT (G.NN (PureMatrix Vector) Vector HyperbolicTangent Nonlinear Double)
makeGenericVectorNN inputLayerSize hiddenLayerSizes finalLayerSize =
  G.makeNN inputLayerSize hiddenLayerSizes finalLayerSize (sample stdNormal)
makeGenericListNN :: Int -> [Int] -> Int -> State PureMT (G.NN (PureMatrix []) [] HyperbolicTangent Nonlinear Double)
makeGenericListNN inputLayerSize hiddenLayerSizes finalLayerSize =
  G.makeNN inputLayerSize hiddenLayerSizes finalLayerSize (sample stdNormal)
makeUnboxedDoubleNN :: Int -> [Int] -> Int -> State PureMT (G.NN MatrixDouble VectorDouble HyperbolicTangent Nonlinear Double)
makeUnboxedDoubleNN inputLayerSize hiddenLayerSizes finalLayerSize =
  G.makeNN inputLayerSize hiddenLayerSizes finalLayerSize (sample stdNormal)
makeUnboxMatrixNN :: Int -> [Int] -> Int -> State PureMT (G.NN UnboxMatrix U.Vector HyperbolicTangent Nonlinear Double)
makeUnboxMatrixNN inputLayerSize hiddenLayerSizes finalLayerSize =
  G.makeNN inputLayerSize hiddenLayerSizes finalLayerSize (sample stdNormal)
makeUnboxMatrixWithTransposeNN :: Int -> [Int] -> Int -> State PureMT (G.NN UnboxMatrixWithTranspose U.Vector HyperbolicTangent Nonlinear Double)
makeUnboxMatrixWithTransposeNN inputLayerSize hiddenLayerSizes finalLayerSize =
  G.makeNN inputLayerSize hiddenLayerSizes finalLayerSize (sample stdNormal)
makeOpenBlasMatrixNN :: Int -> [Int] -> Int -> State PureMT (G.NN OpenBlasMatrix StorableVectorDouble HyperbolicTangent Nonlinear Double)
makeOpenBlasMatrixNN inputLayerSize hiddenLayerSizes finalLayerSize =
  G.makeNN inputLayerSize hiddenLayerSizes finalLayerSize (sample stdNormal)

mkVectorInput
  :: (Double -> ([Double], [Double]))
  -> Double -> (Vector Double, Vector Double)
mkVectorInput mkInput = (V.fromList *** V.fromList) . mkInput

mkVectorDoubleInput
  :: (Double -> ([Double], [Double]))
  -> Double -> (VectorDouble Double, VectorDouble Double)
mkVectorDoubleInput mkInput = (VD.fromList *** VD.fromList) . mkInput

mkUnboxedVectorInput
  :: (Double -> ([Double], [Double]))
  -> Double -> (U.Vector Double, U.Vector Double)
mkUnboxedVectorInput mkInput = (U.fromList *** U.fromList) . mkInput

mkStorableVectorDoubleInput
  :: (Double -> ([Double], [Double]))
  -> Double -> (StorableVectorDouble Double, StorableVectorDouble Double)
mkStorableVectorDoubleInput mkInput = (SVD.fromList *** SVD.fromList) . mkInput


compareGradients
  :: forall k nn v. (NNVectorLike k nn Double, Pretty (nn Double), ElemConstraints k Double)
  => String
  -> (Double -> (v Double, v Double))
  -> (State PureMT (nn Double))
  -> (Vector (v Double, v Double) -> nn Double -> (Double, Grad nn Double))
  -> (Vector (v Double, v Double) -> nn Double -> (Double, Grad nn Double))
  -> TestTree
compareGradients name mkInput mkNN targetFuncGrad targetFuncGrad' =
  compareNNGradients name mkInput mkNN targetFuncGrad mkInput mkNN targetFuncGrad'

compareNNGradients
  :: forall nn nn' k k' v v'.
     (NNVectorLike k nn Double, Pretty (nn Double), ElemConstraints k Double)
  => (NNVectorLike k' nn' Double, Pretty (nn' Double), ElemConstraints k' Double)
  => String
  -> (Double -> (v Double, v Double))
  -> (State PureMT (nn Double))
  -> (Vector (v Double, v Double) -> nn Double -> (Double, Grad nn Double))
  -> (Double -> (v' Double, v' Double))
  -> (State PureMT (nn' Double))
  -> (Vector (v' Double, v' Double) -> nn' Double -> (Double, Grad nn' Double))
  -> TestTree
compareNNGradients name mkInput mkNN targetFuncGrad mkInput' mkNN' targetFuncGrad' =
  testCase name $ do
    assertEqual "target function values do not match" (ApproxEq x) (ApproxEq x')
    unless (gradList == gradList') $
      assertFailure $ display' $
        "target function gradients do not match" PP.<$>
        "expected:    " <> PP.text (T.pack $ show gradList) PP.<$>
        "expected nn: " <> pretty nn PP.<$>
        PP.empty PP.<$>
        "got:         " <> PP.text (T.pack $ show gradList') PP.<$>
        "got nn:      " <> pretty nn'
  where
    nn :: nn Double
    nn = evalState mkNN mt
    nn' :: nn' Double
    nn' = evalState mkNN' mt
    mt :: PureMT
    mt = pureMT 0
    dataset :: Vector (v Double, v Double)
    dataset = V.fromList $ map mkInput [1] -- [1..10]
    dataset' :: Vector (v' Double, v' Double)
    dataset' = V.fromList $ map mkInput' [1] -- [1..10]
    (x, grad)   = targetFuncGrad dataset nn
    (x', grad') = targetFuncGrad' dataset' nn'

    gradList  = fmap (fmap (fmap ApproxEq)) $ NN.toWeightList $ getGrad grad
    gradList' = fmap (fmap (fmap ApproxEq)) $ NN.toWeightList $ getGrad grad'

allTests :: TestTree
allTests = testGroup "all tests"
  [ tests
  , MatrixTests.tests
  , OtherTests.tests
  ]

main :: IO ()
main = do
  defaultMain allTests

