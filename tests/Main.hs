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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main where

import Control.Arrow
import Control.Monad.State
import Data.Monoid
import qualified Data.Text.Lazy as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Text.PrettyPrint.Leijen.Text (Pretty(..))
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.Aligned.Double
import Data.AlignedStorableVector (AlignedStorableVector)
import qualified Data.AlignedStorableVector as ASV
import Data.ConstrainedFunctor
import Data.Grad
import Data.Nonlinearity
import Data.OpenBlasMatrix (OpenBlasMatrix)
import Data.PureMatrix (PureMatrix)
import Data.UnboxMatrix (UnboxMatrix)
import Data.UnboxMatrixWithTranspose (UnboxMatrixWithTranspose)
import qualified NN
import NN (NNVectorLike)
import qualified NN.Generic as G
import qualified NN.Specific as S
import Util

import Test.Tasty
import Test.Tasty.HUnit

import Data.Random.Distribution.Normal (stdNormal)
import Data.Random.Sample (sample)
import Data.Random.Source.PureMT (PureMT, pureMT)

import qualified MatrixTests
import qualified OtherTests
import qualified VectorTests

default (Int)

data ApproxEq = forall a. (Show a, Pretty a, ToDouble a) => ApproxEq a

machineEps :: Double
machineEps = 1.11022302462516e-16

eps :: Double
eps = sqrt $ sqrt machineEps

chunkSize :: Int
chunkSize = 2

instance Eq ApproxEq where
  ApproxEq x == ApproxEq y = abs (toDouble x - toDouble y) <= eps

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
          (mkAlignedStorableVectorInput mkInput)
          (makeOpenBlasMatrixNN inputLayerSize hiddenLayers finalLayerSize)
          (G.backprop chunkSize)
      ]
  ]

epsilon :: Double
epsilon = 1e-6

makeSpecificNN :: Int -> [Int] -> Int -> State PureMT (S.NN HyperbolicTangent HyperbolicTangent Double)
makeSpecificNN inputLayerSize hiddenLayerSizes finalLayerSize =
  S.makeNN inputLayerSize hiddenLayerSizes finalLayerSize (sample stdNormal)
makeGenericVectorNN :: Int -> [Int] -> Int -> State PureMT (G.NN (PureMatrix Vector) Vector HyperbolicTangent HyperbolicTangent Double)
makeGenericVectorNN inputLayerSize hiddenLayerSizes finalLayerSize =
  G.makeNN inputLayerSize hiddenLayerSizes finalLayerSize (sample stdNormal)
makeGenericListNN :: Int -> [Int] -> Int -> State PureMT (G.NN (PureMatrix []) [] HyperbolicTangent HyperbolicTangent Double)
makeGenericListNN inputLayerSize hiddenLayerSizes finalLayerSize =
  G.makeNN inputLayerSize hiddenLayerSizes finalLayerSize (sample stdNormal)
makeUnboxMatrixNN :: Int -> [Int] -> Int -> State PureMT (G.NN UnboxMatrix U.Vector HyperbolicTangent HyperbolicTangent Double)
makeUnboxMatrixNN inputLayerSize hiddenLayerSizes finalLayerSize =
  G.makeNN inputLayerSize hiddenLayerSizes finalLayerSize (sample stdNormal)
makeUnboxMatrixWithTransposeNN :: Int -> [Int] -> Int -> State PureMT (G.NN UnboxMatrixWithTranspose U.Vector HyperbolicTangent HyperbolicTangent Double)
makeUnboxMatrixWithTransposeNN inputLayerSize hiddenLayerSizes finalLayerSize =
  G.makeNN inputLayerSize hiddenLayerSizes finalLayerSize (sample stdNormal)
makeOpenBlasMatrixNN :: Int -> [Int] -> Int -> State PureMT (G.NN OpenBlasMatrix AlignedStorableVector HyperbolicTangent HyperbolicTangent AlignedDouble)
makeOpenBlasMatrixNN inputLayerSize hiddenLayerSizes finalLayerSize =
  G.makeNN inputLayerSize hiddenLayerSizes finalLayerSize (AlignedDouble <$> sample stdNormal)

mkVectorInput
  :: (Double -> ([Double], [Double]))
  -> Double -> (Vector Double, Vector Double)
mkVectorInput mkInput = (V.fromList *** V.fromList) . mkInput

mkUnboxedVectorInput
  :: (Double -> ([Double], [Double]))
  -> Double -> (U.Vector Double, U.Vector Double)
mkUnboxedVectorInput mkInput = (U.fromList *** U.fromList) . mkInput

mkAlignedStorableVectorInput
  :: (Double -> ([Double], [Double]))
  -> Double -> (AlignedStorableVector AlignedDouble, AlignedStorableVector AlignedDouble)
mkAlignedStorableVectorInput mkInput =
  (ASV.fromList *** ASV.fromList) .
  (map AlignedDouble *** map AlignedDouble) .
  mkInput


compareGradients
  :: forall nn v. (NNVectorLike nn Double, Pretty (nn Double), ElemConstraints nn Double)
  => String
  -> (Double -> (v Double, v Double))
  -> (State PureMT (nn Double))
  -> (Vector (v Double, v Double) -> nn Double -> (Double, Grad nn Double))
  -> (Vector (v Double, v Double) -> nn Double -> (Double, Grad nn Double))
  -> TestTree
compareGradients name mkInput mkNN targetFuncGrad targetFuncGrad' =
  compareNNGradients name mkInput mkNN targetFuncGrad mkInput mkNN targetFuncGrad'

compareNNGradients
  :: forall nn nn' v v' a b.
     (NNVectorLike nn a, Pretty (nn a), ElemConstraints nn a)
  => (NNVectorLike nn' b, Pretty (nn' b), ElemConstraints nn' b)
  => (Show a, Pretty a, ToDouble a, Show b, Pretty b, ToDouble b)
  => String
  -> (Double -> (v a, v a))
  -> (State PureMT (nn a))
  -> (Vector (v a, v a) -> nn a -> (a, Grad nn a))
  -> (Double -> (v' b, v' b))
  -> (State PureMT (nn' b))
  -> (Vector (v' b, v' b) -> nn' b -> (b, Grad nn' b))
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
    nn :: nn a
    nn = evalState mkNN mt
    nn' :: nn' b
    nn' = evalState mkNN' mt
    mt :: PureMT
    mt = pureMT 0
    dataset :: Vector (v a, v a)
    dataset = V.fromList $ map mkInput [1] -- [1..10]
    dataset' :: Vector (v' b, v' b)
    dataset' = V.fromList $ map mkInput' [1] -- [1..10]
    (x, grad)   = targetFuncGrad dataset nn
    (x', grad') = targetFuncGrad' dataset' nn'

    gradList  = fmap (fmap (fmap ApproxEq)) $ NN.toWeightList $ getGrad grad
    gradList' = fmap (fmap (fmap ApproxEq)) $ NN.toWeightList $ getGrad grad'

class ToDouble a where
  toDouble :: a -> Double

instance ToDouble Double where
  toDouble = id

instance ToDouble AlignedDouble where
  toDouble = getAlignedDouble

allTests :: TestTree
allTests = testGroup "all tests"
  [ tests
  , MatrixTests.tests
  , VectorTests.tests
  , OtherTests.tests
  ]

main :: IO ()
main = do
  defaultMain allTests

