----------------------------------------------------------------------------
-- |
-- Module      :  NNTests
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Saturday, 21 May 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module NNTests (tests) where

import Control.Arrow
import Control.Monad.Except
import Control.Monad.State
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Monoid
import qualified Data.Text.Lazy as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.Aligned ()
import Data.Aligned.Double
import Data.AlignedStorableVector (AlignedStorableVector)
import Data.ConstrainedFunctor
import Data.Grad
import Data.Nonlinearity
import Data.OpenBlasMatrix (OpenBlasMatrix)
import Data.PureMatrix (PureMatrix)
import Data.UnboxMatrix (UnboxMatrix)
import Data.UnboxMatrixWithTranspose (UnboxMatrixWithTranspose)
import Data.VectClass (Vect)
import qualified Data.VectClass as VC
import NN (NNVectorLike, NeuralNetwork)
import qualified NN
import qualified NN.Generic as G
import qualified NN.Specific as S
import Util

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck hiding (sample)

import Data.Random.Distribution.Normal (stdNormal)
import Data.Random.Sample (sample)
import Data.Random.Source.PureMT (PureMT, pureMT)

import NN.Description
import ApproxEq

chunkSize :: Int
chunkSize = 2

epsilon :: Double
epsilon = 1e-6

tests :: TestTree
tests = testGroup "Neural network tests"
  [ nnTests
  , localOption (QC.QuickCheckMaxSize 40) $
    QC.testProperty "Generic OpenBlasMatrix match Specific values and gradients, random NNs and inputs"
      nn_prop
  ]

nnTests :: TestTree
nnTests = testGroup "NN.Specific tests"
  [ testGroup "compare gradients from ad package and from numerical calculation by central differences"
      [ compareAdVsNumericalGradients
          "1 to 1 network, no hidden layers"
          genInput_1_to_1
          (mkDescription' 1 [] 1)
      , compareAdVsNumericalGradients
          "1 to 1 network, 1 hidden layer with 1 node each"
          genInput_1_to_1
          (mkDescription' 1 [1] 1)
      , compareAdVsNumericalGradients
          "1 to 1 network, 1 hidden layer with 2 nodes each"
          genInput_1_to_1
          (mkDescription' 1 [2] 1)
      , compareAdVsNumericalGradients
          "1 to 1 network, 2 hidden layers with 1 node each"
          genInput_1_to_1
          (mkDescription' 1 [1, 1] 1)
      , compareAdVsNumericalGradients
          "2 to 2 network, 2 hidden layers with 4 nodes each"
          genInput_2_to_2
          (mkDescription' 2 [4, 4] 2)
      ]

  , testGroup "compare gradients from different sources"
      [ compareGradientsFromDifferentSources
          "1 to 1 network, no hidden layers"
          genInput_1_to_1
          (mkDescription' 1 [] 1)
      , compareGradientsFromDifferentSources
          "1 to 1 network, 1 hidden layer with 1 node each"
          genInput_1_to_1
          (mkDescription' 1 [1] 1)
      , compareGradientsFromDifferentSources
          "1 to 1 network, 1 hidden layer with 2 nodes each"
          genInput_1_to_1
          (mkDescription' 1 [2]
          1)
      , compareGradientsFromDifferentSources
          "1 to 1 network, 2 hidden layers with 1 node each"
          genInput_1_to_1
          (mkDescription' 1 [1, 1] 1)
      , compareGradientsFromDifferentSources
          "2 to 2 network, 2 hidden layers with 4 nodes each"
          genInput_2_to_2
          (mkDescription' 2 [4, 4] 2)
      , compareGradientsFromDifferentSources
          "5 to 5 network, no hidden layers"
          genInput_5_to_5
          (mkDescription' 5 [] 5)
      , compareGradientsFromDifferentSources
          "5 to 5 network, 2 hidden layers with 10 nodes each"
          genInput_5_to_5
          (mkDescription' 5 [10, 10] 5)
      , compareGradientsFromDifferentSources
          "5 to 5 network, 10 hidden layers with 10 nodes each"
          genInput_5_to_5
          (mkDescription' 5 (replicate 10 10) 5)
      ]
  ]
  where
    genInput_1_to_1 k = (k :| [], k :| [])
    genInput_2_to_2 k = (k :| [2 * k], 10 * k :| [k^2])
    genInput_5_to_5 k =
      (k :| [k ** 0.5, k ** 0.3, k ** 0.25, k ** 0.2], k :| [ k^n | n <- [2..5]])

compareAdVsNumericalGradients
  :: String
  -> (Double -> (NonEmpty Double, NonEmpty Double))
  -> Description n o Double
  -> TestTree
compareAdVsNumericalGradients name genInput descr =
  testGroup name
    [ testGroup "ad vs numerical gradient"
        [ compareGradients
            "Specific"
            genInput
            (makeSpecificNN descr)
            S.targetFunctionGradAD
            (S.targetFunctionGradNumerical epsilon)
        , testGroup "Generic"
            [ compareGradients
                "Vector"
                genInput
                (makeGenericVectorNN descr)
                G.targetFunctionGradAD
                (G.targetFunctionGradNumerical epsilon)
            , compareGradients
                "List"
                genInput
                (makeGenericListNN descr)
                G.targetFunctionGradAD
                (G.targetFunctionGradNumerical epsilon)
            ]
        ]
    ]

compareGradientsFromDifferentSources
  :: String
  -> (Double -> (NonEmpty Double, NonEmpty Double))
  -> Description n o Double
  -> TestTree
compareGradientsFromDifferentSources name genInput descr =
  testGroup name
  [ testGroup "ad vs backpropagation, same NN type"
      [ compareGradients
          "Specific"
          genInput
          (makeSpecificNN descr)
          S.targetFunctionGradAD
          S.backprop
      , testGroup "Generic"
          [ compareGradients
              "PureMatrix: Vector, chunkSize = 1"
              genInput
              (makeGenericVectorNN descr)
              -- (G.targetFunctionGradNumerical epsilon)
              G.targetFunctionGradAD
              (G.backprop 1)
          , compareGradients
              ("PureMatrix: Vector, chunkSize = " ++ show chunkSize)
              genInput
              (makeGenericVectorNN descr)
              -- (G.targetFunctionGradNumerical epsilon)
              G.targetFunctionGradAD
              (G.backprop chunkSize)
          , compareGradients
              "PureMatrix: List"
              genInput
              (makeGenericListNN descr)
              -- (G.targetFunctionGradNumerical epsilon)
              G.targetFunctionGradAD
              (G.backprop chunkSize)
          ]
      ]
  , testGroup "backpropagation, different nn types"
      [ compareNNGradients
          "Specific vs Generic Vector"
          genInput
          (makeSpecificNN descr)
          S.backprop
          (makeGenericVectorNN descr)
          (G.backprop chunkSize)
      , compareNNGradients
          "Specific vs Generic UnboxMatrix"
          genInput
          (makeSpecificNN descr)
          S.backprop
          (makeUnboxMatrixNN descr)
          (G.backprop chunkSize)
      , compareNNGradients
          "Specific vs Generic UnboxMatrixWithTranspose"
          genInput
          (makeSpecificNN descr)
          S.backprop
          (makeUnboxMatrixWithTransposeNN descr)
          (G.backprop chunkSize)
      , compareNNGradients
          "Specific vs Generic OpenBlasMatrix"
          genInput
          (makeSpecificNN descr)
          S.backprop
          (makeOpenBlasMatrixNN descr)
          (G.backprop chunkSize)
      ]
  ]

makeSpecificNN :: Description n o Double -> S.NN HyperbolicTangent HyperbolicTangent Double
makeSpecificNN = either error id . fromDescription . relaxDescription
makeGenericVectorNN :: Description n o Double -> G.NN (PureMatrix Vector) Vector HyperbolicTangent HyperbolicTangent Double
makeGenericVectorNN = either error id . fromDescription . relaxDescription
makeGenericListNN :: Description n o Double -> G.NN (PureMatrix []) [] HyperbolicTangent HyperbolicTangent Double
makeGenericListNN = either error id . fromDescription . relaxDescription
makeUnboxMatrixNN :: Description n o Double -> G.NN UnboxMatrix U.Vector HyperbolicTangent HyperbolicTangent Double
makeUnboxMatrixNN = either error id . fromDescription . relaxDescription
makeUnboxMatrixWithTransposeNN :: Description n o Double -> G.NN UnboxMatrixWithTranspose U.Vector HyperbolicTangent HyperbolicTangent Double
makeUnboxMatrixWithTransposeNN =
  either error id . fromDescription . relaxDescription
makeOpenBlasMatrixNN :: Description n o Double -> G.NN OpenBlasMatrix AlignedStorableVector HyperbolicTangent HyperbolicTangent AlignedDouble
makeOpenBlasMatrixNN =
  either error id . fromDescription . relaxDescription . fmap AlignedDouble

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

compareGradients
  :: forall nn n o v. (NNVectorLike (nn n o) Double, Pretty (nn n o Double))
  => (ElemConstraints (nn n o) Double)
  => (NNDescription nn, Vect v, ElemConstraints v Double)
  => String
  -> (Double -> (NonEmpty Double, NonEmpty Double))
  -> nn n o Double
  -> (Vector (v Double, v Double) -> nn n o Double -> (Double, Grad (nn n o) Double))
  -> (Vector (v Double, v Double) -> nn n o Double -> (Double, Grad (nn n o) Double))
  -> TestTree
compareGradients name genInput mkNN targetFuncGrad targetFuncGrad' =
  compareNNGradients name genInput mkNN targetFuncGrad mkNN targetFuncGrad'

-- | Compare gradients for two networks
compareNNGradients
  :: forall nn nn' n o v v' a b.
     (NNVectorLike (nn n o) a, Pretty (nn n o a), ElemConstraints (nn n o) a)
  => (NNVectorLike (nn' n o) b, Pretty (nn' n o b), ElemConstraints (nn' n o) b)
  => (NNDescription nn, NNDescription nn')
  => (Vect v, ElemConstraints v a)
  => (Vect v', ElemConstraints v' b)
  => (Show a, Pretty a, ToDouble a, FromDouble a)
  => (Show b, Pretty b, ToDouble b, FromDouble b)
  => String                                                      -- ^ Test name
  -> (Double -> (NonEmpty Double, NonEmpty Double))              -- ^ Input generator
  -> nn n o a                                                    -- ^ First network
  -> (Vector (v a, v a) -> nn n o a -> (a, Grad (nn n o) a))     -- ^ Function to compute target function along with gradient for first NN
  -> nn' n o b                                                   -- ^ Second network
  -> (Vector (v' b, v' b) -> nn' n o b -> (b, Grad (nn' n o) b)) -- ^ Target function value and gradient calculator for second NN
  -> TestTree
compareNNGradients name genInput nn targetFuncGrad nn' targetFuncGrad' =
  testCase name
    $ either (assertFailure . display') return
    $ compareGrads nn targetFuncGrad nn' targetFuncGrad' dataset
  where
    dataset = V.fromList $ map genInput [1..10]

compareGrads
  :: forall nn nn' n o v v' a b m.
     (NNVectorLike (nn n o) a, Pretty (nn n o a), ElemConstraints (nn n o) a)
  => (NNVectorLike (nn' n o) b, Pretty (nn' n o b), ElemConstraints (nn' n o) b)
  => (NNDescription nn, NNDescription nn')
  => (Vect v, ElemConstraints v a)
  => (Vect v', ElemConstraints v' b)
  => (Show a, Pretty a, ToDouble a, FromDouble a)
  => (Show b, Pretty b, ToDouble b, FromDouble b)
  => (MonadError Doc m)
  => nn n o a                                                    -- ^ First network
  -> (Vector (v a, v a) -> nn n o a -> (a, Grad (nn n o) a))     -- ^ Function to compute target function along with gradient for first NN
  -> nn' n o b                                                   -- ^ Second network
  -> (Vector (v' b, v' b) -> nn' n o b -> (b, Grad (nn' n o) b)) -- ^ Function to compute target function along with gradient for second NN
  -> Vector (NonEmpty Double, NonEmpty Double)                   -- ^ Dataset
  -> m ()
compareGrads nn targetFuncGrad nn' targetFuncGrad' dataset = do
  unless (ApproxEq y == ApproxEq y') $
    throwError $ PP.vcat
      [ "target function values do not match:"
      , "first result:  " <> pretty y
      , "second result: " <> pretty y'
      ]
  unless (gradDescr == gradDescr') $
    throwError $ PP.vcat
      [ "target function gradients do not match:"
      , "expected:    " <> PP.text (T.pack $ show gradDescr)
      , "expected nn: " <> pretty nn
      , PP.empty
      , "got:         " <> PP.text (T.pack $ show gradDescr')
      , "got nn:      " <> pretty nn'
      ]
  where
    firstDataset :: Vector (v a, v a)
    firstDataset = both (VC.fromList . map fromDouble . toList) <$> dataset
    secondDataset :: Vector (v' b, v' b)
    secondDataset = both (VC.fromList . map fromDouble . toList) <$> dataset
    (y, grad)   = targetFuncGrad firstDataset nn
    (y', grad') = targetFuncGrad' secondDataset nn'

    gradDescr :: Description n o ApproxEq
    gradDescr  = fmap ApproxEq $ either error id $ NN.toDescription $ getGrad grad
    gradDescr' = fmap ApproxEq $ either error id $ NN.toDescription $ getGrad grad'

compareForwardProp
  :: forall nn nn' n o v v' a b m.
     (NeuralNetwork (nn n o) v a, NeuralNetwork (nn' n o) v' b)
  => (NNVectorLike (nn n o) a, NNVectorLike (nn' n o) b)
  => (ElemConstraints (nn n o) a, ElemConstraints (nn' n o) b)
  => (Vect v, Vect v')
  => (ElemConstraints v a, ElemConstraints v' b)
  => (Show a, Pretty a, ToDouble a, FromDouble a)
  => (Show b, Pretty b, ToDouble b, FromDouble b)
  => (MonadError Doc m)
  => nn n o a                 -- ^ First network
  -> nn' n o b                -- ^ Second network
  -> Vector (NonEmpty Double) -- ^ Dataset - only inputs
  -> m ()
compareForwardProp nn nn' inputs = V.zipWithM_ cmp ys ys'
  where
    firstDataset :: Vector (v a)
    firstDataset = VC.fromList . map fromDouble . toList <$> inputs
    secondDataset :: Vector (v' b)
    secondDataset = VC.fromList . map fromDouble . toList <$> inputs
    ys :: Vector (v a)
    ys  = NN.forwardPropagate nn <$> firstDataset
    ys' :: Vector (v' b)
    ys' = NN.forwardPropagate nn' <$> secondDataset
    cmp :: v a -> v' b -> m ()
    cmp xs ys = do
      unless (VC.length xs == VC.length ys) $
        throwError $ PP.vcat
          [ "output dimensions mismatch:"
          , "first result:  " <> pretty xs'
          , "second result: " <> pretty ys'
          ]
      zipWithM_ (\(n, x) y ->
          unless (ApproxEq x == ApproxEq y) $
            throwError $ PP.vcat
              [ pretty n <> "th component of the output do not match:"
              , "first component:  " <> pretty x
              , "second component: " <> pretty y
              , "first result:     " <> pretty xs'
              , "second result:    " <> pretty ys'
              ])
        (zip [0 :: Int ..] xs')
        ys'
      where
        xs' = VC.toList xs
        ys' = VC.toList ys

mkDescription' :: Int -> [Int] -> Int -> Description n o Double
mkDescription' inputSize hiddenLayerSizes outputSize =
  evalState
    (mkDescription inputSize hiddenLayerSizes outputSize (sample stdNormal))
    mt
  where
    mt :: PureMT
    mt = pureMT 0

data NNTest = NNTest
  { nnTestDescr :: Description HyperbolicTangent HyperbolicTangent Double
  , nnDataset   :: NonEmpty (NonEmpty Double, NonEmpty Double)
  , nnChunkSize :: Int
  } deriving (Show, Eq, Ord)

instance Arbitrary NNTest where
  arbitrary = do
    descr@Description {descriptionInputSize, descriptionOutputSize} <- arbitrary
    dataset <- neListOf $ (,)
                 <$> neVectorOf descriptionInputSize arbitrary
                 <*> neVectorOf descriptionOutputSize arbitrary
    chunkSize <- choose (1, 1000)
    return NNTest
      { nnTestDescr = descr
      , nnDataset   = dataset
      , nnChunkSize = chunkSize
      }
  shrink test@NNTest {nnTestDescr, nnDataset, nnChunkSize}
    | any (\(x, _) -> NE.length x /= descriptionInputSize nnTestDescr) nnDataset =
      error $ "Dataset input does not match description's input size "
        ++ show (descriptionInputSize nnTestDescr)
        ++ "\ndataset = " ++ show nnDataset
    | any (\(_, y) -> NE.length y /= descriptionOutputSize nnTestDescr) nnDataset =
      error $ "Dataset output does not match description's otput size "
        ++ show (descriptionOutputSize nnTestDescr)
        ++ "\ndataset = " ++ show nnDataset
    | otherwise =
      [ test { nnDataset = dataset }
      | dataset <- drop1 nnDataset
      ] `interleave`
      [ test
          { nnTestDescr = testDescr
          , nnDataset   = shrinkDataset shrinkType nnDataset
          }
      | (shrinkType, testDescr) <- shrinkDescription nnTestDescr
      ] `interleave`
      [ test { nnChunkSize = size }
      | Positive size <- shrink $ Positive nnChunkSize
      ]

shrinkDataset
  :: ShrinkType
  -> NonEmpty (NonEmpty Double, NonEmpty Double)
  -> NonEmpty (NonEmpty Double, NonEmpty Double)
shrinkDataset (ShrinkInput n)      = fmap (first (dropFromNE n))
shrinkDataset (ShrinkOutput n)     = fmap (second (dropFromNE n))
shrinkDataset ShrinkLayer          = id
shrinkDataset ShrinkNumberOfLayers = id

dropFromNE :: Int -> NonEmpty a -> NonEmpty a
dropFromNE n = go n
  where
    go 0 (_ :| xs) =
      case xs of
        []     ->
          error $ "dropFromNE: non-empty list would become empty after drop, when tried to drop Nth element: " ++ show n
        y : ys -> y :| ys
    go n (x :| xs) = x :| dropFromList (n - 1) xs

dropFromList :: forall a. Int -> [a] -> [a]
dropFromList n = go n mempty
  where
    go :: Int -> DList a -> [a] -> [a]
    go _ _      []     = error "dropFromList: can drop from empty list"
    go 0 prefix (_:xs) = DL.apply prefix xs
    go n prefix (x:xs) = go (n - 1) (DL.snoc prefix x) xs

neListOf :: Gen a -> Gen (NonEmpty a)
neListOf gen = (:|) <$> gen <*> listOf gen

neVectorOf :: Int -> Gen a -> Gen (NonEmpty a)
neVectorOf n gen
  | n > 0     = (:|) <$> gen <*> vectorOf (n - 1) gen
  | otherwise = fail $ "neVectorOf: cannot generate < 1 elements: " ++ show n

nn_prop :: NNTest -> Property
nn_prop NNTest {nnTestDescr, nnDataset, nnChunkSize} =
  case result of
    Left err -> counterexample (display' err) (property False)
    Right _  -> property True
  where
    result = compareGrads
               nn
               NN.targetFunctionGrad
               nn'
               (G.backprop nnChunkSize)
               (V.fromList $ toList nnDataset)
    nn :: S.NN HyperbolicTangent HyperbolicTangent Double
    nn = makeSpecificNN nnTestDescr
    nn' :: G.NN OpenBlasMatrix AlignedStorableVector HyperbolicTangent HyperbolicTangent AlignedDouble
    nn' = makeOpenBlasMatrixNN nnTestDescr
    -- nn' :: G.NN (PureMatrix Vector) Vector HyperbolicTangent HyperbolicTangent Double
    -- nn' = makeGenericVectorNN nnTestDescr

_nnForwardPropagationCheck :: (MonadError Doc m) => NNTest -> m ()
_nnForwardPropagationCheck NNTest {nnTestDescr, nnDataset} =
  compareForwardProp
    nn
    nn'
    (V.fromList $ toList $ fst <$> nnDataset)
  where
    nn :: S.NN HyperbolicTangent HyperbolicTangent Double
    nn = makeSpecificNN nnTestDescr
    nn' :: G.NN OpenBlasMatrix AlignedStorableVector HyperbolicTangent HyperbolicTangent AlignedDouble
    nn' = makeOpenBlasMatrixNN nnTestDescr
