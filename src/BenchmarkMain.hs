----------------------------------------------------------------------------
-- |
-- Module      :  BenchmarkMain
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
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BenchmarkMain where

import Control.Arrow
import Control.Monad.State
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Criterion.Main
import Criterion.Types

import Data.Random.Distribution.Normal (stdNormal)
import Data.Random.Sample (sample)
import Data.Random.Source (MonadRandom)
import Data.Random.Source.PureMT (PureMT, pureMT)

import Data.Aligned.Double
import Data.Aligned.Float
import Data.AlignedStorableVector (AlignedStorableVector(..))
import Data.Nonlinearity
import Data.OpenBlasMatrix (OpenBlasMatrix)
import Data.PureMatrix (PureMatrix)
import Data.UnboxMatrix (UnboxMatrix)
import Data.UnboxMatrixWithTranspose (UnboxMatrixWithTranspose)
import qualified Data.VectClass as VC
import LearningAlgorithms
import NN.Specific
import qualified NN.Generic as NG
import Util

criterionConfig :: Config
criterionConfig =
  defaultConfig { forceGC    = True
                , reportFile = Just "/tmp/nn-benchmark.html"
                , resamples  = 10
                }

nnHiddenLayersSize :: [Int]
nnHiddenLayersSize = [10, 10]

mkSpecificNN :: (Applicative m, MonadRandom m) => m (NN HyperbolicTangent HyperbolicTangent Double)
mkSpecificNN = makeNN 1 nnHiddenLayersSize 1 (sample stdNormal)
-- for sine dataset
-- mkSpecificNN = makeNN hyperbolicTangentNT nonlinearOut 1 [2, 2] 1

mkGenericVectorNN :: (Applicative m, MonadRandom m) => m (NG.NN (PureMatrix Vector) Vector HyperbolicTangent HyperbolicTangent Double)
mkGenericVectorNN = NG.makeNN 1 nnHiddenLayersSize 1 (sample stdNormal)

mkGenericListNN :: (Applicative m, MonadRandom m) => m (NG.NN (PureMatrix []) [] HyperbolicTangent HyperbolicTangent Double)
mkGenericListNN = NG.makeNN 1 nnHiddenLayersSize 1 (sample stdNormal)

mkUnboxMatrixNN :: (Applicative m, MonadRandom m) => m (NG.NN UnboxMatrix U.Vector HyperbolicTangent HyperbolicTangent Double)
mkUnboxMatrixNN = NG.makeNN 1 nnHiddenLayersSize 1 (sample stdNormal)
mkUnboxMatrixWithTransposeNN
  :: (Applicative m, MonadRandom m)
  => m (NG.NN UnboxMatrixWithTranspose U.Vector HyperbolicTangent HyperbolicTangent Double)
mkUnboxMatrixWithTransposeNN = NG.makeNN 1 nnHiddenLayersSize 1 (sample stdNormal)
mkOpenBlasMatrixDoubleNN
  :: (Applicative m, MonadRandom m)
  => m (NG.NN OpenBlasMatrix AlignedStorableVector HyperbolicTangent HyperbolicTangent AlignedDouble)
mkOpenBlasMatrixDoubleNN = NG.makeNN 1 nnHiddenLayersSize 1 (AlignedDouble <$> sample stdNormal)
mkOpenBlasMatrixFloatNN
  :: (Applicative m, MonadRandom m)
  => m (NG.NN OpenBlasMatrix AlignedStorableVector HyperbolicTangent HyperbolicTangent AlignedFloat)
mkOpenBlasMatrixFloatNN = NG.makeNN 1 nnHiddenLayersSize 1 (AlignedFloat <$> sample stdNormal)

main :: IO ()
main = do
  let nn        = evalState mkSpecificNN mt
      rpropData = rprop standardDeltaInfo nn trainDataset
  let nnGVec        = evalState mkGenericVectorNN mt
      rpropDataGVec = rprop standardDeltaInfo nnGVec trainDataset
  let nnGList        = evalState mkGenericListNN mt
      rpropDataGList = rprop standardDeltaInfo nnGList $
                       V.map (V.toList *** V.toList) trainDataset
  let nnGUnboxMatrix        = evalState mkUnboxMatrixNN mt
      unboxMatrixDataset :: Vector (U.Vector Double, U.Vector Double)
      unboxMatrixDataset    = V.map (VC.fromList . V.toList *** VC.fromList . V.toList) trainDataset
      rpropDataGUnboxMatrix :: IterateData
                                 (NG.NN UnboxMatrix U.Vector HyperbolicTangent HyperbolicTangent)
                                 (RPropState
                                   (NG.NN UnboxMatrix U.Vector HyperbolicTangent HyperbolicTangent)
                                   Double)
                                 Double
      rpropDataGUnboxMatrix = rprop standardDeltaInfo nnGUnboxMatrix unboxMatrixDataset
  let nnGUnboxMatrixWithTranspose = evalState mkUnboxMatrixWithTransposeNN mt
      rpropDataGUnboxMatrixWithTranspose
        :: IterateData
             (NG.NN UnboxMatrixWithTranspose U.Vector HyperbolicTangent HyperbolicTangent)
             (RPropState
               (NG.NN UnboxMatrixWithTranspose U.Vector HyperbolicTangent HyperbolicTangent)
               Double)
             Double
      rpropDataGUnboxMatrixWithTranspose = rprop standardDeltaInfo nnGUnboxMatrixWithTranspose unboxMatrixDataset
  let openBlasMatrixDoubleDataset :: Vector (AlignedStorableVector AlignedDouble, AlignedStorableVector AlignedDouble)
      openBlasMatrixDoubleDataset = V.map (VC.fromList . V.toList . V.map AlignedDouble *** VC.fromList . V.toList . VC.map AlignedDouble) trainDataset
      nnGOpenBlasDoubleMatrix = evalState mkOpenBlasMatrixDoubleNN mt
      rpropDataGOpenBlasDoubleMatrix
        :: IterateData
             (NG.NN OpenBlasMatrix AlignedStorableVector HyperbolicTangent HyperbolicTangent)
             (RPropState
               (NG.NN OpenBlasMatrix AlignedStorableVector HyperbolicTangent HyperbolicTangent)
               AlignedDouble)
             AlignedDouble
      rpropDataGOpenBlasDoubleMatrix = rprop standardDeltaInfo nnGOpenBlasDoubleMatrix openBlasMatrixDoubleDataset
  let openBlasMatrixFloatDataset :: Vector (AlignedStorableVector AlignedFloat, AlignedStorableVector AlignedFloat)
      openBlasMatrixFloatDataset = V.map (VC.fromList . V.toList . V.map (AlignedFloat . realToFrac) *** VC.fromList . V.toList . VC.map (AlignedFloat . realToFrac)) trainDataset
      nnGOpenBlasFloatMatrix = evalState mkOpenBlasMatrixFloatNN mt
      rpropDataGOpenBlasFloatMatrix
        :: IterateData
             (NG.NN OpenBlasMatrix AlignedStorableVector HyperbolicTangent HyperbolicTangent)
             (RPropState
               (NG.NN OpenBlasMatrix AlignedStorableVector HyperbolicTangent HyperbolicTangent)
               AlignedFloat)
             AlignedFloat
      rpropDataGOpenBlasFloatMatrix = rprop standardDeltaInfo nnGOpenBlasFloatMatrix openBlasMatrixFloatDataset

  defaultMainWith criterionConfig [
     -- bench "rprop specific" $
     -- nf (constantUpdates rpropData iterations) nn
    -- , bench "rprop generic - Vector" $
    --   nf (constantUpdates rpropDataGVec iterations) nnGVec
    -- , bench "rprop generic - List" $
    --   nf (constantUpdates rpropDataGList iterations) nnGList
    -- , bench "rprop generic - UnboxMatrix" $
    --   nf (constantUpdates rpropDataGUnboxMatrix iterations) nnGUnboxMatrix
    -- ,
      -- bench "rprop generic - UnboxMatrixWithTranspose" $
      -- nf (constantUpdates rpropDataGUnboxMatrixWithTranspose iterations) nnGUnboxMatrixWithTranspose
    -- ,
      bench "rprop generic - OpenBlasMatrix, Double" $
      nf (constantUpdates rpropDataGOpenBlasDoubleMatrix iterations) nnGOpenBlasDoubleMatrix
    , bench "rprop generic - OpenBlasMatrix, Float" $
      nf (constantUpdates rpropDataGOpenBlasFloatMatrix iterations) nnGOpenBlasFloatMatrix
    -- , bench "rprop unboxed tuple" $ nf (rprop' errInfo standardDeltaInfo nn) trainDataset
    ]
  where
    iterations = 10

    -- trainDataset = xorDataset
    -- xorDataset :: Vector (Vector Double, Vector Double)
    -- xorDataset = V.fromList $
    --              map (V.fromList *** V.fromList)
    --                   [ ([f, f], [f])
    --                   , ([f, t], [t])
    --                   , ([t, f], [t])
    --                   , ([t, t], [f])
    --                   ]
    --   where
    --     t = 1
    --     f = (-1)

    trainDataset = sinDataset
    sinDataset :: Vector (Vector Double, Vector Double)
    sinDataset = V.fromList $
                 map (V.fromList *** V.fromList)
                     [ ([x], [sin x])
                     | x <- linspace 1000 0 (2 * pi)
                     ]

    mt :: PureMT
    mt = pureMT 0
