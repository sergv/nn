----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Sergey Vinokurov 2014
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

module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.State
-- import Data.Monoid
-- import Data.Text.Lazy (Text)
-- import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import System.Directory
import Text.Printf

import Criterion.Main
import Criterion.Types

import Data.Random.Distribution.Normal (stdNormal)
import Data.Random.Sample (sample)
import Data.Random.Source (MonadRandom)
import Data.Random.Source.PureMT (PureMT, pureMT)

import Data.Colour.Names
import Data.Colour
import Control.Lens
import Data.Default.Class
import Graphics.Rendering.Chart hiding (Vector)
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Aligned.Double
import Data.OpenBlasMatrix (OpenBlasMatrix)
import Data.PureMatrix (PureMatrix)
import Data.StorableVectorDouble (StorableVectorDouble(..))
import qualified Data.StorableVectorDouble as SVD
import Data.UnboxMatrix (UnboxMatrix)
import Data.UnboxMatrixWithTranspose (UnboxMatrixWithTranspose)
import qualified Data.VectClass as VC
import LearningAlgorithms
import NN.Specific
import qualified NN.Generic as NG
import Nonlinearity
import Util


-- f :: (Floating a) => Identity a -> a
-- f (Identity x) = x^(2 :: Int)
--
-- df :: (Floating a) => Identity a -> (a, Identity a)
-- -- df (Identity x) = grad (\x -> f $ fmap auto x) x
-- df x@(Identity _) = grad' f x
--
-- ddf :: forall a. (Floating a) => Identity a -> [a]
-- ddf x@(Identity _) = take 5 $ toList $ grads f x
--   where
--     toList :: Cofree Identity a -> [a]
--     toList (x :< rest) = x : toList (runIdentity rest)


-- targetFunction :: (Floating a) => [(Vector a, Vector a)] -> NN a -> a
-- targetFunction dataset nn =
--   sum $
--   map (\(x, y) -> vectorSize $ V.zipWith (-) (forwardPropagate nn x) y) dataset

-- Algorithms

criterionConfig :: Config
criterionConfig =
  defaultConfig { forceGC    = True
                , reportFile = Just "/tmp/nn-benchmark.html"
                , resamples  = 10
                }

nnHiddenLayersSize :: [Int]
nnHiddenLayersSize = [10, 10]

mkSpecificNN :: (Applicative m, MonadRandom m) => m (NN HyperbolicTangent Nonlinear Double)
mkSpecificNN = makeNN 1 nnHiddenLayersSize 1 (sample stdNormal)
-- for sine dataset
-- mkSpecificNN = makeNN hyperbolicTangentNT nonlinearOut 1 [2, 2] 1

mkGenericVectorNN :: (Applicative m, MonadRandom m) => m (NG.NN (PureMatrix Vector) Vector HyperbolicTangent Nonlinear Double)
mkGenericVectorNN = NG.makeNN 1 nnHiddenLayersSize 1 (sample stdNormal)

mkGenericListNN :: (Applicative m, MonadRandom m) => m (NG.NN (PureMatrix []) [] HyperbolicTangent Nonlinear Double)
mkGenericListNN = NG.makeNN 1 nnHiddenLayersSize 1 (sample stdNormal)

mkUnboxMatrixNN :: (Applicative m, MonadRandom m) => m (NG.NN UnboxMatrix U.Vector HyperbolicTangent Nonlinear Double)
mkUnboxMatrixNN = NG.makeNN 1 nnHiddenLayersSize 1 (sample stdNormal)
mkUnboxMatrixWithTransposeNN
  :: (Applicative m, MonadRandom m)
  => m (NG.NN UnboxMatrixWithTranspose U.Vector HyperbolicTangent Nonlinear Double)
mkUnboxMatrixWithTransposeNN = NG.makeNN 1 nnHiddenLayersSize 1 (sample stdNormal)
mkOpenBlasMatrixNN
  :: (Applicative m, MonadRandom m)
  => m (NG.NN OpenBlasMatrix StorableVectorDouble HyperbolicTangent Nonlinear AlignedDouble)
mkOpenBlasMatrixNN = NG.makeNN 1 nnHiddenLayersSize 1 (AlignedDouble <$> sample stdNormal)

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
                                 (NG.NN UnboxMatrix U.Vector HyperbolicTangent Nonlinear)
                                 (RPropState
                                   (NG.NN UnboxMatrix U.Vector HyperbolicTangent Nonlinear)
                                   Double)
                                 Double
      rpropDataGUnboxMatrix = rprop standardDeltaInfo nnGUnboxMatrix unboxMatrixDataset
  let nnGUnboxMatrixWithTranspose = evalState mkUnboxMatrixWithTransposeNN mt
      rpropDataGUnboxMatrixWithTranspose
        :: IterateData
             (NG.NN UnboxMatrixWithTranspose U.Vector HyperbolicTangent Nonlinear)
             (RPropState
               (NG.NN UnboxMatrixWithTranspose U.Vector HyperbolicTangent Nonlinear)
               Double)
             Double
      rpropDataGUnboxMatrixWithTranspose = rprop standardDeltaInfo nnGUnboxMatrixWithTranspose unboxMatrixDataset
  let openBlasMatrixDataset :: Vector (StorableVectorDouble AlignedDouble, StorableVectorDouble AlignedDouble)
      openBlasMatrixDataset = V.map (VC.fromList . V.toList . V.map AlignedDouble *** VC.fromList . V.toList . VC.map AlignedDouble) trainDataset
      nnGOpenBlasMatrix = evalState mkOpenBlasMatrixNN mt
      rpropDataGOpenBlasMatrix
        :: IterateData
             (NG.NN OpenBlasMatrix StorableVectorDouble HyperbolicTangent Nonlinear)
             (RPropState
               (NG.NN OpenBlasMatrix StorableVectorDouble HyperbolicTangent Nonlinear)
               AlignedDouble)
             AlignedDouble
      rpropDataGOpenBlasMatrix = rprop standardDeltaInfo nnGOpenBlasMatrix openBlasMatrixDataset

  defaultMainWith criterionConfig [
    --  bench "rprop specific" $
    --  nf (constantUpdates rpropData iterations) nn
    -- , bench "rprop generic - Vector" $
    --   nf (constantUpdates rpropDataGVec iterations) nnGVec
    -- , bench "rprop generic - List" $
    --   nf (constantUpdates rpropDataGList iterations) nnGList
    -- ,
      -- bench "rprop generic - UnboxMatrix" $
      -- nf (constantUpdates rpropDataGUnboxMatrix iterations) nnGUnboxMatrix
    -- ,
      bench "rprop generic - UnboxMatrixWithTranspose" $
      nf (constantUpdates rpropDataGUnboxMatrixWithTranspose iterations) nnGUnboxMatrixWithTranspose
    , bench "rprop generic - OpenBlasMatrix" $
      nf (constantUpdates rpropDataGOpenBlasMatrix iterations) nnGOpenBlasMatrix
    -- , bench "rprop unboxed tuple" $ nf (rprop' errInfo standardDeltaInfo nn) trainDataset
    ]
  -- void $ searchForFittingNN mt mkNN errInfo trainDataset

  -- print nn'
  where
    iterations = 10
    errInfo = ErrInfo 1e-5 1e-8

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

    searchForFittingNN
      :: (Nonlinearity n, OutputType o n)
      => PureMT
      -> State PureMT (NN n o Double)
      -> ErrInfo
      -> Vector (Vector Double, Vector Double)
      -> IO (NN n o Double)
    searchForFittingNN mt mkNN errInfo dataset = go mt 0
      where
        plottableDataset :: Vector (Double, Double)
        plottableDataset = V.map (head . V.toList *** head . V.toList) dataset
        go mt n = do
          printf "iteration %d\n" n
          plotResult n errorAmt nn' plottableDataset
          if errorAmt > 1
          then go mt' (n + 1)
          else do
            putStrLn "Start NN"
            T.putStrLn $ display nn
            putStrLn "Start NN on dataset"
            print $ V.map (head . V.toList . forwardPropagate nn . fst) dataset
            putStrLn "Result NN"
            T.putStrLn $ display nn'
            putStrLn "Result NN on dataset"
            print $ V.map (head . V.toList . forwardPropagate nn' . fst) dataset
            return nn'
          where
            (nn, mt')       = runState mkNN mt
            rpropData       = rprop standardDeltaInfo nn dataset
            (errorAmt, nn') = iteratedUpdates rpropData errInfo nn

plotResult
  :: (Nonlinearity n, OutputType o n)
  => Int -> Double -> NN n o Double -> Vector (Double, Double) -> IO ()
plotResult n err nn dataset = do
  createDirectoryIfMissing True plotPath
  void $ renderableToFile def (printf "%s/model%05d.png" plotPath n) chart
  where
    plotPath = "/tmp/nn3"
    chart    = toRenderable layout
      where
        target = plot_lines_values .~ [V.toList dataset]
               $ plot_lines_style  . line_color .~ opaque red
               $ plot_lines_title .~ "original"
               $ def

        predictedDataset = V.map (id *** V.head . forwardPropagate nn . V.singleton) dataset
        predicted = plot_lines_values .~ [V.toList predictedDataset]
                  $ plot_lines_style  . line_color .~ opaque blue
                  $ plot_lines_title .~ "predicted model"
                  $ def

        layout = layout_title .~ (printf "Model #%d, error = %g" n err)
               $ layout_plots .~ [toPlot target, toPlot predicted]
               $ def
