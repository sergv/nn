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
{-# LANGUAGE TypeFamilies        #-}

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
import System.Directory
import Text.PrettyPrint.Leijen.Text (Pretty)
import Text.Printf

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
import Data.StorableVectorDouble (StorableVectorDouble(..))
import Data.V3 (V3)
import Data.ConstrainedConvert (Convert)
import Data.ConstrainedFunctor
import Data.VectClass (Vect)
import qualified Data.VectClass as VC
import Data.Zippable
import LearningAlgorithms
import NN
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

nnHiddenLayersSize :: [Int]
nnHiddenLayersSize = [10, 10]

mkOpenBlasMatrixNN
  :: (Applicative m, MonadRandom m)
  => m (NG.NN OpenBlasMatrix StorableVectorDouble HyperbolicTangent Nonlinear AlignedDouble)
mkOpenBlasMatrixNN = NG.makeNN 1 nnHiddenLayersSize 1 (AlignedDouble <$> sample stdNormal)

main :: IO ()
main = do
  nn' <- searchForFittingNN mt mkOpenBlasMatrixNN errInfo openBlasMatrixDataset
  print nn'
  where
    errInfo = ErrInfo 1e-5 1e-8

    openBlasMatrixDataset :: Vector (StorableVectorDouble AlignedDouble, StorableVectorDouble AlignedDouble)
    openBlasMatrixDataset =
      V.map (mkStorableVectorDouble *** mkStorableVectorDouble) trainDataset

    mkStorableVectorDouble = VC.fromList . V.toList . V.map AlignedDouble

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
      :: forall k nn v a k' nn'. (NNVectorLike k nn a, NeuralNetwork k nn v a, Pretty (nn a))
      => (ElemConstraints k a, Vect k v, Show a, Num a, Floating a, PlotValue a)
      => (Convert k k' nn nn', ConstrainedFunctor k' nn', Zippable k' nn')
      => (ElemConstraints k' a, ElemConstraints k' (V3 a))
      => PureMT
      -> State PureMT (nn a)
      -> ErrInfo a
      -> Vector (v a, v a)
      -> IO (nn a)
    searchForFittingNN mt mkNN errInfo dataset = go mt 0
      where
        plottableDataset :: Vector (a, a)
        plottableDataset = V.map (head . VC.toList *** head . VC.toList) dataset
        go mt n = do
          printf "iteration %d\n" n
          plotResult n errorAmt nn' plottableDataset
          if errorAmt > 1
          then go mt' (n + 1)
          else do
            putStrLn "Start NN"
            T.putStrLn $ display nn
            putStrLn "Start NN on dataset"
            print $ V.map (head . VC.toList . forwardPropagate nn . fst) dataset
            putStrLn "Result NN"
            T.putStrLn $ display nn'
            putStrLn "Result NN on dataset"
            print $ V.map (head . VC.toList . forwardPropagate nn' . fst) dataset
            return nn'
          where
            (nn, mt')       = runState mkNN mt
            rpropData       = rprop standardDeltaInfo nn dataset
            (errorAmt, nn') = iteratedUpdates rpropData errInfo nn

plotResult
  :: (NeuralNetwork k nn v a, ElemConstraints k a, Vect k v, Ord a, Show a, PlotValue a)
  => Int -> a -> nn a -> Vector (a, a) -> IO ()
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

        predictedDataset = V.map (id *** head . VC.toList . forwardPropagate nn . VC.singleton) dataset
        predicted = plot_lines_values .~ [V.toList predictedDataset]
                  $ plot_lines_style  . line_color .~ opaque blue
                  $ plot_lines_title .~ "predicted model"
                  $ def

        layout = layout_title .~ (printf "Model #%d, error = %s" n (show err))
               $ layout_plots .~ [toPlot target, toPlot predicted]
               $ def
