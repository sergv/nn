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
import System.Directory
import Text.Printf

import Data.Random.Source.PureMT (PureMT, pureMT)

import Data.Colour.Names
import Data.Colour
import Control.Lens
import Data.Default.Class
import Graphics.Rendering.Chart hiding (Vector)
import Graphics.Rendering.Chart.Backend.Cairo

import LearningAlgorithms
import NN
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

main :: IO ()
main = do
  void $ searchForFittingNN mt mkNN errInfo sinDataset
  -- print nn'
  where
    mkNN = makeNN hyperbolicTangentNT nonlinearOut 1 [5, 5, 5] 1
    errInfo = ErrInfo 1e-5 1e-8
    -- xorDataset :: [(Vector Double, Vector Double)]
    -- xorDataset = map (V.fromList *** V.fromList)
    --                   [ ([f, f], [f])
    --                   , ([f, t], [t])
    --                   , ([t, f], [t])
    --                   , ([t, t], [f])
    --                   ]
    --   where
    --     t = 1
    --     f = (-1)

    sinDataset :: [(Vector Double, Vector Double)]
    sinDataset = map (V.fromList *** V.fromList)
                     [ ([x], [sin x])
                     | x <- linspace 1000 0 (2 * pi)
                     ]

    mt :: PureMT
    mt = pureMT 0

    searchForFittingNN :: PureMT                           ->
                          State PureMT (NN n o Double)     ->
                          ErrInfo                          ->
                          [(Vector Double, Vector Double)] ->
                          IO (NN n o Double)
    searchForFittingNN mt mkNN errInfo dataset = go mt 0
      where
        plottableDataset :: [(Double, Double)]
        plottableDataset = map (head . V.toList *** head . V.toList) dataset
        go mt n = do
          printf "iteration %d\n" n
          plotResult n errorAmt nn' plottableDataset
          if errorAmt > 1
          then go mt' (n + 1)
          else do
            putStrLn "Start NN"
            T.putStrLn $ ppNN nn
            putStrLn "Start NN on dataset"
            print $ map (head . V.toList . forwardPropagate nn . fst) dataset
            putStrLn "Result NN"
            T.putStrLn $ ppNN nn'
            putStrLn "Result NN on dataset"
            print $ map (head . V.toList . forwardPropagate nn' . fst) dataset
            return nn'
          where
            (nn, mt')       = runState mkNN mt
            (errorAmt, nn') = rprop errInfo standardDeltaInfo nn dataset

plotResult :: Int -> Double -> NN n o Double -> [(Double, Double)] -> IO ()
plotResult n err nn dataset = do
  createDirectoryIfMissing True plotPath
  void $ renderableToFile def (printf "%s/model%05d.png" plotPath n) chart
  where
    plotPath = "/tmp/nn3"
    chart    = toRenderable layout
      where
        target = plot_lines_values .~ [dataset]
               $ plot_lines_style  . line_color .~ opaque red
               $ plot_lines_title .~ "original"
               $ def

        predictedDataset = map (id *** V.head . forwardPropagate nn . V.singleton) dataset
        predicted = plot_lines_values .~ [predictedDataset]
                  $ plot_lines_style  . line_color .~ opaque blue
                  $ plot_lines_title .~ "predicted model"
                  $ def

        layout = layout_title .~ (printf "Model #%d, error = %g" n err)
               $ layout_plots .~ [toPlot target, toPlot predicted]
               $ def


-- Local Variables:
-- haskell-program-name: ("bash" "-c" "cd /home/sergey/projects/haskell/projects/numerical/nn/ && cabal repl exe:nn")
-- End:

