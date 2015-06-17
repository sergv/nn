----------------------------------------------------------------------------
-- |
-- Module      :  NN.Generic
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module NN.Generic where

import Control.Arrow
import Control.Monad
import Control.DeepSeq
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Numeric.AD hiding (gradientDescent, Grad)

import Data.VectClass (Vect)
import qualified Data.VectClass as VC

import Data.Random.Distribution.Normal (stdNormal)
import Data.Random.Sample (sample)
import Data.Random.Source (MonadRandom)
import Data.Random.Source.PureMT ()

import Util

data NN v n o a = NN (Vector (v (v a))) -- hidden layers
                     (v (v a))          -- final layer
                deriving (Functor, Foldable, Traversable)

deriving instance (Show (v (v a))) => Show (NN v n o a)
deriving instance (Eq (v (v a)))   => Eq (NN v n o a)
deriving instance (Ord (v (v a)))  => Ord (NN v n o a)

instance (NFData (v (v a))) => NFData (NN v n o a) where
  rnf (NN xs fin) = rnf xs `seq` rnf fin

nnZipWith :: (Vect v) => (a -> b -> c) -> NN v n o a -> NN v n o b -> NN v n o c
nnZipWith f (NN xs finX) (NN ys finY) =
  NN (V.zipWith zipLayers xs ys)
     (zipLayers finX finY)
  where
    zipLayers = VC.zipWith (VC.zipWith f)

nnZipWith3 :: (Vect v) => (a -> b -> c -> d) -> NN v n o a -> NN v n o b -> NN v n o c -> NN v n o d
nnZipWith3 f (NN xs finX) (NN ys finY) (NN zs finZ) =
  NN (V.zipWith3 zipLayers xs ys zs)
     (zipLayers finX finY finZ)
  where
    zipLayers = VC.zipWith3 (VC.zipWith3 f)

nnZipWith4 :: (Vect v) => (a -> b -> c -> d -> e) -> NN v n o a -> NN v n o b -> NN v n o c -> NN v n o d -> NN v n o e
nnZipWith4 f (NN xs finX) (NN ys finY) (NN zs finZ) (NN ws finW) =
  NN (V.zipWith4 zipLayers xs ys zs ws)
     (zipLayers finX finY finZ finW)
  where
    zipLayers = VC.zipWith4 (VC.zipWith4 f)

-- nnZ = b * nnX + NNy
addScaled :: (Vect v, Floating a) => NN v n o a -> a -> NN v n o a -> NN v n o a
-- addScaled b (NN xs) (NN ys) = NN $ zipWith (\x y -> V.zipWith (\x' y' -> V.zipWith () x' y') x y) xs ys
addScaled nn b addend = nnZipWith (\x y -> x + b * y) nn addend

nnSize :: (Vect v, Floating a) => NN v n o a -> a
nnSize (NN layers fin) =
  sqrt $ V.sum (V.map layerSize layers) + layerSize fin
  where
    layerSize = VC.sum . VC.map (VC.sum . VC.map (^(2 :: Int)))

differenceSize :: (Vect v, Floating a) => NN v n o a -> NN v n o a -> a
differenceSize xs ys = nnSize $ addScaled xs (-1) ys

-- layer size should be specified without accounting for bias
makeNN :: forall m n o v. (Applicative m, MonadRandom m, Vect v, Nonlinearity n, OutputType o n)
       => Int
       -> [Int]
       -> Int
       -> m (NN v n o Double)
makeNN firstLayerSize hiddenLayerSizes finalLayerSize = do
  (lastHiddenSize, hiddenLayersRev) <- foldM f (firstLayerSize, V.empty) hiddenLayerSizes
  finalLayer <- mkLayer finalLayerSize lastHiddenSize
  return $ NN (V.reverse hiddenLayersRev) finalLayer
  where
    mkLayer :: Int -> Int -> m (v (v Double))
    mkLayer size prevSize = VC.replicateM size (VC.replicateM prevSizeWithBias (sample stdNormal))
      where
        prevSizeWithBias = prevSize + 1

    f :: (Int, Vector (v (v Double))) -> Int -> m (Int, Vector (v (v Double)))
    f (prevSize, layers) size = do
      layer <- mkLayer size prevSize
      return (size, V.cons layer layers)

forwardPropagate
  :: forall v a n o. (Vect v, Floating a, Nonlinearity n, OutputType o n)
  => NN v n o a
  -> v a
  -> v a
forwardPropagate nn@(NN hiddenLayers finalLayer) input =
  f (output nn)
    (V.foldl' (f (nonlinearity nn)) input hiddenLayers)
    finalLayer
  where
    f :: (a -> a) -> v a -> v (v a) -> v a
    f activation prev layer =
      VC.map (\lr -> activation $
                     VC.head lr + VC.dot prev (VC.tail lr))
             layer

targetFunction
  :: (Vect v, Floating a, Mode a, Nonlinearity n, OutputType o n)
  => Vector (v (Scalar a), v (Scalar a))
  -> NN v n o a
  -> a
targetFunction dataset nn =
  V.sum $
  V.map (\(x, y) -> vectorSize $
                    VC.zipWith (-) (forwardPropagate nn x) y)
        (V.map (VC.map auto *** VC.map auto) dataset)

targetFunctionGrad
  :: (Vect v, Nonlinearity n, OutputType o n)
  => Vector (v Double, v Double)
  -> NN v n o Double
  -> (Double, Grad (NN v n o) Double)
targetFunctionGrad dataset = \nn -> second Grad $ grad' (targetFunction dataset) nn

instance forall v n o a. (Vect v, Show a, Nonlinearity n, OutputType o n) => Pretty (NN v n o a) where
  pretty nn@(NN hiddenLayers finalLayer) =
    "Nonlinearity: " <> ppNonlinearity nn <> PP.line <>
    "Output: "       <> ppOutput nn       <> PP.line <>
    "HiddenLayers: " <> (PP.hcat $
                         PP.punctuate (PP.line <> PP.line) $
                         V.toList $
                         V.map showLayer hiddenLayers) <> PP.line <>
    "OutputLayer: "  <> showLayer finalLayer
    where
      showLayer :: v (v a) -> Doc
      showLayer = PP.vsep .
                  map (PP.hcat . PP.punctuate PP.comma . map prettyShow . VC.toList) .
                  VC.toList

vectorSize :: (Vect v, Floating a) => v a -> a
vectorSize = sqrt . VC.sum . VC.map (\x -> x * x) -- (^(2 :: Int))
