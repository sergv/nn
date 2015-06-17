----------------------------------------------------------------------------
-- |
-- Module      :  NN.Specific
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NN.Specific where

import Control.Arrow
import Control.Monad
import Control.DeepSeq
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Numeric.AD hiding (gradientDescent, Grad)

import Data.Random.Distribution.Normal (stdNormal)
import Data.Random.Sample (sample)
import Data.Random.Source (MonadRandom)
import Data.Random.Source.PureMT ()

import Util

data NN n o a = NN (Vector (Vector (Vector a))) -- hidden layers
                   (Vector (Vector a))          -- final layer
              deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (NFData a) => NFData (NN n o a) where
  rnf (NN xs fin) = rnf xs `seq` rnf fin

nnZipWith :: (a -> b -> c) -> NN n o a -> NN n o b -> NN n o c
nnZipWith f (NN xs finX) (NN ys finY) =
  NN (V.zipWith zipLayers xs ys)
     (zipLayers finX finY)
  where
    zipLayers = V.zipWith (V.zipWith f)

nnZipWith3 :: (a -> b -> c -> d) -> NN n o a -> NN n o b -> NN n o c -> NN n o d
nnZipWith3 f (NN xs finX) (NN ys finY) (NN zs finZ) =
  NN (V.zipWith3 zipLayers xs ys zs)
     (zipLayers finX finY finZ)
  where
    zipLayers = V.zipWith3 (V.zipWith3 f)

nnZipWith4 :: (a -> b -> c -> d -> e) -> NN n o a -> NN n o b -> NN n o c -> NN n o d -> NN n o e
nnZipWith4 f (NN xs finX) (NN ys finY) (NN zs finZ) (NN ws finW) =
  NN (V.zipWith4 zipLayers xs ys zs ws)
     (zipLayers finX finY finZ finW)
  where
    zipLayers = V.zipWith4 (V.zipWith4 f)

-- nnZ = b * nnX + NNy
addScaled :: (Floating a) => NN n o a -> a -> NN n o a -> NN n o a
-- addScaled b (NN xs) (NN ys) = NN $ zipWith (\x y -> V.zipWith (\x' y' -> V.zipWith () x' y') x y) xs ys
addScaled nn b addend = nnZipWith (\x y -> x + b * y) nn addend

nnSize :: (Floating a) => NN n o a -> a
nnSize (NN layers fin) =
  sqrt $ V.sum (V.map layerSize layers) + layerSize fin
  where
    layerSize = V.sum . V.map (V.sum . V.map (^(2 :: Int)))

differenceSize :: (Floating a) => NN n o a -> NN n o a -> a
differenceSize xs ys = nnSize $ addScaled xs (-1) ys

-- layer size should be specified without accounting for bias
makeNN :: forall m n o. (MonadRandom m) =>
          Int                ->
          [Int]              ->
          Int                ->
          m (NN n o Double)
makeNN firstLayerSize hiddenLayerSizes finalLayerSize = do
  (lastHiddenSize, hiddenLayersRev) <- foldM f (firstLayerSize, V.empty) hiddenLayerSizes
  finalLayer <- mkLayer finalLayerSize lastHiddenSize
  return $ NN (V.reverse hiddenLayersRev) finalLayer
  where
    mkLayer :: Int -> Int -> m (Vector (Vector Double))
    mkLayer size prevSize = V.replicateM size (V.replicateM prevSizeWithBias (sample stdNormal))
      where
        prevSizeWithBias = prevSize + 1

    f :: (Int, Vector (Vector (Vector Double))) -> Int -> m (Int, Vector (Vector (Vector Double)))
    f (prevSize, layers) size = do
      layer <- mkLayer size prevSize
      return (size, V.cons layer layers)

forwardPropagate :: (Floating a, Nonlinearity n, OutputType o n) => NN n o a -> Vector a -> Vector a
forwardPropagate nn@(NN hiddenLayers finalLayer) input =
  f (output nn)
    (V.foldl' (f (nonlinearity nn)) input hiddenLayers)
    finalLayer
  where
    f activation prev layer =
      V.map (\lr -> activation $
                    V.head lr + dot prev (V.tail lr))
            layer

targetFunction
  :: (Floating a, Mode a, Nonlinearity n, OutputType o n)
  => Vector (Vector (Scalar a), Vector (Scalar a))
  -> NN n o a
  -> a
targetFunction dataset nn =
  V.sum $
  V.map (\(x, y) -> vectorSize $
                    V.zipWith (-) (forwardPropagate nn x) y)
        (V.map (V.map auto *** V.map auto) dataset)

targetFunctionGrad
  :: (Nonlinearity n, OutputType o n)
  => Vector (Vector Double, Vector Double)
  -> NN n o Double
  -> (Double, Grad (NN n o) Double)
targetFunctionGrad dataset = \nn -> second Grad $ grad' (targetFunction dataset) nn

instance forall n o a. (Show a, Nonlinearity n, OutputType o n) => Pretty (NN n o a) where
  pretty nn@(NN hiddenLayers finalLayer) =
    "Nonlinearity: " <> ppNonlinearity nn  <> PP.line <>
    "Output: "       <> ppOutput nn        <> PP.line <>
    "HiddenLayers: " <> (PP.hcat $
                         PP.punctuate (PP.line <> PP.line) $
                         V.toList $
                         V.map showLayer hiddenLayers) <> PP.line <>
    "OutputLayer: "  <> showLayer finalLayer
    where
      showLayer :: Vector (Vector a) -> Doc
      showLayer = PP.vsep .
                  map (PP.hcat . PP.punctuate PP.comma . map prettyShow . V.toList) .
                  V.toList

dot :: (Floating a) => Vector a -> Vector a -> a
dot xs ys = if V.length xs /= V.length ys
            then error "cannot take dot products for vectors of different length"
            else V.foldr (+) 0 $ V.zipWith (*) xs ys

vectorSize :: (Floating a) => Vector a -> a
vectorSize = sqrt . V.sum . V.map (\x -> x * x) -- (^(2 :: Int))

