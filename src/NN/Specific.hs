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

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.DeepSeq
import Data.Foldable (Foldable)
import Data.List
import Data.Monoid
import Data.Traversable (Traversable)
import Data.Text.Lazy (Text)
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

data NN n o a = NN !(NonlinearityType n)
                   !(OutputType o)
                   (Vector (Vector (Vector a))) -- hidden layers
                   (Vector (Vector a))          -- final layer
              deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (NFData a) => NFData (NN n o a) where
  rnf (NN n o xs fin) = rnf n `seq` rnf o `seq` rnf xs `seq` rnf fin

nnZipWith :: (a -> b -> c) -> NN n o a -> NN n o b -> NN n o c
nnZipWith f (NN nonlinType outType xs finX) (NN _ _ ys finY) =
  NN nonlinType
     outType
     (V.zipWith zipLayers xs ys)
     (zipLayers finX finY)
  where
    zipLayers = V.zipWith (V.zipWith f)

nnZipWith3 :: (a -> b -> c -> d) -> NN n o a -> NN n o b -> NN n o c -> NN n o d
nnZipWith3 f (NN nonlinType outType xs finX) (NN _ _ ys finY) (NN _ _ zs finZ) =
  NN nonlinType
     outType
     (V.zipWith3 zipLayers xs ys zs)
     (zipLayers finX finY finZ)
  where
    zipLayers = V.zipWith3 (V.zipWith3 f)

nnZipWith4 :: (a -> b -> c -> d -> e) -> NN n o a -> NN n o b -> NN n o c -> NN n o d -> NN n o e
nnZipWith4 f (NN nonlinType outType xs finX) (NN _ _ ys finY) (NN _ _ zs finZ) (NN _ _ ws finW) =
  NN nonlinType
     outType
     (V.zipWith4 zipLayers xs ys zs ws)
     (zipLayers finX finY finZ finW)
  where
    zipLayers = V.zipWith4 (V.zipWith4 f)

-- nnZ = b * nnX + NNy
addScaled :: (Floating a) => NN n o a -> a -> NN n o a -> NN n o a
-- addScaled b (NN xs) (NN ys) = NN $ zipWith (\x y -> V.zipWith (\x' y' -> V.zipWith () x' y') x y) xs ys
addScaled nn b addend = nnZipWith (\x y -> x + b * y) nn addend

nnSize :: (Floating a) => NN n o a -> a
nnSize (NN _ _ layers fin) =
  sqrt $ V.sum (V.map layerSize layers) + layerSize fin
  where
    layerSize = V.sum . V.map (V.sum . V.map (^(2 :: Int)))

differenceSize :: (Floating a) => NN n o a -> NN n o a -> a
differenceSize xs ys = nnSize $ addScaled xs (-1) ys

-- layer size should be specified without accounting for bias
makeNN :: forall m n o. (MonadRandom m) =>
          NonlinearityType n ->
          OutputType o       ->
          Int                ->
          [Int]              ->
          Int                ->
          m (NN n o Double)
makeNN nonlinType outType firstLayerSize hiddenLayerSizes finalLayerSize = do
  (lastHiddenSize, hiddenLayersRev) <- foldM f (firstLayerSize, V.empty) hiddenLayerSizes
  finalLayer <- mkLayer finalLayerSize lastHiddenSize
  return $ NN nonlinType outType (V.reverse hiddenLayersRev) finalLayer
  where
    mkLayer :: Int -> Int -> m (Vector (Vector Double))
    mkLayer size prevSize = V.replicateM size (V.replicateM prevSizeWithBias (sample stdNormal))
      where
        prevSizeWithBias = prevSize + 1

    f :: (Int, Vector (Vector (Vector Double))) -> Int -> m (Int, Vector (Vector (Vector Double)))
    f (prevSize, layers) size = do
      layer <- mkLayer size prevSize
      return (size, V.cons layer layers)

nonlinearity :: (Floating a) => NonlinearityType n -> a -> a
nonlinearity HyperbolicTangent x = tanh x
nonlinearity Sigmoid           x = x' / (1 + x')
  where
    x' = exp x

output :: (Floating a) => NonlinearityType n -> OutputType o -> a -> a
output _      Linear    = id
output nonlin Nonlinear = nonlinearity nonlin

forwardPropagate :: (Floating a) => NN n o a -> Vector a -> Vector a
forwardPropagate (NN nonlinType outType hiddenLayers finalLayer) input =
  f (output nonlinType outType)
    (V.foldl' (f (nonlinearity nonlinType)) input hiddenLayers)
    finalLayer
  where
    f activation prev layer =
      V.map (\lr -> activation $
                    V.head lr + dot prev (V.tail lr))
            layer

targetFunction :: (Floating a, Mode a)
               => Vector (Vector (Scalar a), Vector (Scalar a))
               -> NN n o a
               -> a
targetFunction dataset nn =
  V.sum $
  V.map (\(x, y) -> vectorSize $
                    V.zipWith (-) (forwardPropagate nn x) y)
        (V.map (V.map auto *** V.map auto) dataset)

targetFunctionGrad :: Vector (Vector Double, Vector Double)
                   -> NN n o Double
                   -> (Double, Grad (NN n o) Double)
targetFunctionGrad dataset = \nn -> second Grad $ grad' (targetFunction dataset) nn

instance forall n o a. (Show a) => Pretty (NN n o a) where
  pretty (NN nonlinType outType hiddenLayers finalLayer) =
    "Nonlinearity: " <> prettyShow nonlinType <> PP.line <>
    "Output: "       <> prettyShow outType    <> PP.line <>
    "HiddenLayers: " <> prettyShow outType    <> PP.line <>
                        (PP.hcat $
                         PP.punctuate (PP.line <> PP.line) $
                         V.toList $
                         V.map showLayer hiddenLayers) <> PP.line <>
    "OutputLayer: "  <> showLayer finalLayer
    where
      showLayer :: Vector (Vector a) -> Doc
      showLayer = PP.vsep .
                  map (PP.hcat . PP.punctuate PP.comma . map prettyShow . V.toList) .
                  V.toList

ppNN :: forall a n o. (Show a) => NN n o a -> Text
ppNN nn = PP.displayT $ PP.renderPretty 0.8 80 $ pretty nn

dot :: (Floating a) => Vector a -> Vector a -> a
dot xs ys = if V.length xs /= V.length ys
            then error "cannot take dot products for vectors of different length"
            else V.foldr (+) 0 $ V.zipWith (*) xs ys

vectorSize :: (Floating a) => Vector a -> a
vectorSize = sqrt . V.sum . V.map (\x -> x * x) -- (^(2 :: Int))

