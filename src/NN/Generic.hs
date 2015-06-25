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
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}

module NN.Generic where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.DeepSeq
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Numeric.AD hiding (gradientDescent, Grad)

import Data.VectClass (Vect)
import qualified Data.VectClass as VC
import Nonlinearity
import Util

import Data.Random.Source (MonadRandom)
import Data.Random.Source.PureMT ()

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

-- nnZ = nnX + NNy
add :: (Vect v, Floating a) => NN v n o a -> NN v n o a -> NN v n o a
add nn addend = nnZipWith (\x y -> x + y) nn addend

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
makeNN :: forall m n o v a. (Applicative m, MonadRandom m, Vect v, Nonlinearity n, OutputType o n)
       => Int
       -> [Int]
       -> Int
       -> m a
       -> m (NN v n o a)
makeNN inputLayerSize hiddenLayerSizes finalLayerSize mkElem = do
  (lastHiddenSize, hiddenLayersRev) <- foldM f (inputLayerSize, V.empty) hiddenLayerSizes
  finalLayer <- mkLayer finalLayerSize lastHiddenSize
  return $ NN (V.reverse hiddenLayersRev) finalLayer
  where
    mkLayer :: Int -> Int -> m (v (v a))
    mkLayer size prevSize = VC.replicateM size (VC.replicateM prevSizeWithBias mkElem)
      where
        prevSizeWithBias = prevSize + 1

    f :: (Int, Vector (v (v a))) -> Int -> m (Int, Vector (v (v a)))
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
      VC.map (\ws -> activation $
                     VC.head ws + VC.dot prev (VC.tail ws))
             layer

targetFunction
  :: (Vect v, Floating a, Nonlinearity n, OutputType o n)
  => Vector (v a, v a)
  -> NN v n o a
  -> a
targetFunction dataset nn =
  V.sum $
  V.map (\(x, y) -> vectorSize' $
                    VC.zipWith (-) (forwardPropagate nn x) y)
        dataset

targetFunctionGrad
  :: forall v n o a. (Vect v, Nonlinearity n, OutputType o n, Floating a)
  => Vector (v a, v a)
  -> NN v n o a
  -> (a, Grad (NN v n o) a)
targetFunctionGrad dataset =
  \nn -> second Grad $ grad' (targetFunction' dataset) nn
  where
    targetFunction'
      :: (Floating b, Mode b)
      => Vector (v (Scalar b), v (Scalar b))
      -> NN v n o b
      -> b
    targetFunction' dataset =
      targetFunction (VC.map (VC.map auto *** VC.map auto) dataset)

backprop
  :: forall v n o a. (Vect v, Nonlinearity n, OutputType o n, Floating a)
  => Vector (v a, v a)
  -> NN v n o a
  -> (a, Grad (NN v n o) a)
backprop dataset = go
  where
    go :: NN v n o a -> (a, Grad (NN v n o) a)
    go nn@(NN _hiddenLayerWeights finalLayerWeights) =
      V.foldr'
        combineAdd
        (0, zeroGrad)
        (fmap (uncurry computeSample) dataset)
      where
        zeroGrad :: Grad (NN v n o) a
        zeroGrad = Grad (fmap (const 0) nn)

        computeSample :: v a -> v a -> (a, Grad (NN v n o) a)
        computeSample x y
          | VC.length prediction /= VC.length y =
            error "Size mismatch between network prediction and expected output"
          | VC.length finalLayer /= VC.length finalLayerWeights =
            error "Size mismatch between final layer sums and final layer neurons"
          | otherwise =
            (err, Grad $ NN hiddenLayerDerivs finalDerivs)
          where
            -- NB full neurons of hidden layers can be obtained by using
            -- V.snoc hiddenLayersNeurons prefinalNeuronLayer
            hiddenLayersNeurons :: Vector (v (a, a, v a))
            (hiddenLayersNeurons, prefinalNeuronLayer, finalLayer) = forwardProp nn x
            prediction :: v a
            prediction = fmap (\(x, _deds, _ws) -> x) finalLayer
            mismatch :: v a
            mismatch = VC.zipWith (-!) prediction y
            err :: a
            err = vectorSize' mismatch
            finalDeltas :: v a
            finalDeltas = VC.zipWith
                            (\m d -> 2 *! m *! d)
                            mismatch
                            (VC.map (\(_x, deds, _ws) -> deds) finalLayer)
            finalDerivs :: v (v a)
            finalDerivs = mkLayerDeriv finalDeltas prefinalNeuronLayer

            _prefinalLayerDelta :: v a
            prefinalDeltaWithLayer@(_prefinalLayerDelta, _) =
              mkDelta prefinalNeuronLayer (finalDeltas, finalLayer)

            -- Includes prefinalLayerDelta at the end. Does not need
            -- to include deltas for input layer since they won't be used
            hiddenLayerDeltas :: Vector (v a)
            hiddenLayerDeltas =
              V.map fst $
              -- Same as V.tail $ V.scanr' ...
              V.prescanr' mkDelta prefinalDeltaWithLayer hiddenLayersNeurons
              -- Old version
              -- V.scanr' mkDelta prefinalDeltaWithLayer hiddenLayersNeurons

            -- Zipping deltas for all but first layer and neuron values for
            -- all but prefinal layer.
            hiddenLayerDerivs :: Vector (v (v a))
            hiddenLayerDerivs =
              V.zipWith mkLayerDeriv hiddenLayerDeltas hiddenLayersNeurons

            mkLayerDeriv :: v a -> v (a, a, v a) -> v (v a)
            mkLayerDeriv deltas prevLayer =
              VC.map (\delta -> VC.cons delta $ VC.map (\(x, _deds, _ws) -> delta *! x) prevLayer) deltas

            mkDelta
              :: v (a, a, v a)
              -> (v a, v (a, a, v a))
              -> (v a, v (a, a, v a))
            mkDelta layer (deltas', layer') = (deltas, layer)
              where
                deltas :: v a
                deltas =
                  VC.zipWith (\(_x, deds, _ws) weightedDeltas -> weightedDeltas *! deds) layer $
                  VC.foldr1 (VC.zipWith (+!)) $
                  VC.zipWith (\(_, _, ws) delta ->
                               -- Ignore first weight since that's the weight for constant bias.
                               -- And there's no delta for bias.
                               VC.map (*! delta) $ VC.tail ws) layer' deltas'

        combineAdd :: (a, Grad (NN v n o) a) -> (a, Grad (NN v n o) a) -> (a, Grad (NN v n o) a)
        combineAdd (x, Grad g) (x', Grad g') = (x +! x', Grad $ add g g')

    forwardProp
      :: NN v n o a
      -> v a
      -> (Vector (v (a, a, v a)), v (a, a, v a), v (a, a, v a))
    forwardProp nn@(NN hiddenLayersWeights finalLayerWeights) input =
      (neuronValues', prefinalNeuronLayer, finalLayer)
      where
        neuronValues :: Vector (v (a, a, v a))
        neuronValues =
          V.scanl'
            f
            -- (fmap (\x -> (x, 1, error "no weights before input layer")) input)
            (fmap (\x -> (x, 1, VC.empty)) input)
            hiddenLayersWeights

        neuronValues' = V.unsafeInit neuronValues
        prefinalNeuronLayer :: v (a, a, v a)
        prefinalNeuronLayer = V.unsafeLast neuronValues
        -- (neuronValues', prefinalNeuronLayer) = V.splitAt (V.length neuronValues - 1) neuronValues

        finalLayer :: v (a, a, v a)
        finalLayer = g prefinalNeuronLayer finalLayerWeights

        f :: v (a, a, v a) -> v (v a) -> v (a, a, v a)
        f prevLayer layer = VC.map useLayer layer
          where
            useLayer :: v a -> (a, a, v a)
            useLayer ws = (x, deds, ws)
              where
                s             = VC.head ws +! dot' prevLayer (VC.tail ws)
                (# x, deds #) = nonlinearityDeriv nn s

        g :: v (a, a, v a) -> v (v a) -> v (a, a, v a)
        g prevLayer layer =
          VC.map (\ws -> let s             = VC.head ws +! dot' prevLayer (VC.tail ws)
                             (# x, deds #) = outputDeriv nn s
                         in (x, deds, ws))
                layer

        dot' :: v (a, b, c) -> v a -> a
        dot' xs ys
          | xsLen /= ysLen =
            error $ "dot': cannot take dot products for vectors of different length: " ++
              "|xs| = " ++ show xsLen ++ ", |ys| = " ++ show ysLen
          | otherwise      =
            VC.foldr (+!) 0 $ VC.zipWith (\(x, _deds, _ws) y -> x *! y) xs ys
          where
            xsLen = VC.length xs
            ysLen = VC.length ys



targetFunctionGradNumerical
  :: forall v n o a. (Vect v, Nonlinearity n, OutputType o n, Floating a)
  => a
  -> Vector (v a, v a)
  -> NN v n o a
  -> (a, Grad (NN v n o) a)
targetFunctionGradNumerical epsilon dataset nn =
  (targetFunction dataset nn, Grad grad)
  where
    nn' :: NN v n o (Int, a, a, a)
    nn' = evalState (traverse enum nn) 0
    enum :: a -> State Int (Int, a, a, a)
    enum x = (, x, x - epsilon, x + epsilon) <$> get <* modify (+1)

    grad :: NN v n o a
    grad = fmap calcGrad nn'

    calcGrad :: (Int, a, a, a) -> a
    calcGrad (n, _, xPrev, xNext) = (yNext - yPrev) / (2 * epsilon)
      where
        yPrev = targetFunction dataset (fmap (subst n xPrev) nn')
        yNext = targetFunction dataset (fmap (subst n xNext) nn')

        subst :: Int -> a -> (Int, a, b, b) -> a
        subst n x (m, y, _, _)
          | n == m    = x
          | otherwise = y


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
      showLayer =
        PP.vsep .
        map (PP.hcat . PP.punctuate PP.comma . map prettyShow . VC.toList) .
        VC.toList

vectorSize :: (Vect v, Floating a) => v a -> a
vectorSize = sqrt . vectorSize'

vectorSize' :: (Vect v, Floating a) => v a -> a
vectorSize' = VC.sum . VC.map (\x -> x * x) -- (^(2 :: Int))
