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

{-# LANGUAGE BangPatterns         #-}
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
{-# LANGUAGE UndecidableInstances #-}

module NN.Generic where

import Prelude hiding (zipWith, zipWith3)
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.DeepSeq
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.Random.Source (MonadRandom)
import Data.Random.Source.PureMT ()
import Numeric.AD hiding (gradientDescent, Grad)

import Data.MatrixClass (Matrix)
import qualified Data.MatrixClass as MC
import Data.VectClass (Vect, (.+.))
import qualified Data.VectClass as VC
import Nonlinearity
import Unboxed.Functor
import Util
import Util.Zippable

-- w - matrix
-- v - vector
-- n - nonlinearity type
-- o - output type
-- a - element type
data NN w v n o a =
  NN {-# UNPACK #-} !(Vector (v a, w a)) -- hidden layers, each is a pair of bias vector and
                                         -- weights matrix
     {-# UNPACK #-} !(v a, w a)          -- final layer bias and weights
     deriving (Functor, Foldable, Traversable)

deriving instance (Show (v a), Show (w a)) => Show (NN w v n o a)
deriving instance (Eq (v a), Eq (w a))     => Eq (NN w v n o a)
deriving instance (Ord (v a), Ord (w a))   => Ord (NN w v n o a)

instance (NFData (v a), NFData (w a)) => NFData (NN w v n o a) where
  rnf (NN xs fin) = rnf xs `seq` rnf fin

instance (UnboxedFunctor v, UnboxedFunctor w) => UnboxedFunctor (NN w v n o) where
  ufmap f (NN layers (finBias, finWeights)) =
    NN (V.map (ufmap f *** ufmap f) layers) (ufmap f finBias, ufmap f finWeights)

instance (Matrix w v, Vect v) => Zippable (NN w v n o) where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith  = nnZipWith
  zipWith3 = nnZipWith3
  zipWith4 = nnZipWith4

nnZipWith :: (Matrix w v, Vect v) => (a -> b -> c) -> NN w v n o a -> NN w v n o b -> NN w v n o c
nnZipWith f (NN xs finX) (NN ys finY) =
  NN (zipWith zipLayers xs ys)
     (zipLayers finX finY)
  where
    zipLayers (xb, x) (yb, y) = (zipWith f xb yb, zipWith f x y)

nnZipWith3 :: (Matrix w v, Vect v) => (a -> b -> c -> d) -> NN w v n o a -> NN w v n o b -> NN w v n o c -> NN w v n o d
nnZipWith3 f (NN xs finX) (NN ys finY) (NN zs finZ) =
  NN (zipWith3 zipLayers xs ys zs)
     (zipLayers finX finY finZ)
  where
    zipLayers (xb, x) (yb, y) (zb, z) = (zipWith3 f xb yb zb, zipWith3 f x y z)

nnZipWith4 :: (Matrix w v, Vect v) => (a -> b -> c -> d -> e) -> NN w v n o a -> NN w v n o b -> NN w v n o c -> NN w v n o d -> NN w v n o e
nnZipWith4 f (NN xs finX) (NN ys finY) (NN zs finZ) (NN ws finW) =
  NN (zipWith4 zipLayers xs ys zs ws)
     (zipLayers finX finY finZ finW)
  where
    zipLayers (xb, x) (yb, y) (zb, z) (wb, w) = (zipWith4 f xb yb zb wb, zipWith4 f x y z w)

-- nnZ = nnX + NNy
add :: (Matrix w v, Vect v, Floating a) => NN w v n o a -> NN w v n o a -> NN w v n o a
add nn addend = nnZipWith (\x y -> x +! y) nn addend

-- nnZ = b * nnX + NNy
addScaled :: (Matrix w v, Vect v, Floating a) => NN w v n o a -> a -> NN w v n o a -> NN w v n o a
addScaled nn b addend = nnZipWith (\x y -> x +! b *! y) nn addend

nnSize :: forall w v n o a. (Matrix w v, Vect v, Floating a) => NN w v n o a -> a
nnSize (NN layers fin) =
  sqrt $ V.sum (V.map layerSize layers) + layerSize fin
  where
    layerSize :: (v a, w a) -> a
    layerSize (bias, weightMatrix) = VC.normL2Square bias + MC.normL2Square weightMatrix

differenceSize :: (Matrix w v, Vect v, Floating a) => NN w v n o a -> NN w v n o a -> a
differenceSize xs ys = nnSize $ addScaled xs (-1) ys

-- layer size should be specified without accounting for bias
makeNN
  :: forall m n o w v a. (Applicative m, MonadRandom m, Matrix w v, Vect v, Nonlinearity n, OutputType o n)
  => Int
  -> [Int]
  -> Int
  -> m a
  -> m (NN w v n o a)
makeNN inputLayerSize hiddenLayerSizes finalLayerSize mkElem = do
  (lastHiddenSize, hiddenLayersRev) <- foldM f (inputLayerSize, []) hiddenLayerSizes
  finalLayer                        <- mkLayer finalLayerSize lastHiddenSize
  return $ NN (V.fromList $ reverse hiddenLayersRev) finalLayer
  where
    mkLayer :: Int -> Int -> m (v a, w a)
    mkLayer size prevSize = do
      bias  <- VC.replicateM size mkElem
      layer <- MC.replicateM size prevSize mkElem -- VC.replicateM size (VC.replicateM prevSize mkElem)
      return (bias, layer)

    f :: (Int, [(v a, w a)]) -> Int -> m (Int, [(v a, w a)])
    f (prevSize, layers) size = do
      layer <- mkLayer size prevSize
      return (size, layer : layers)

forwardPropagate
  :: forall w v a n o. (Matrix w v, Vect v, Floating a, Nonlinearity n, OutputType o n)
  => NN w v n o a
  -> v a
  -> v a
forwardPropagate nn@(NN hiddenLayers finalLayer) input =
  f (output nn)
    (V.foldl' (f (nonlinearity nn)) input hiddenLayers)
    finalLayer
  where
    f :: (a -> a) -> v a -> (v a, w a) -> v a
    f activation prev (bias, layer) =
      VC.map activation $ bias .+. MC.vecMulRight layer prev

targetFunction
  :: (Matrix w v, Vect v, Floating a, Nonlinearity n, OutputType o n)
  => Vector (v a, v a)
  -> NN w v n o a
  -> a
targetFunction dataset nn =
  V.sum $
  V.map (\(x, y) -> VC.normL2Square $
                    zipWith (-!) (forwardPropagate nn x) y)
        dataset

targetFunctionGrad
  :: forall w v n o a. (Matrix w v, Traversable w, Vect v, Traversable v, Nonlinearity n, OutputType o n, Floating a)
  => Vector (v a, v a)
  -> NN w v n o a
  -> (a, Grad (NN w v n o) a)
targetFunctionGrad dataset =
  \nn -> second Grad $ grad' (targetFunction' dataset) nn
  where
    targetFunction'
      :: (Floating b, Mode b)
      => Vector (v (Scalar b), v (Scalar b))
      -> NN w v n o b
      -> b
    targetFunction' dataset =
      targetFunction (VC.map (VC.map auto *** VC.map auto) dataset)

backprop
  :: forall w v n o a. (Matrix w v, Vect v, Nonlinearity n, OutputType o n, Floating a, UnboxedFunctor w, UnboxedFunctor v, Unbox a)
  => Vector (v a, v a)
  -> NN w v n o a
  -> (a, Grad (NN w v n o) a)
backprop dataset = go
  where
    go :: NN w v n o a -> (a, Grad (NN w v n o) a)
    go nn@(NN _hiddenLayerWeights (_, finalLayerWeights)) =
      V.foldr'
        combineAdd
        (0, zeroGrad)
        (fmap (uncurry computeSample) dataset)
      where
        zeroGrad :: Grad (NN w v n o) a
        zeroGrad = Grad (ufmap (const 0) nn)

        computeSample :: v a -> v a -> (a, Grad (NN w v n o) a)
        computeSample x y
          | VC.length prediction /= VC.length y =
            error "Size mismatch between network prediction and expected output"
          | VC.length finalLayer /= MC.rows finalLayerWeights =
            error "Size mismatch between final layer sums and final layer neurons"
          | otherwise =
            (err, Grad $ NN hiddenLayerDerivs (finBiasDerivs, finDerivs))
          where
            -- NB full neurons of hidden layers can be obtained by using
            -- V.snoc hiddenLayersNeurons prefinalNeuronLayer
            hiddenLayersNeurons :: Vector (v (a, a), w a)
            (hiddenLayersNeurons, prefinalNeuronLayer, finalLayer) = forwardProp nn x
            prediction :: v a
            prediction = ufmap (\(x, _deds) -> x) finalLayer
            mismatch :: v a
            mismatch = zipWith (-!) prediction y
            err :: a
            err = VC.normL2Square mismatch
            finDeltas :: v a
            finDeltas = zipWith
                          (\m d -> 2 *! m *! d)
                          mismatch
                          (VC.map (\(_x, deds) -> deds) finalLayer)
            finBiasDerivs :: v a
            finDerivs     :: w a
            (finBiasDerivs, finDerivs) = mkLayerDeriv finDeltas prefinalNeuronLayer

            _prefinalLayerDelta :: v a
            prefinalDeltaWithLayer@(_prefinalLayerDelta, _) =
              mkDelta prefinalNeuronLayer (finDeltas, finalLayerWeights)

            -- Includes prefinalLayerDelta at the end. Does not need
            -- to include deltas for input layer since they won't be used
            hiddenLayerDeltas :: Vector (v a)
            hiddenLayerDeltas =
              V.map (\(deltas, _weights) -> deltas) $
              -- Same as V.tail $ V.scanr' ...
              V.prescanr' mkDelta prefinalDeltaWithLayer hiddenLayersNeurons
              -- Old version
              -- V.scanr' mkDelta prefinalDeltaWithLayer hiddenLayersNeurons

            -- Zipping deltas for all but first layer and neuron values for
            -- all but prefinal layer.
            hiddenLayerDerivs :: Vector (v a, w a)
            hiddenLayerDerivs =
              zipWith mkLayerDeriv hiddenLayerDeltas hiddenLayersNeurons

            mkLayerDeriv :: v a -> (v (a, a), w a) -> (v a, w a)
            mkLayerDeriv deltas (prevLayer, _) = (biasDerivs, derivs)
              where
                biasDerivs = deltas
                derivs = MC.outerProduct deltas $ VC.map (\(x, _deds) -> x) prevLayer

            mkDelta
              :: (v (a, a), w a)
              -> (v a, w a)
              -> (v a, w a)
            mkDelta (layer, weights) (deltas', weights') = (deltas, weights)
              where
                deltas :: v a
                deltas =
                  zipWith (\(_x, deds) weightedDeltas -> weightedDeltas *! deds) layer $
                  MC.vecMulLeft deltas' weights'

        combineAdd :: (a, Grad (NN w v n o) a) -> (a, Grad (NN w v n o) a) -> (a, Grad (NN w v n o) a)
        combineAdd (x, Grad g) (x', Grad g') = (x +! x', Grad $ add g g')

    forwardProp
      :: NN w v n o a
      -> v a
      -> (Vector (v (a, a), w a), (v (a, a), w a), v (a, a))
    forwardProp nn@(NN hiddenLayersWeights finalLayerWeights) input =
      (neuronValues', prefinalNeuronLayer, finalLayer)
      where
        neuronValues :: Vector (v (a, a), w a)
        neuronValues =
          V.scanl'
            f
            -- (fmap (\x -> (x, 1, error "no weights before input layer")) input)
            (ufmap (\x -> (x, 1)) input, error "no weights before input layer")
            hiddenLayersWeights

        neuronValues' = V.unsafeInit neuronValues
        prefinalNeuronLayer :: (v (a, a), w a)
        prefinalNeuronLayer = V.unsafeLast neuronValues
        -- (neuronValues', prefinalNeuronLayer) = V.splitAt (V.length neuronValues - 1) neuronValues

        finalLayer :: v (a, a)
        finalLayer = g prefinalNeuronLayer finalLayerWeights

        f :: (v (a, a), w a) -> (v a, w a) -> (v (a, a), w a)
        f (prevLayer, _) (bias, layer) =
          (VC.map (nonlinearityDeriv nn) $ bias .+. MC.vecMulRight layer (VC.map fst prevLayer), layer)

        g :: (v (a, a), w a) -> (v a, w a) -> v (a, a)
        g (prevLayer, _) (bias, layer) =
          VC.map (outputDeriv nn) $ bias .+. MC.vecMulRight layer (VC.map fst prevLayer)

        -- dot' :: v (a, b) -> v a -> a
        -- dot' xs ys
        --   | xsLen /= ysLen =
        --     error $ "dot': cannot take dot products for vectors of different length: " ++
        --       "|xs| = " ++ show xsLen ++ ", |ys| = " ++ show ysLen
        --   | otherwise      =
        --     VC.foldr (+!) 0 $ zipWith (\(x, _deds) y -> x *! y) xs ys
        --   where
        --     xsLen = VC.length xs
        --     ysLen = VC.length ys


targetFunctionGradNumerical
  :: forall w v n o a. (Matrix w v, Functor w, Traversable w, Vect v, Traversable v, Nonlinearity n, OutputType o n, Floating a)
  => a
  -> Vector (v a, v a)
  -> NN w v n o a
  -> (a, Grad (NN w v n o) a)
targetFunctionGradNumerical epsilon dataset nn =
  (targetFunction dataset nn, Grad grad)
  where
    nn' :: NN w v n o (Int, a, a, a)
    nn' = evalState (traverse enum nn) 0
    enum :: a -> State Int (Int, a, a, a)
    enum x = (, x, x - epsilon, x + epsilon) <$> get <* modify (+1)

    grad :: NN w v n o a
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


instance forall w v n o a. (Pretty (w a), Vect v, Show a, Nonlinearity n, OutputType o n) => Pretty (NN w v n o a) where
  pretty nn@(NN hiddenLayers finalLayer) =
    "Nonlinearity: " <> ppNonlinearity nn <> PP.line <>
    "Output: "       <> ppOutput nn       <> PP.line <>
    "HiddenLayers: " <> (PP.hcat $
                         PP.punctuate (PP.line <> PP.line) $
                         V.toList $
                         V.map showLayer hiddenLayers) <> PP.line <>
    "OutputLayer: "  <> showLayer finalLayer
    where
      showLayer :: (v a, w a) -> Doc
      showLayer (bias, weights) =
        "bias:   " <> showWeights bias PP.<$>
        "weighs: " <> PP.align (pretty weights)
      showWeights :: v a -> Doc
      showWeights = PP.hcat . PP.punctuate PP.comma . map prettyShow . VC.toList

