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

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module NN.Generic where

import Prelude hiding (zipWith, zipWith3)
import Control.Arrow
import Control.Monad.Except
import Control.Monad.State
import Control.DeepSeq
import qualified Data.List as L
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.Random.Source.PureMT ()
import Numeric.AD hiding (grad, Grad)

import Data.UnboxMatrix (UnboxMatrix)
import Data.UnboxMatrixWithTranspose (UnboxMatrixWithTranspose)
import Data.ConstrainedConvert (Convert)
import qualified Data.ConstrainedConvert as Conv
import Data.ConstrainedFunctor
import Data.MatrixClass (Matrix)
import qualified Data.MatrixClass as MC
import Data.VectClass (Vect, (.+.))
import qualified Data.VectClass as VC
import Data.Zippable
import Nonlinearity
import Util

-- import Debug.Trace

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

instance (ConstrainedFunctor k v, ConstrainedFunctor k w) => ConstrainedFunctor k (NN w v n o) where
  {-# INLINABLE cfmap #-}
  cfmap f (NN layers (finBias, finWeights)) =
    NN (V.map (cfmap f *** cfmap f) layers) (cfmap f finBias, cfmap f finWeights)

instance (Matrix k w v, Zippable k w, Vect k v) => Zippable k (NN w v n o) where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith  = nnZipWith
  zipWith3 = nnZipWith3
  zipWith4 = nnZipWith4

instance (Convert k k' w w', Convert k k' v v') => Convert k k' (NN w v n o) (NN w' v' n o) where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   (NN hiddenLayers finalLayer) =
    NN (V.map conv hiddenLayers) (conv finalLayer)
    where
      conv = Conv.convertTo *** Conv.convertTo
  convertFrom (NN hiddenLayers finalLayer) = id
    NN (V.map conv hiddenLayers) (conv finalLayer)
    where
      conv = Conv.convertFrom *** Conv.convertFrom

toWeightList
  :: forall k w v n o a. (Matrix k w v, Vect k v, ElemConstraints k a)
  => NN w v n o a
  -> [[[a]]]
toWeightList (NN hiddenLayers finalLayer) =
  map convertLayer $ V.toList hiddenLayers ++ [finalLayer]
  where
    convertLayer :: (v a, w a) -> [[a]]
    convertLayer (bias, weights) = L.zipWith (:) (VC.toList bias) (MC.toList weights)

nnZipWith
  :: (Matrix k w v, Zippable k w, Vect k v)
  => (ElemConstraints k a)
  => (ElemConstraints k b)
  => (ElemConstraints k c)
  => (a -> b -> c)
  -> NN w v n o a
  -> NN w v n o b
  -> NN w v n o c
nnZipWith f (NN xs finX) (NN ys finY) =
  NN (V.zipWith zipLayers xs ys)
     (zipLayers finX finY)
  where
    zipLayers (xb, x) (yb, y) = (zipWith f xb yb, zipWith f x y)

nnZipWith3
  :: (Matrix k w v, Zippable k w, Vect k v)
  => (ElemConstraints k a)
  => (ElemConstraints k b)
  => (ElemConstraints k c)
  => (ElemConstraints k d)
  => (a -> b -> c -> d)
  -> NN w v n o a
  -> NN w v n o b
  -> NN w v n o c
  -> NN w v n o d
nnZipWith3 f (NN xs finX) (NN ys finY) (NN zs finZ) =
  NN (V.zipWith3 zipLayers xs ys zs)
     (zipLayers finX finY finZ)
  where
    zipLayers (xb, x) (yb, y) (zb, z) = (zipWith3 f xb yb zb, zipWith3 f x y z)

nnZipWith4
  :: (Matrix k w v, Zippable k w, Vect k v)
  => (ElemConstraints k a)
  => (ElemConstraints k b)
  => (ElemConstraints k c)
  => (ElemConstraints k d)
  => (ElemConstraints k e)
  => (a -> b -> c -> d -> e)
  -> NN w v n o a
  -> NN w v n o b
  -> NN w v n o c
  -> NN w v n o d
  -> NN w v n o e
nnZipWith4 f (NN xs finX) (NN ys finY) (NN zs finZ) (NN ws finW) =
  NN (V.zipWith4 zipLayers xs ys zs ws)
     (zipLayers finX finY finZ finW)
  where
    zipLayers (xb, x) (yb, y) (zb, z) (wb, w) = (zipWith4 f xb yb zb wb, zipWith4 f x y z w)

-- nnZ = nnX + NNy
add :: (Matrix k w v, Zippable k w, Vect k v, Num a, ElemConstraints k a)
    => NN w v n o a -> NN w v n o a -> NN w v n o a
add nn addend = nnZipWith (\x y -> x +! y) nn addend

-- nnZ = b * nnX + NNy
addScaled
  :: (Matrix k w v, Zippable k w, Vect k v, Num a, ElemConstraints k a)
  => NN w v n o a -> a -> NN w v n o a -> NN w v n o a
addScaled nn b addend = nnZipWith (\x y -> x +! b *! y) nn addend

nnSize
  :: forall k w v n o a. (Matrix k w v, ConstrainedFunctor k w, Vect k v, Floating a, ElemConstraints k a)
  => NN w v n o a -> a
nnSize (NN layers fin) =
  sqrt $ V.sum (V.map layerSize layers) + layerSize fin
  where
    layerSize :: (v a, w a) -> a
    layerSize (bias, weightMatrix) = VC.normL2Square bias + MC.normL2Square weightMatrix

differenceSize
  :: (Matrix k w v, ConstrainedFunctor k w, Zippable k w)
  => (Vect k v, Floating a, ElemConstraints k a)
  => NN w v n o a -> NN w v n o a -> a
differenceSize xs ys = nnSize $ addScaled xs (-1) ys

fromWeightList
  :: forall m k w v n o a. (MonadError String m, ElemConstraints k a, Show a)
  => (Matrix k w v, Vect k v)
  => [[[a]]]
  -> m (NN w v n o a)
fromWeightList []  = throwError "Cannot create neural network from empty list of weights"
fromWeightList wss = NN <$> (V.fromList <$> mapM convertLayer wss')
                        <*> convertLayer finalWs
  where
    wss'    = init wss
    finalWs = last wss
    convertLayer :: [[a]] -> m (v a, w a)
    convertLayer [] = throwError "Cannot convert empty layer to neural network"
    convertLayer ws@(w:_)
      | wLen > 0 && all (\w -> length w == wLen) ws =
        return $ (VC.fromList $ map head ws, MC.fromList $ map tail ws)
      | otherwise =
        throwError $ "Invalid layer, all rows must be of the same lenght: " ++ show ws
      where
        wLen = length w

makeNN
  :: forall k m n o w v a. (Monad m, Show a)
  => (Matrix k w v, Vect k v, ElemConstraints k a)
  => (Nonlinearity n, OutputType o n)
  => Int
  -> [Int]
  -> Int
  -> m a
  -> m (NN w v n o a)
makeNN inputLayerSize hiddenLayerSizes finalLayerSize mkElem =
  either error return . fromWeightList =<<
  makeWeightList inputLayerSize hiddenLayerSizes finalLayerSize mkElem

forwardPropagate
  :: forall k w v a n o. (Matrix k w v, Vect k v, Floating a, Nonlinearity n, OutputType o n, ConstrainedFunctor k v, ElemConstraints k a)
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
      cfmap activation $ bias .+. MC.vecMulRight layer prev

targetFunction
  :: (Matrix k w v, Vect k v, ConstrainedFunctor k v, Floating a, ElemConstraints k a)
  => (Nonlinearity n, OutputType o n)
  => Vector (v a, v a)
  -> NN w v n o a
  -> a
targetFunction dataset nn =
  V.sum $
  V.map (\(x, y) -> VC.normL2Square $
                    zipWith (-!) (forwardPropagate nn x) y)
        dataset

targetFunctionGrad
  :: forall w v n o a. (Matrix NoConstraints w v, Traversable w)
  => (Vect NoConstraints v, ConstrainedFunctor NoConstraints v, Traversable v)
  => (Nonlinearity n, OutputType o n)
  => (Floating a)
  => Vector (v a, v a)
  -> NN w v n o a
  -> (a, Grad (NN w v n o) a)
targetFunctionGrad dataset =
  \nn -> second Grad $ grad' (targetFunction' dataset) nn
  where
    targetFunction'
      :: (Floating b, Mode b, ElemConstraints NoConstraints b)
      => Vector (v (Scalar b), v (Scalar b))
      -> NN w v n o b
      -> b
    targetFunction' dataset =
      targetFunction (V.map (fmap auto *** fmap auto) dataset)

-- backprop'
--   :: (Nonlinearity n, OutputType o n)
--   => Vector (VectorDouble Double, VectorDouble Double)
--   -> NN MatrixDouble VectorDouble n o Double
--   -> (Double, Grad (NN MatrixDouble VectorDouble n o) Double)
-- backprop' = backprop

-- {-# SPECIALIZE
--   backprop
--     :: (OutputType o n)
--     => Vector (VectorDouble Double, VectorDouble Double)
--     -> NN MatrixDouble VectorDouble n o Double
--     -> (Double, Grad (NN MatrixDouble VectorDouble n o) Double)
--   #-}

{-# INLINABLE backprop #-}
backprop
  :: forall k w v n o a. (Matrix k w v, ConstrainedFunctor k w, Zippable k w)
  => (Vect k v, ConstrainedFunctor k v)
  => (Floating a, ElemConstraints k a)
  => (Nonlinearity n, OutputType o n)
  => (Show a)
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
        zeroGrad = Grad (cfmap (const 0) nn)

        computeSample :: v a -> v a -> (a, Grad (NN w v n o) a)
        computeSample x y
          | VC.length prediction /= VC.length y =
            error "Size mismatch between network prediction and expected output"
          | VC.length finalLayer /= MC.rows finalLayerWeights =
            error "Size mismatch between final layer sums and final layer neurons"
          | otherwise =
            -- trace (display' $ PP.vcat
            --         [ "Generic"
            --         , "hiddenLayersNeurons  = " <> pretty hiddenLayersNeurons
            --         , "hiddenLayerWeights   = " <> pretty _hiddenLayerWeights
            --         , "prefinalNeuronLayer  = " <> pretty prefinalNeuronLayer
            --         , "finalLayer           = " <> pretty finalLayer
            --         , "finalLayerDeriv      = " <> pretty finalLayerDeriv
            --         , "finalLayerWeights    = " <> PP.align (pretty finalLayerWeights)
            --         ]) $
            (err, Grad $ NN hiddenLayersDerivs (finBiasDerivs, finDerivs))
          where
            -- NB full neurons of hidden layers can be obtained by using
            -- V.snoc hiddenLayersNeurons prefinalNeuronLayer
            hiddenLayersNeurons :: Vector (v a, v a, w a)
            (hiddenLayersNeurons, prefinalNeuronLayer, finalLayer, finalLayerDeriv) = forwardProp nn x
            prediction :: v a
            prediction = finalLayer
            mismatch :: v a
            mismatch = zipWith (-!) prediction y
            err :: a
            err = VC.normL2Square mismatch
            finDeltas :: v a
            finDeltas = zipWith
                          (\m d -> 2 *! m *! d)
                          mismatch
                          finalLayerDeriv
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
            hiddenLayersDerivs :: Vector (v a, w a)
            hiddenLayersDerivs =
              zipWith mkLayerDeriv hiddenLayerDeltas hiddenLayersNeurons

            mkLayerDeriv :: v a -> (v a, v a, w a) -> (v a, w a)
            mkLayerDeriv deltas (prevLayer, _prevLayerDeriv, _) = (biasDerivs, derivs)
              where
                biasDerivs = deltas
                derivs = MC.outerProduct deltas prevLayer

            mkDelta
              :: (v a, v a, w a)
              -> (v a, w a)
              -> (v a, w a)
            mkDelta (_layer, layerDeriv, weights) (deltas', weights') = (deltas, weights)
              where
                deltas :: v a
                deltas =
                  zipWith (\deds weightedDeltas -> deds *! weightedDeltas) layerDeriv $
                  MC.vecMulRight (MC.transpose weights') deltas'

        combineAdd :: (a, Grad (NN w v n o) a) -> (a, Grad (NN w v n o) a) -> (a, Grad (NN w v n o) a)
        combineAdd (x, Grad g) (x', Grad g') = (x +! x', Grad $ add g g')

    forwardProp
      :: NN w v n o a
      -> v a
      -> (Vector (v a, v a, w a), (v a, v a, w a), v a, v a)
    forwardProp nn@(NN hiddenLayersWeights finalLayerWeights) input =
      (neuronValues', prefinalNeuronLayer, finalLayer, finalLayerDeriv)
      where
        neuronValues :: Vector (v a, v a, w a)
        neuronValues =
          V.scanl'
            f
            (input, cfmap (const 1) input, MC.outerProduct VC.empty VC.empty {-error "no weights before input layer"-})
            hiddenLayersWeights

        neuronValues' = V.unsafeInit neuronValues
        prefinalNeuronLayer :: (v a, v a, w a)
        prefinalNeuronLayer = V.unsafeLast neuronValues
        -- (neuronValues', prefinalNeuronLayer) = V.splitAt (V.length neuronValues - 1) neuronValues

        finalLayer      :: v a
        finalLayerDeriv :: v a
        (finalLayer, finalLayerDeriv) = g prefinalNeuronLayer finalLayerWeights

        f :: (v a, v a, w a) -> (v a, w a) -> (v a, v a, w a)
        f (prevLayer, _prevLayerDeriv, _) (bias, layer) =
          (cfmap (nonlinearity nn) ss, cfmap (nonlinearityDeriv nn) ss, layer)
          where
            ss = bias .+. MC.vecMulRight layer prevLayer

        g :: (v a, v a, w a) -> (v a, w a) -> (v a, v a)
        g (prevLayer, _prevLayerDeriv, _) (bias, layer) =
          (cfmap (output nn) ss, cfmap (outputDeriv nn) ss)
          where
            ss = bias .+. MC.vecMulRight layer prevLayer

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
  :: forall w v n o a. (Matrix NoConstraints w v, Functor w, Traversable w, ElemConstraints NoConstraints a)
  => (Vect NoConstraints v, ConstrainedFunctor NoConstraints v, Traversable v)
  => (Nonlinearity n, OutputType o n, Floating a)
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


instance forall w v n o k a. (Pretty (w a), Vect k v, ElemConstraints k a, Show a, Nonlinearity n, OutputType o n) => Pretty (NN w v n o a) where
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

