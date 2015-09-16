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

import Data.ConstrainedConvert (Convert)
import qualified Data.ConstrainedConvert as Conv
import Data.ConstrainedFunctor
import Data.MatrixClass (Matrix, (|+|))
import qualified Data.MatrixClass as MC
import Data.VectClass (Vect, (.+.))
import qualified Data.VectClass as VC
import Data.Zippable
import Nonlinearity
import Util

import Data.Aligned.Double (AlignedDouble)
import Data.Aligned.Float (AlignedFloat)
import Data.OpenBlasMatrix (OpenBlasMatrix)
import Data.AlignedStorableVector (AlignedStorableVector)

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

instance ( ConstrainedFunctor v
         , ConstrainedFunctor w
         , ElemConstraints v ~ ElemConstraints w
         )
         => ConstrainedFunctor (NN w v n o) where
  type ElemConstraints (NN w v n o) = ElemConstraints w
  {-# INLINABLE cfmap #-}
  cfmap f (NN layers (finBias, finWeights)) =
    NN (V.map (cfmap f *** cfmap f) layers) (cfmap f finBias, cfmap f finWeights)

instance (Matrix w v, Zippable w, Vect v) => Zippable (NN w v n o) where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith  = nnZipWith
  zipWith3 = nnZipWith3
  zipWith4 = nnZipWith4

instance ( Convert w w'
         , Convert v v'
         , ElemConstraints v ~ ElemConstraints w
         , ElemConstraints v' ~ ElemConstraints w'
         )
         => Convert (NN w v n o) (NN w' v' n o) where
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
  :: forall w v n o a. (Matrix w v, Vect v)
  => (ElemConstraints v ~ ElemConstraints w, ElemConstraints v a)
  => NN w v n o a
  -> [[[a]]]
toWeightList (NN hiddenLayers finalLayer) =
  map convertLayer $ V.toList hiddenLayers ++ [finalLayer]
  where
    convertLayer :: (v a, w a) -> [[a]]
    convertLayer (bias, weights) = L.zipWith (:) (VC.toList bias) (MC.toList weights)

nnZipWith
  :: (Matrix w v, Zippable w, Vect v, ElemConstraints v ~ ElemConstraints w)
  => (ElemConstraints w a)
  => (ElemConstraints w b)
  => (ElemConstraints w c)
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
  :: (Matrix w v, Zippable w, Vect v)
  => (ElemConstraints v ~ ElemConstraints w)
  => (ElemConstraints w a)
  => (ElemConstraints w b)
  => (ElemConstraints w c)
  => (ElemConstraints w d)
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
  :: (Matrix w v, Zippable w, Vect v)
  => (ElemConstraints v ~ ElemConstraints w)
  => (ElemConstraints w a)
  => (ElemConstraints w b)
  => (ElemConstraints w c)
  => (ElemConstraints w d)
  => (ElemConstraints w e)
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

-- nnZ = nnX + nnY
add :: (Matrix w v, Zippable w, Vect v, Num a)
    => (ElemConstraints v ~ ElemConstraints w, ElemConstraints w a)
    => NN w v n o a -> NN w v n o a -> NN w v n o a
add (NN layers final) (NN layers' final') =
  NN (V.zipWith f layers layers')
     (f final final')
  where
    f (xs, xss) (ys, yss) = (xs .+. ys, xss |+| yss)

-- nnZ = nnX + b * nnY
addScaled
  :: (Matrix w v, Zippable w, Vect v, Num a)
  => (ElemConstraints v ~ ElemConstraints w, ElemConstraints w a)
  => NN w v n o a -> a -> NN w v n o a -> NN w v n o a
-- addScaled nn b addend = nnZipWith (\x y -> x +! b *! y) nn addend
addScaled (NN layers final) b (NN layers' final') =
  NN (V.zipWith f layers layers')
     (f final final')
  where
    f (xs, xss) (ys, yss) = (VC.addScaled xs b ys, MC.addScaled xss b yss)

nnSize
  :: forall w v n o a. (Matrix w v, ConstrainedFunctor w, Vect v, Floating a)
  => (ElemConstraints v ~ ElemConstraints w, ElemConstraints w a)
  => NN w v n o a -> a
nnSize (NN layers fin) =
  sqrt $ V.sum (V.map layerSize layers) + layerSize fin
  where
    layerSize :: (v a, w a) -> a
    layerSize (bias, weightMatrix) = VC.normL2Square bias +! MC.normL2Square weightMatrix

differenceSize
  :: (Matrix w v, ConstrainedFunctor w, Zippable w)
  => (Vect v, Floating a)
  => (ElemConstraints v ~ ElemConstraints w, ElemConstraints w a)
  => NN w v n o a -> NN w v n o a -> a
differenceSize xs ys = nnSize $ addScaled xs (-1) ys

fromWeightList
  :: forall m w v n o a. (MonadError String m, Show a)
  => (Matrix w v, Vect v)
  => (ElemConstraints v ~ ElemConstraints w, ElemConstraints w a)
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
  :: forall m n o w v a. (Monad m, Show a)
  => (Matrix w v, Vect v)
  => (ElemConstraints v ~ ElemConstraints w, ElemConstraints w a)
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
  :: forall w v a n o. (Matrix w v, Floating a, Nonlinearity n, OutputType o n)
  => (Vect v, ConstrainedFunctor v)
  => (ElemConstraints v ~ ElemConstraints w, ElemConstraints w a)
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
  :: (Matrix w v, Vect v, ConstrainedFunctor v, Floating a)
  => (ElemConstraints v ~ ElemConstraints w, ElemConstraints w a)
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
  :: forall w v n o a. (Matrix w v, Traversable w, ElemConstraints w ~ IdConstraint)
  => (Vect v, ConstrainedFunctor v, Traversable v, ElemConstraints v ~ IdConstraint)
  => (Nonlinearity n, OutputType o n)
  => (Floating a)
  => Vector (v a, v a)
  -> NN w v n o a
  -> (a, Grad (NN w v n o) a)
targetFunctionGrad dataset =
  \nn -> second Grad $ grad' (targetFunction' dataset) nn
  where
    targetFunction'
      :: (Floating b, Mode b, ElemConstraints v b)
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

splitDataset
  :: forall v w a. (Vect v, Matrix w v, Show a)
  => (ElemConstraints v ~ ElemConstraints w, ElemConstraints w a)
  => Int -> Vector (v a, v a) -> (Vector (w a, w a), (w a, w a), Int)
splitDataset n xs =
  (fmap mkMatrix $ V.fromList fullChunks, mkMatrix lastChunk, lastSize)
  where
    (fullChunks, lastChunk) = splitVec n xs
    lastSize = V.length lastChunk
    mkMatrix :: Vector (v a, v a) -> (w a, w a)
    mkMatrix ws =
      -- Transpose resulting matrices since input vectors were column vectors,
      -- but in lists they're row vectors.
      (MC.transpose $ MC.fromList xss, MC.transpose $ MC.fromList yss)
      where
        xss :: [[a]]
        xss = {-L.transpose $-} V.toList $ fmap (VC.toList . fst) ws
        yss :: [[a]]
        yss = {-L.transpose $-} V.toList $ fmap (VC.toList . snd) ws

{-# INLINABLE backprop #-}
backprop
  :: forall w v n o a. (Matrix w v, ConstrainedFunctor w, Zippable w)
  => (Vect v, ConstrainedFunctor v)
  => (Floating a, ElemConstraints (NN w v n o) a, {-ElemConstraints w a,-} ElemConstraints v a)
  => (Nonlinearity n, OutputType o n)
  => (Show a)
  => Int
  -> Vector (v a, v a)
  -> NN w v n o a
  -> (a, Grad (NN w v n o) a)
backprop chunkSize dataset
  | V.null dataset = \nn -> (0, Grad $ cfmap (const 0) nn)
  | otherwise      = go
  where
    go :: NN w v n o a -> (a, Grad (NN w v n o) a)
    go nn@(NN hiddenLayers finalLayer@(_, finalLayerWeights)) =
      V.foldr'
        combineAdd
        (uncurry computeLast lastChunk)
        (fmap (uncurry computeFull) fullChunks)
      where
        (fullChunks, lastChunk, lastSize) = splitDataset chunkSize dataset

        computeFull = computeBatch hiddenLayersFull finalLayerFull
        hiddenLayersFull :: Vector (w a, w a)
        hiddenLayersFull = V.map (first (broadcastBias chunkSize)) hiddenLayers
        finalLayerFull :: (w a, w a)
        finalLayerFull = first (broadcastBias chunkSize) finalLayer

        computeLast = computeBatch hiddenLayersLast finalLayerLast
        hiddenLayersLast :: Vector (w a, w a)
        hiddenLayersLast = V.map (first (broadcastBias lastSize)) hiddenLayers
        finalLayerLast :: (w a, w a)
        finalLayerLast = first (broadcastBias lastSize) finalLayer

        broadcastBias :: Int -> v a -> w a
        broadcastBias size bias = MC.outerProduct bias broadcastVector
          where
            broadcastVector :: v a
            broadcastVector = VC.replicate size 1

        computeBatch :: Vector (w a, w a)
                     -> (w a, w a)
                     -> w a
                     -> w a
                     -> (a, Grad (NN w v n o) a)
        computeBatch hiddenLayersBroadcasted finalLayerBroadcasted xs ys
          | MC.rows prediction /= MC.rows ys =
            error "Size mismatch between network prediction and expected output"
          | MC.rows finalLayerVals /= MC.rows finalLayerWeights =
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
            hiddenLayersNeurons :: Vector (w a, w a, w a)
            prefinalNeuronLayer :: (w a, w a, w a)
            finalLayerVals      :: w a
            finalLayerDeriv     :: w a
            (hiddenLayersNeurons, prefinalNeuronLayer, finalLayerVals, finalLayerDeriv) =
              forwardProp nn hiddenLayersBroadcasted finalLayerBroadcasted xs
            prediction :: w a
            prediction = finalLayerVals
            mismatch :: w a
            mismatch = zipWith (-!) prediction ys
            err :: a
            err = MC.normL2Square mismatch
            finDeltas :: w a
            finDeltas = zipWith
                          (\m d -> 2 *! m *! d)
                          mismatch
                          finalLayerDeriv
            finBiasDerivs :: v a
            finDerivs     :: w a
            (finBiasDerivs, finDerivs) = mkLayerDeriv finDeltas prefinalNeuronLayer

            _prefinalLayerDelta :: w a
            prefinalDeltaWithLayer@(_prefinalLayerDelta, _) =
              mkDelta prefinalNeuronLayer (finDeltas, finalLayerWeights)

            -- Includes prefinalLayerDelta at the end. Does not need
            -- to include deltas for input layer since they won't be used
            hiddenLayerDeltas :: Vector (w a)
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

            mkLayerDeriv :: w a -> (w a, b, c) -> (v a, w a)
            mkLayerDeriv deltas (prevLayer, _prevLayerDeriv, _) = (biasDerivs, derivs)
              where
                biasDerivs = MC.sumColumns deltas
                -- prevLayer contains columns with layers for individual
                -- samples, but every column delta must be multiplied by row and
                -- resulting matrix derivs must be summed, which is achieved
                -- by matrix multiplication by transposed matrix.
                derivs     = MC.matrixMultByTransposedRight deltas prevLayer

            mkDelta
              :: (b, w a, w a)
              -> (w a, w a)
              -> (w a, w a)
            mkDelta (_layer, layerDeriv, weights) (deltas', weights') = (deltas, weights)
              where
                deltas :: w a
                deltas = zipWith mul layerDeriv
                       $ MC.matrixMultByTransposedLeft weights' deltas'
                       -- MC.vecMulRight (MC.transpose weights') deltas'
                mul deds weightedDeltas = deds *! weightedDeltas

    combineAdd :: (a, Grad (NN w v n o) a) -> (a, Grad (NN w v n o) a) -> (a, Grad (NN w v n o) a)
    combineAdd (x, Grad g) (x', Grad g') = (x +! x', Grad $ add g g')

    forwardProp
      :: NN w v n o a
      -> Vector (w a, w a)
      -> (w a, w a)
      -> w a
      -> (Vector (w a, w a, w a), (w a, w a, w a), w a, w a)
    forwardProp nn hiddenLayers finalLayer input =
      (neuronValues', prefinalNeuronLayer, finalLayerVals, finalLayerValsDeriv)
      where
        neuronValues :: Vector (w a, w a, w a)
        neuronValues =
          V.scanl'
            f
            (input, cfmap (const 1) input, MC.outerProduct VC.empty VC.empty {-error "no weights before input layer"-})
            hiddenLayers

        neuronValues' :: Vector (w a, w a, w a)
        neuronValues' = V.unsafeInit neuronValues
        prefinalNeuronLayer :: (w a, w a, w a)
        prefinalNeuronLayer = V.unsafeLast neuronValues
        -- (neuronValues', prefinalNeuronLayer) = V.splitAt (V.length neuronValues - 1) neuronValues

        finalLayerVals      :: w a
        finalLayerValsDeriv :: w a
        (finalLayerVals, finalLayerValsDeriv) = g prefinalNeuronLayer finalLayer

        f :: (w a, b, c) -> (w a, w a) -> (w a, w a, w a)
        f (prevLayer, _prevLayerDeriv, _prevWeights) (bias, weights) =
          (cfmap (nonlinearity nn) ss, cfmap (nonlinearityDeriv nn) ss, weights)
          where
            ss = bias |+| MC.matrixMult weights prevLayer

        g :: (w a, b, c) -> (w a, w a) -> (w a, w a)
        g (prevLayer, _prevLayerDeriv, _) (bias, layer) =
          (cfmap (output nn) ss, cfmap (outputDeriv nn) ss)
          where
            ss = bias |+| MC.matrixMult layer prevLayer

targetFunctionGradNumerical
  :: forall w v n o a. (Matrix w v, Functor w, Traversable w, ElemConstraints w a, ElemConstraints w ~ IdConstraint)
  => (Vect v, ConstrainedFunctor v, Traversable v, ElemConstraints v ~ IdConstraint)
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


instance forall w v n o a. (Pretty (w a), Vect v, ElemConstraints v a, Show a, Nonlinearity n, OutputType o n) => Pretty (NN w v n o a) where
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

