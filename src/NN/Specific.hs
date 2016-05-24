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

{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

-- {-# OPTIONS_GHC -fno-warn-orphans #-}

module NN.Specific where

import Control.Arrow
import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Monoid
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Numeric.AD hiding (gradientDescent, Grad)

import Data.Random.Source.PureMT ()

import Data.ConstrainedIsomorphism (ConstrainedIsomorphism(..))
import Data.ConstrainedFunctor
import Data.Grad
import Data.Nonlinearity
import Data.SpecialisedFunction
import Data.Zippable
import Util

import NN.Description

-- import Debug.Trace

data NN n o a =
  NN
    -- ^ Hidden layers, starting from the input layer. Each layer is an m by n matrix,
    -- where m ranges over neurons in this layer and n depends on number of neurons
    -- in previous layer plus 1.
    (Vector (Vector (Vector a)))
    -- ^ Final layer.
    (Vector (Vector a))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance forall n o a. (Show a, PrettyProxy n, PrettyProxy o) => Pretty (NN n o a) where
  pretty nn@(NN hiddenLayers outputLayer) = PP.vsep
    [ "Nonlinearity: " <> prettyProxy (NonlinearityProxy nn)
    , "Output: "       <> prettyProxy (OutputProxy nn)
    , "HiddenLayers: " <> PP.hcat
                          (PP.punctuate (PP.line <> PP.line) $
                           toList $
                           V.map showLayer hiddenLayers)
    , "OutputLayer: "  <> showLayer outputLayer
    ]
    where
      showLayer :: Vector (Vector a) -> Doc
      showLayer =
        PP.vsep .
        map (PP.hcat . PP.punctuate PP.comma . map prettyShow . toList) .
        toList

instance (NFData a) => NFData (NN n o a) where
  rnf (NN xs fin) = rnf xs `seq` rnf fin

instance ConstrainedFunctor (NN n o) where
  type ElemConstraints (NN n o) = IdConstraint
  {-# INLINABLE cfmap #-}
  cfmap = fmap

instance Zippable (NN n o) where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith  = nnZipWith
  zipWith3 = nnZipWith3
  zipWith4 = nnZipWith4

instance ConstrainedIsomorphism (NN n o) (NN n o) where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = id
  convertFrom = id

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

add :: forall a n o. (Floating a) => NN n o a -> NN n o a -> NN n o a
-- addScaled b (NN xs) (NN ys) = NN $ zipWith (\x y -> V.zipWith (\x' y' -> V.zipWith () x' y') x y) xs ys
add = nnZipWith (+!)

-- nnZ = b * nnX + NNy
addScaled :: forall a n o. (Floating a) => NN n o a -> a -> NN n o a -> NN n o a
-- addScaled b (NN xs) (NN ys) = NN $ zipWith (\x y -> V.zipWith (\x' y' -> V.zipWith () x' y') x y) xs ys
addScaled nn b addend = nnZipWith (\x y -> x +! b *! y) nn addend

nnSize :: (Floating a) => NN n o a -> a
nnSize (NN layers fin) =
  sqrt $ V.sum (V.map layerSize layers) +! layerSize fin
  where
    layerSize = V.sum . V.map (V.sum . V.map (^(2 :: Int)))

differenceSize :: (Floating a) => NN n o a -> NN n o a -> a
differenceSize xs ys = nnSize $ addScaled xs (-1) ys

instance NNDescription NN where
  toDescription
    :: forall n o a m. (MonadError String m)
    => NN n o a
    -> m (Description n o a)
  toDescription (NN hiddenWeights finalWeights) = do
    inputSize <- case toList <$> toList hiddenWeights of
                   []      -> return finalSize
                   (x:_):_
                     | inputSizeWithBias > 1 -> return $ inputSizeWithBias - 1
                     | otherwise             ->
                       throwError $ "Invalid NN: "
                         ++ "input size with bias is not greater than 1: "
                         ++ show inputSizeWithBias
                     where
                       inputSizeWithBias = V.length x
                   []:_ -> throwError "Invalid NN: first layer is empty"
    Description
      <$> pure inputSize
      <*> pure finalSize
      <*> traverse convertLayer (toList hiddenWeights)
      <*> convertLayer finalWeights
    where
      finalSize :: Int
      finalSize = V.length finalWeights
      convertLayer :: Vector (Vector a) -> m (NonEmpty a, NonEmpty (NonEmpty a))
      convertLayer xss = do
        (bs, ws) <- fmap unzip $ for (toList xss) $ \xs ->
          case toList xs of
            []         -> throwError "Invalid NN: empty row in bias + weight matrix"
            [_]        -> throwError "Invalid NN: empty row in weight matrix"
            bias:x:xs' -> return (bias, x :| xs')
        case (bs, ws) of
          ([], _)           -> throwError "Invalid NN: empty bias"
          (_,  [])          -> throwError "Invalid NN: no rows in weights matrix"
          (b : bs', w : ws) -> return (b :| bs', w :| ws)

  fromDescription
    :: forall m n o a. (MonadError String m, Show a)
    => Description n o a
    -> m (NN n o a)
  fromDescription Description {descriptionInputSize, descriptionOutputSize, descriptionHiddenLayers, descriptionFinalLayer} = do
    when (descriptionInputSize == 0) $
      throwError "Input size is 0"
    when (descriptionOutputSize == 0) $
      throwError "Final size is 0"
    NN <$> (V.fromList <$> traverse convertLayer descriptionHiddenLayers)
       <*> convertLayer descriptionFinalLayer
    where
      convertLayer :: (NonEmpty a, NonEmpty (NonEmpty a)) -> m (Vector (Vector a))
      convertLayer (bias, ws'@(w :| ws))
        | biasLen > 0 && biasLen == NE.length ws' && all (\w' -> NE.length w' == NE.length w) ws =
          return
            $ V.fromList
            $ toList
            $ V.fromList . toList <$> NE.zipWith NE.cons bias ws'
        | otherwise =
          throwError $ "Invalid layer, all rows must be of the same length: "
            ++ show ws'
            ++ ", including bias: " ++ show bias
        where
          biasLen = NE.length bias

-- {-# SPECIALIZE forwardPropagate :: NN n o Double -> Vector Double -> Vector Double #-}
forwardPropagate
  :: forall a n o. (Floating a)
  => (VectorisedNonlinearity n Vector, VectorisedNonlinearity o Vector)
  => NN n o a
  -> Vector a
  -> Vector a
forwardPropagate nn@(NN hiddenLayers outputLayer) input =
  f (nonlinearity (OutputProxy nn))
    (V.foldl' (f (nonlinearity (NonlinearityProxy nn))) input hiddenLayers)
    outputLayer
  where
    f :: (Vector a -> Vector a) -> Vector a -> Vector (Vector a) -> Vector a
    f activation prev layer =
      activation $
      V.map (\ws -> V.head ws +! dot prev (V.tail ws)) layer

targetFunction
  :: (Floating a)
  => (VectorisedNonlinearity n Vector, VectorisedNonlinearity o Vector)
  => Vector (Vector a, Vector a)
  -> NN n o a
  -> a
targetFunction dataset nn =
  V.sum $
  V.map (\(x, y) -> vectorSize' $ V.zipWith (-!) (forwardPropagate nn x) y)
        dataset

targetFunctionGrad
  :: forall n o a. (Floating a)
  => (VectorisedNonlinearity n Vector, VectorisedNonlinearity o Vector)
  => Vector (Vector a, Vector a)
  -> NN n o a
  -> (a, Grad (NN n o) a)
-- targetFunctionGrad dataset nn =
--   second Grad $ grad' (targetFunction (V.map (V.map auto *** V.map auto) dataset)) nn
targetFunctionGrad dataset =
  second Grad . grad' (targetFunction' dataset)
  where
    targetFunction'
      :: (Floating b, Mode b)
      => Vector (Vector (Scalar b), Vector (Scalar b))
      -> NN n o b
      -> b
    targetFunction' dataset =
      targetFunction (V.map (V.map auto *** V.map auto) dataset)

backprop
  :: forall n o a. (Floating a, Show a)
  => (SpecialisedFunction n (Identity a) (Identity a))
  => (SpecialisedFunction o (Identity a) (Identity a))
  => (SpecialisedFunction (Deriv n) (Identity a) (Grad Identity a))
  => (SpecialisedFunction (Deriv o) (Identity a) (Grad Identity a))
  => Vector (Vector a, Vector a)
  -> NN n o a
  -> (a, Grad (NN n o) a)
backprop dataset = go
  where
    go :: NN n o a -> (a, Grad (NN n o) a)
    go nn@(NN _hiddenLayerWeights finalLayerWeights) =
      V.foldr'
        combineAdd
        (0, zeroGrad)
        (fmap (uncurry computeSample) dataset)
      where
        zeroGrad :: Grad (NN n o) a
        zeroGrad = Grad (fmap (const 0) nn)

        computeSample :: Vector a -> Vector a -> (a, Grad (NN n o) a)
        computeSample x y
          | V.length prediction /= V.length y =
            error "Size mismatch between network prediction and expected output"
          | V.length outputLayer /= V.length finalLayerWeights =
            error "Size mismatch between final layer sums and final layer neurons"
          | otherwise =
            -- trace (display' $ PP.vcat
            --         [ "Specific"
            --         , "hiddenLayersNeurons = " <> pretty hiddenLayersNeurons
            --         , "prefinalNeuronLayer = " <> pretty prefinalNeuronLayer
            --         , "outputLayer          = " <> pretty outputLayer
            --         ]) $
            (err, Grad $ NN hiddenLayersDerivs finalDerivs)
          where
            -- NB full neurons of hidden layers can be obtained by using
            -- V.snoc hiddenLayersNeurons prefinalNeuronLayer
            hiddenLayersNeurons :: Vector (Vector (a, a, Vector a))
            (hiddenLayersNeurons, prefinalNeuronLayer, outputLayer) = forwardProp nn x
            prediction :: Vector a
            prediction = fmap (\(x, _deds, _ws) -> x) outputLayer
            mismatch :: Vector a
            mismatch = V.zipWith (-!) prediction y
            err :: a
            err = vectorSize' mismatch
            finalDeltas :: Vector a
            finalDeltas = V.zipWith
                            (\m d -> 2 *! m *! d)
                            mismatch
                            (V.map (\(_x, deds, _ws) -> deds) outputLayer)
            finalDerivs :: Vector (Vector a)
            finalDerivs = mkLayerDeriv finalDeltas prefinalNeuronLayer

            _prefinalLayerDelta :: Vector a
            prefinalDeltaWithLayer@(_prefinalLayerDelta, _) =
              mkDelta prefinalNeuronLayer (finalDeltas, outputLayer)

            -- Includes prefinalLayerDelta at the end. Does not need
            -- to include deltas for input layer since they won't be used
            hiddenLayerDeltas :: Vector (Vector a)
            hiddenLayerDeltas =
              V.map fst $
              -- Same as V.tail $ V.scanr' ...
              V.prescanr' mkDelta prefinalDeltaWithLayer hiddenLayersNeurons
              -- Old version
              -- V.scanr' mkDelta prefinalDeltaWithLayer hiddenLayersNeurons

            -- Zipping deltas for all but first layer and neuron values for
            -- all but prefinal layer.
            hiddenLayersDerivs :: Vector (Vector (Vector a))
            hiddenLayersDerivs =
              V.zipWith mkLayerDeriv hiddenLayerDeltas hiddenLayersNeurons

            mkLayerDeriv :: Vector a -> Vector (a, a, Vector a) -> Vector (Vector a)
            mkLayerDeriv deltas prevLayer =
              V.map (\delta -> V.cons delta $ V.map (\(x, _deds, _ws) -> delta *! x) prevLayer) deltas

            mkDelta
              :: Vector (a, a, Vector a)
              -> (Vector a, Vector (a, a, Vector a))
              -> (Vector a, Vector (a, a, Vector a))
            mkDelta layer (deltas', layer') = (deltas, layer)
              where
                deltas :: Vector a
                deltas =
                  V.zipWith (\(_x, deds, _ws) weightedDeltas -> weightedDeltas *! deds) layer $
                  V.foldr1 (V.zipWith (+!)) $
                  V.zipWith (\(_, _, ws) delta ->
                               -- Ignore first weight since that's the weight for constant bias.
                               -- And there's no delta for bias.
                               V.map (*! delta) $ V.tail ws) layer' deltas'

        combineAdd :: (a, Grad (NN n o) a) -> (a, Grad (NN n o) a) -> (a, Grad (NN n o) a)
        combineAdd (x, Grad g) (x', Grad g') = (x +! x', Grad $ add g g')

    forwardProp
      :: NN n o a
      -> Vector a
      -> (Vector (Vector (a, a, Vector a)), Vector (a, a, Vector a), Vector (a, a, Vector a))
    forwardProp nn@(NN hiddenLayersWeights finalLayerWeights) input =
      (neuronValues', prefinalNeuronLayer, outputLayer)
      where
        neuronValues :: Vector (Vector (a, a, Vector a))
        neuronValues =
          V.scanl'
            f
            -- (fmap (\x -> (x, 1, error "no weights before input layer")) input)
            (fmap (\x -> (x, 1, V.empty)) input)
            hiddenLayersWeights

        neuronValues' = V.unsafeInit neuronValues
        prefinalNeuronLayer :: Vector (a, a, Vector a)
        prefinalNeuronLayer = V.unsafeLast neuronValues
        -- (neuronValues', prefinalNeuronLayer) = V.splitAt (V.length neuronValues - 1) neuronValues

        outputLayer :: Vector (a, a, Vector a)
        outputLayer = g prefinalNeuronLayer finalLayerWeights

        f :: Vector (a, a, Vector a) -> Vector (Vector a) -> Vector (a, a, Vector a)
        f prevLayer = fmap useLayer
          where
            useLayer :: Vector a -> (a, a, Vector a)
            useLayer ws = (x, deds, ws)
              where
                nn'                  = NonlinearityProxy nn
                s                    = V.head ws +! dot' prevLayer (V.tail ws)
                Identity x           = sfmap nn' $ Identity s
                Grad (Identity deds) = sfmap (addDerivInProxy nn') $ Identity s

        g :: Vector (a, a, Vector a) -> Vector (Vector a) -> Vector (a, a, Vector a)
        g prevLayer =
          fmap (\ws -> let s                    = V.head ws +! dot' prevLayer (V.tail ws)
                           nn'                  = OutputProxy nn
                           Identity x           = sfmap nn' $ Identity s
                           Grad (Identity deds) = sfmap (addDerivInProxy nn') $ Identity s
                       in (x, deds, ws))

        dot' :: Vector (a, b, c) -> Vector a -> a
        dot' xs ys
          | xsLen /= ysLen =
            error $ "dot': cannot take dot products for vectors of different length: " ++
              "|xs| = " ++ show xsLen ++ ", |ys| = " ++ show ysLen
          | otherwise      =
            V.foldr (+!) 0 $ V.zipWith (\(x, _deds, _ws) y -> x *! y) xs ys
          where
            xsLen = V.length xs
            ysLen = V.length ys

targetFunctionGradNumerical
  :: forall n o a. (Floating a)
  => (VectorisedNonlinearity n Vector, VectorisedNonlinearity o Vector)
  => a
  -> Vector (Vector a, Vector a)
  -> NN n o a
  -> (a, Grad (NN n o) a)
targetFunctionGradNumerical epsilon dataset nn =
  (targetFunction dataset nn, Grad grad)
  where
    nn' :: NN n o (Int, a, a, a)
    nn' = evalState (traverse enum nn) 0
    enum :: a -> State Int (Int, a, a, a)
    enum x = (, x, x - epsilon, x + epsilon) <$> get <* modify (+1)

    grad :: NN n o a
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

dot :: forall a. (Floating a) => Vector a -> Vector a -> a
dot xs ys
  | xsLen /= ysLen =
    error $ "dot: cannot take dot products for vectors of different length: " ++
      "|xs| = " ++ show xsLen ++ ", |ys| = " ++ show ysLen
  | otherwise      =
    V.foldr (+!) 0 $ V.zipWith (*!) xs ys
  where
    xsLen = V.length xs
    ysLen = V.length ys

{-# INLINABLE vectorSize #-}
vectorSize :: (Floating a) => Vector a -> a
vectorSize = sqrt . vectorSize'

{-# INLINABLE vectorSize' #-}
vectorSize' :: (Floating a) => Vector a -> a
vectorSize' = V.sum . V.map (\x -> x *! x) -- (^(2 :: Int))

