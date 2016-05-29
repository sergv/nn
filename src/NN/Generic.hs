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

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module NN.Generic where

import Control.Arrow
import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.State
import Data.Coerce
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Monoid
import Data.Proxy
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (zipWith, zipWith3)
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.Random.Source.PureMT ()
import Numeric.AD hiding (grad, Grad)

import Data.ConstrainedFunctor
import Data.ConstrainedIsomorphism (ConstrainedIsomorphism)
import qualified Data.ConstrainedIsomorphism as Iso
import Data.Grad
import Data.MatrixClass (Matrix, (|+|))
import qualified Data.MatrixClass as MC
import Data.Nonlinearity
import Data.SpecialisedFunction
import Data.VectClass (Vect, (.+.))
import qualified Data.VectClass as VC
import Data.Zippable
import NN.Description
import Util

-- import Data.Aligned.Double (AlignedDouble)
-- import Data.Aligned.Float (AlignedFloat)
-- import Data.AlignedStorableVector (AlignedStorableVector)
-- import Data.OpenBlasMatrix (OpenBlasMatrix)

-- w - matrix
-- v - vector
-- n - nonlinearity type
-- o - output type
-- a - element type
data NN w v n o a =
  NN {-# UNPACK #-} !(Vector (v a, w a)) -- ^ Hidden layers, each is a pair of bias vector and
                                         -- weights matrix.
     {-# UNPACK #-} !(v a, w a)          -- ^ Output layer bias and weights.
     deriving (Functor, Foldable, Traversable)

deriving instance (Show (v a), Show (w a)) => Show (NN w v n o a)
deriving instance (Eq (v a), Eq (w a))     => Eq (NN w v n o a)
deriving instance (Ord (v a), Ord (w a))   => Ord (NN w v n o a)

instance (NFData (v a), NFData (w a)) => NFData (NN w v n o a) where
  rnf (NN xs fin) = rnf xs `seq` rnf fin

instance
  ( ConstrainedFunctor v
  , ConstrainedFunctor w
  , ElemConstraints v ~ ElemConstraints w
  ) => ConstrainedFunctor (NN w v n o) where
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

instance
  ( ConstrainedIsomorphism w w'
  , ConstrainedIsomorphism v v'
  , ElemConstraints v ~ ElemConstraints w
  , ElemConstraints v' ~ ElemConstraints w'
  ) => ConstrainedIsomorphism (NN w v n o) (NN w' v' n o) where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   (NN hiddenLayers outputLayer) =
    NN (V.map conv hiddenLayers) (conv outputLayer)
    where
      conv = Iso.convertTo *** Iso.convertTo
  convertFrom (NN hiddenLayers outputLayer) =
    NN (V.map conv hiddenLayers) (conv outputLayer)
    where
      conv = Iso.convertFrom *** Iso.convertFrom

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

instance
  ( Matrix w v
  , ConstrainedFunctor w
  -- , Zippable w
  , ElemConstraints v ~ ElemConstraints w
  ) => NNDescription (NN w v) where
  toDescription
    :: forall n o a m. (MonadError String m, ElemConstraints v a)
    => NN w v n o a
    -> m (Description n o a)
  toDescription (NN hiddenWeights outputLayer@(finalBias, _)) = do
    inputSize <- case toList hiddenWeights of
                   []      -> return finalSize
                   (_, w):_
                     | inputSize > 0 -> return inputSize
                     | otherwise             ->
                       throwError "Invalid NN: first layer is empty"
                     where
                       inputSize = MC.columns w
    Description
      <$> pure inputSize
      <*> pure finalSize
      <*> traverse convertLayer (toList hiddenWeights)
      <*> convertLayer outputLayer
    where
      finalSize :: Int
      finalSize = VC.length finalBias
      convertLayer :: (v a, w a) -> m (NonEmpty a, NonEmpty (NonEmpty a))
      convertLayer (bias, w) = do
        w' <- for (MC.toList w) $ \case
                []   -> throwError "Invalid nn: weight matrix has empty row"
                w:ws -> return $ w :| ws
        case (bias', w') of
          ([], _)             -> throwError "Invalid NN layer: empty bias"
          (_,  [])            -> throwError "Invalid NN layer: empty list of weights"
          (b:bias'', w : w'') -> do
            unless (all ((== length w) . length) w'') $
              throwError "Invalid NN layer: not all rows have the same number of columns"
            return (b :| bias'', w :| w'')
        where
          bias' = VC.toList bias

  fromDescription
    :: forall m n o a. (MonadError String m, Show a)
    => (Matrix w v, Vect v)
    => (ElemConstraints v ~ ElemConstraints w, ElemConstraints w a)
    => Description n o a
    -> m (NN w v n o a)
  fromDescription Description {descriptionInputSize, descriptionOutputSize, descriptionHiddenLayers, descriptionFinalLayer} = do
    when (descriptionInputSize == 0) $
      throwError "Input size is 0"
    when (descriptionOutputSize == 0) $
      throwError "Final size is 0"
    NN <$> (V.fromList <$> traverse convertLayer descriptionHiddenLayers)
       <*> convertLayer descriptionFinalLayer
    where
      convertLayer :: (NonEmpty a, NonEmpty (NonEmpty a)) -> m (v a, w a)
      convertLayer (bias, ws'@(w :| ws))
        | biasLen > 0 && biasLen == NE.length ws' && all (\w' -> NE.length w' == NE.length w) ws =
          return (VC.fromList $ toList bias, MC.fromList $ map toList $ toList ws')
        | otherwise =
          throwError $ "Invalid layer, all rows must be of the same lenght: "
            ++ show ws'
            ++ ", including bias: " ++ show bias
        where
          biasLen = NE.length bias

forwardPropagate
  :: forall w v a n o. (Matrix w v, Floating a)
  => (VectorisedNonlinearity n v, VectorisedNonlinearity o v)
  => (Vect v, ConstrainedFunctor v)
  => (ElemConstraints v ~ ElemConstraints w, ElemConstraints w a)
  => NN w v n o a
  -> v a
  -> v a
forwardPropagate nn@(NN hiddenLayers outputLayer) input =
  f (nonlinearity (OutputProxy nn))
    (V.foldl' (f (nonlinearity (NonlinearityProxy nn))) input hiddenLayers)
    outputLayer
  where
    f :: (v a -> v a) -> v a -> (v a, w a) -> v a
    f activation prev (bias, layer) =
      activation $ bias .+. MC.vecMulRight layer prev

targetFunction
  :: (Matrix w v, Vect v, ConstrainedFunctor v, Floating a)
  => (ElemConstraints v ~ ElemConstraints w, ElemConstraints w a)
  => (VectorisedNonlinearity n v, VectorisedNonlinearity o v)
  => Vector (v a, v a)
  -> NN w v n o a
  -> a
targetFunction dataset nn =
  V.sum $
  V.map (\(x, y) -> VC.normL2Square $
                    zipWith (-!) (forwardPropagate nn x) y)
        dataset

-- | Compute target function gradient with the help of ad.
targetFunctionGradAD
  :: forall w v n o a. (Matrix w v, Traversable w, ElemConstraints w ~ IdConstraint)
  => (Vect v, ConstrainedFunctor v, Traversable v, ElemConstraints v ~ IdConstraint)
  => (VectorisedNonlinearity n v, VectorisedNonlinearity o v)
  => (Floating a)
  => Vector (v a, v a)
  -> NN w v n o a
  -> (a, Grad (NN w v n o) a)
targetFunctionGradAD dataset =
  \nn -> second Grad $ grad' (targetFunction' dataset) nn
  where
    targetFunction'
      :: (Floating b, Mode b, ElemConstraints v b)
      => Vector (v (Scalar b), v (Scalar b))
      -> NN w v n o b
      -> b
    targetFunction' dataset =
      targetFunction (V.map (fmap auto *** fmap auto) dataset)

splitDataset
  :: forall v w a. (Vect v, Matrix w v, Show a)
  => (ElemConstraints v ~ ElemConstraints w, ElemConstraints w a)
  => Int                 -- ^ Chunk size
  -> Vector (v a, v a)   -- ^ Initial dataset
  -> ( Vector (w a, w a) -- ^ Chunks
     , Maybe (w a, w a)  -- ^ Last chunk
     , Int               -- ^ Last chunk Size
     )
splitDataset n xs =
  (mkMatrix <$> V.fromList fullChunks, lastChunk', lastSize)
  where
    (fullChunks, lastChunk) = splitVec n xs
    lastSize = V.length lastChunk
    lastChunk' :: Maybe (w a, w a)
    lastChunk'
      | lastSize == 0 = Nothing
      | otherwise     = Just $ mkMatrix lastChunk
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
-- | Backpropagate neural network on a given @dataset@, chunk by chunk.
backprop
  :: forall w v n o a. (Matrix w v, ConstrainedFunctor w, Zippable w)
  => (Vect v, ConstrainedFunctor v)
  => (Floating a, ElemConstraints (NN w v n o) a)
  => (SpecialisedFunction (FuncWithDeriv n) (w a) (w a, Grad w a))
  => (SpecialisedFunction (FuncWithDeriv o) (w a) (w a, Grad w a))
  => (Show a)
  => Int
  -> Vector (v a, v a)
  -> NN w v n o a
  -> (a, Grad (NN w v n o) a)
backprop chunkSize dataset
  | V.null dataset = zeroRes
  | otherwise      = go
  where
    zeroRes nn = (0, Grad $ cfmap (const 0) nn)
    go :: NN w v n o a -> (a, Grad (NN w v n o) a)
    go nn@(NN hiddenLayers outputLayer@(_, finalLayerWeights)) =
      V.foldr'
        combineAdd
        lastRes
        (fmap (uncurry computeFull) fullChunks)
      where
        (fullChunks, lastChunk, lastSize) = splitDataset chunkSize dataset

        computeFull :: w a -> w a -> (a, Grad (NN w v n o) a)
        computeFull = computeBatch hiddenLayersFull outputLayerFull
          where
            hiddenLayersFull :: Vector (w a, w a)
            hiddenLayersFull = V.map (first (broadcastBias chunkSize)) hiddenLayers
            outputLayerFull :: (w a, w a)
            outputLayerFull = first (broadcastBias chunkSize) outputLayer

        lastRes :: (a, Grad (NN w v n o) a)
        lastRes = case lastChunk of
                    Nothing     -> zeroRes nn
                    Just (x, y) -> computeLast x y
        computeLast :: w a -> w a -> (a, Grad (NN w v n o) a)
        computeLast = computeBatch hiddenLayersLast outputLayerLast
          where
            hiddenLayersLast :: Vector (w a, w a)
            hiddenLayersLast = V.map (first (broadcastBias lastSize)) hiddenLayers
            outputLayerLast :: (w a, w a)
            outputLayerLast = first (broadcastBias lastSize) outputLayer

        -- Lift bias vector into single-column bias matrix.
        broadcastBias :: Int -> v a -> w a
        broadcastBias size biasVector = MC.outerProduct biasVector broadcastVector
          where
            broadcastVector :: v a
            broadcastVector = VC.replicate size 1

        mkNNGrad :: NN (Grad w) (Grad v) n o a -> Grad (NN w v n o) a
        mkNNGrad = coerce

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
            --         , "outputLayer          = " <> pretty outputLayer
            --         , "finalLayerDeriv      = " <> pretty finalLayerDeriv
            --         , "finalLayerWeights    = " <> PP.align (pretty finalLayerWeights)
            --         ]) $
            (err, mkNNGrad $ NN hiddenLayersDerivs (finBiasDerivs, finDerivs))
          where
            -- NB full neurons of hidden layers can be obtained by using
            -- V.snoc hiddenLayersNeurons prefinalNeuronLayer
            hiddenLayersNeurons :: Vector (w a, Grad w a, w a)
            prefinalNeuronLayer :: (w a, Grad w a, w a)
            finalLayerVals      :: w a
            finalLayerDeriv     :: Grad w a
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
                          (getGrad finalLayerDeriv)
            finBiasDerivs :: Grad v a
            finDerivs     :: Grad w a
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
            hiddenLayersDerivs :: Vector (Grad v a, Grad w a)
            hiddenLayersDerivs =
              zipWith mkLayerDeriv hiddenLayerDeltas hiddenLayersNeurons

            mkLayerDeriv :: w a -> (w a, b, c) -> (Grad v a, Grad w a)
            mkLayerDeriv deltas (prevLayer, _prevLayerDeriv, _) =
              (Grad biasDerivs, Grad derivs)
              where
                biasDerivs = MC.sumColumns deltas
                -- prevLayer contains columns with layers for individual
                -- samples, but every column delta must be multiplied by row and
                -- resulting matrix derivs must be summed, which is achieved
                -- by matrix multiplication by transposed matrix.
                derivs     = MC.matrixMultByTransposedRight deltas prevLayer

            mkDelta
              :: (b, Grad w a, w a)
              -> (w a, w a)
              -> (w a, w a)
            mkDelta (_layer, Grad layerDeriv, weights) (deltas', weights') =
              (deltas, weights)
              where
                deltas :: w a
                deltas = zipWith mul layerDeriv
                       $ MC.matrixMultByTransposedLeft weights' deltas'
                mul deds weightedDeltas = deds *! weightedDeltas

    combineAdd :: (a, Grad (NN w v n o) a) -> (a, Grad (NN w v n o) a) -> (a, Grad (NN w v n o) a)
    combineAdd (x, Grad g) (x', Grad g') = (x +! x', Grad $ add g g')

    forwardProp
      :: NN w v n o a
      -> Vector (w a, w a)
      -> (w a, w a)
      -> w a
      -> (Vector (w a, Grad w a, w a), (w a, Grad w a, w a), w a, Grad w a)
    forwardProp nn hiddenLayers outputLayer input =
      (neuronValues', prefinalNeuronLayer, finalLayerVals, finalLayerValsDeriv)
      where
        neuronValues :: Vector (w a, Grad w a, w a)
        neuronValues = V.scanl' f inputLayer hiddenLayers

        inputLayer :: (w a, Grad w a, w a)
        inputLayer = ( input
                     , Grad $ cfmap (const 1) input
                     , MC.outerProduct VC.empty VC.empty {-error "no weights before input layer"-}
                     )

        neuronValues' :: Vector (w a, Grad w a, w a)
        neuronValues' = V.unsafeInit neuronValues
        prefinalNeuronLayer :: (w a, Grad w a, w a)
        prefinalNeuronLayer = V.unsafeLast neuronValues
        -- (neuronValues', prefinalNeuronLayer) = V.splitAt (V.length neuronValues - 1) neuronValues

        finalLayerVals      :: w a
        finalLayerValsDeriv :: Grad w a
        (finalLayerVals, finalLayerValsDeriv) = g prefinalNeuronLayer outputLayer

        f :: (w a, b, c) -> (w a, w a) -> (w a, Grad w a, w a)
        f (prevLayer, _prevLayerDeriv, _prevWeights) (bias, weights) =
          (nonlin, nonlinDeriv, weights)
          where
            ss :: w a
            ss = bias |+| MC.matrixMult weights prevLayer
            (nonlin, nonlinDeriv) = sfmap nnNonlinDerivProxy ss

        g :: (w a, b, c) -> (w a, w a) -> (w a, Grad w a)
        g (prevLayer, _prevLayerDeriv, _) (bias, layer) =
          sfmap nnOutputDerivProxy ss
          -- (cfmap (output nn) ss, cfmap (outputDeriv nn) ss)
          where
            ss = bias |+| MC.matrixMult layer prevLayer
            -- (output, outputDeriv) = sfmap nnOutputDerivProxy ss

        nnNonlinDerivProxy :: Proxy (FuncWithDeriv n)
        nnNonlinDerivProxy = addFuncWithDerivInProxy $ NonlinearityProxy nn
        nnOutputDerivProxy :: Proxy (FuncWithDeriv o)
        nnOutputDerivProxy = addFuncWithDerivInProxy $ OutputProxy nn

-- | Calculate target function gradient numerically.
targetFunctionGradNumerical
  :: forall w v n o a. (Matrix w v, Functor w, Traversable w, ElemConstraints w a, ElemConstraints w ~ IdConstraint)
  => (Vect v, ConstrainedFunctor v, Traversable v, ElemConstraints v ~ IdConstraint)
  => (Floating a)
  => (VectorisedNonlinearity n v, VectorisedNonlinearity o v)
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


instance
  forall w v n o a.
  ( Pretty (w a)
  , Vect v
  , ElemConstraints v a
  , Show a
  , PrettyProxy n
  , PrettyProxy o
  ) => Pretty (NN w v n o a) where
  pretty nn@(NN hiddenLayers outputLayer) = PP.vsep
    [ "Nonlinearity: " <> prettyProxy (NonlinearityProxy nn)
    , "Output: "       <> prettyProxy (OutputProxy nn)
    , "HiddenLayers: " <> PP.hcat
                          (PP.punctuate (PP.line <> PP.line) $
                           V.toList $
                           V.map showLayer hiddenLayers)
    , "OutputLayer: "  <> showLayer outputLayer
    ]
    where
      showLayer :: (v a, w a) -> Doc
      showLayer (bias, weights) =
        "bias:   " <> showWeights bias PP.<$>
        "weighs: " <> PP.align (pretty weights)
      showWeights :: v a -> Doc
      showWeights = PP.hcat . PP.punctuate PP.comma . map prettyShow . VC.toList

