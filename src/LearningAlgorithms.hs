----------------------------------------------------------------------------
-- |
-- Module      :  LearningAlgorithms
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module LearningAlgorithms where

import Data.Vector (Vector)

import Data.V3
import Data.ConstrainedConvert (Convert)
import Data.ConstrainedFunctor
import qualified Data.ConstrainedConvert as Conv
import Data.Zippable
import NN (NeuralNetwork, NNVectorLike)
import qualified NN as NN
import Util

data ErrInfo a = ErrInfo
  { epsilon                :: a
  , errorFuncAbsoluteValue :: a
  }
  deriving (Show, Eq, Ord)

gradientDescentStep
  :: forall k nn v. (ConstrainedFunctor k nn, NNVectorLike k nn Double, NeuralNetwork k nn v Double, ElemConstraints k Double)
  => Double
  -> nn Double
  -> Vector (v Double, v Double)
  -> (Double, Grad nn Double, nn Double)
gradientDescentStep alpha nn dataset =
  (value, gradient, NN.addScaled nn (- alpha) (getGrad gradient'))
  where
    gradient :: Grad nn Double
    (value, gradient) = NN.targetFunctionGrad dataset nn
    gradient'         = cfmap (* alpha) gradient

gradientDescent
  :: forall nn k v. (ConstrainedFunctor k nn, NNVectorLike k nn Double, NeuralNetwork k nn v Double, ElemConstraints k Double)
  => Double
  -> nn Double
  -> Vector (v Double, v Double)
  -> IterateData nn () Double
gradientDescent alpha nn dataset =
  IterateData f () value0 gradient0Size
  where
    (value0, gradient0) = NN.targetFunctionGrad dataset nn
    gradient0Size = NN.size $ getGrad gradient0

    f :: () -> nn Double -> (Double, Grad nn Double, nn Double, ())
    f _ nn = (x, y, z, ())
      where
        (x, y, z) = gradientDescentStep alpha nn dataset


data DeltaInfo a = DeltaInfo
  { delta0        :: a
  , deltaMin      :: a
  , deltaMax      :: a
  , deltaIncrease :: a
  , deltaDecrease :: a
  }
  deriving (Show, Eq, Ord)

standardDeltaInfo :: (Floating a) => DeltaInfo a
standardDeltaInfo = DeltaInfo
  { delta0        = 0.1
  , deltaMin      = 1e-6
  , deltaMax      = 50
  , deltaIncrease = 1.2
  , deltaDecrease = 0.5
  }

data RPropState nn a = RPropState
  { rpropDeltas :: nn a
  , rpropGrad   :: Grad nn a
  }

deriving instance (Show (nn a)) => Show (RPropState nn a)
deriving instance (Eq (nn a)) => Eq (RPropState nn a)
deriving instance (Ord (nn a)) => Ord (RPropState nn a)

{-# INLINABLE rprop #-}
rprop
  :: forall k nn k' nn' v a.
     -- (ConstrainedFunctor k nn, Zippable k nn,
     --  NNVectorLike k nn Double, NeuralNetwork k nn v Double)
     (NNVectorLike k nn a, NeuralNetwork k nn v a, Convert k k' nn nn')
  => (ElemConstraints k a)
  => (ConstrainedFunctor k' nn', Zippable k' nn')
  => (ElemConstraints k' a, ElemConstraints k' (V3 a))
  => (Num a, Ord a)
  => DeltaInfo a
  -> nn a
  -> Vector (v a, v a)
  -> IterateData nn (RPropState nn a) a
rprop (DeltaInfo {delta0, deltaMin, deltaMax, deltaIncrease, deltaDecrease}) nn dataset =
  IterateData f (RPropState initialDeltas initialGrad) value0 gradient0Size
  where
    (value0, gradient0) = NN.targetFunctionGrad dataset nn
    gradient0Size       = NN.size $ getGrad gradient0

    initialDeltas = Conv.mapConverting (const delta0) nn
    initialGrad   = Grad $ Conv.mapConverting (const (0 :: a)) nn
    f :: RPropState nn a
      -> nn a
      -> ( a
         , Grad nn a
         , nn a
         , RPropState nn a
         )
    f (RPropState deltas prevGradient) nn =
      (value, gradient, nn', (RPropState deltas' (Grad prevGradient')))
      where
        (value, gradient) = NN.targetFunctionGrad dataset nn
        upd :: nn' (V3 a)
        upd               = zipWith4
                              g
                              (Conv.convertTo $ getGrad prevGradient)
                              (Conv.convertTo $ getGrad gradient)
                              (Conv.convertTo nn)
                              (Conv.convertTo deltas)
        nn'               = Conv.convertFrom $ cfmap (\(V3 x _ _) -> x) upd
        deltas'           = Conv.convertFrom $ cfmap (\(V3 _ y _) -> y) upd
        prevGradient'     = Conv.convertFrom $ cfmap (\(V3 _ _ z) -> z) upd

        g :: a -> a -> a -> a -> V3 a
        g dwPrev dw w delta
          | dwTrend > 0 = let delta'  = min (delta *! deltaIncrease) deltaMax
                              w'      = w -! signum dw *! delta'
                              dwPrev' = dw
                          in V3 w' delta' dwPrev'
          | dwTrend < 0 = let delta'  = max (delta *! deltaDecrease) deltaMin
                              w'      = w
                              dwPrev' = 0
                          in V3 w' delta' dwPrev'
          | otherwise   = let delta'  = delta
                              w'      = w -! signum dw *! delta'
                              dwPrev' = dw
                          in V3 w' delta' dwPrev'
          where
            dwTrend = dwPrev *! dw

data IterateData nn s a = IterateData
  { iterateFunc          :: s -> nn a -> (a, Grad nn a, nn a, s)
  , iterateFuncInitState :: s
  , iterateInitValue     :: a
  , iterateInitGradSize  :: a
  }

iteratedUpdates
  :: forall k nn s a. (NNVectorLike k nn a, ElemConstraints k a, Num a, Floating a, Ord a)
  => IterateData nn s a
  -> ErrInfo a
  -> nn a
  -> (a, nn a)
iteratedUpdates (IterateData f fState value0 gradient0Size) (ErrInfo {epsilon, errorFuncAbsoluteValue}) nn =
  go 0 (value0 + sqrt epsilon) nn fState
  where
    go :: Int -> a -> nn a -> s -> (a, nn a)
    go n prevTargetFuncVal nn state =
      -- trace (printf "#%d, error = %g, errDelta = %g" n targetFuncVal errDelta) $
      if errDelta > epsilon &&
         gradientSize > epsilon * gradient0Size &&
         abs targetFuncVal > errorFuncAbsoluteValue
      then go (n + 1) targetFuncVal nn' state'
      else (targetFuncVal, nn)
      where
        (targetFuncVal, gradient, nn', state') = f state nn
        gradientSize                           = NN.size $ getGrad gradient
        errDelta                               = abs (targetFuncVal - prevTargetFuncVal)

constantUpdates
  :: forall nn s a. IterateData nn s a
  -> Int
  -> nn a
  -> (a, nn a)
constantUpdates (IterateData f fState _value0 _gradient0Size) iterations nn =
  go 0 nn fState
  where
    go :: Int -> nn a -> s -> (a, nn a)
    go n nn state
      | n == iterations = (targetFuncVal, nn)
      | otherwise =
        go (n + 1) nn' state'
      where
        (targetFuncVal, _, nn', state') = f state nn
