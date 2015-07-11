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
import Text.Printf

import NN (NeuralNetwork, NNVectorLike)
import qualified NN as NN
import Util
import Util.ConstrainedFunctor
import Util.V3
import Util.Zippable


import Debug.Trace

data ErrInfo = ErrInfo
  { epsilon                :: Double
  , errorFuncAbsoluteValue :: Double
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
  -> IterateData nn ()
gradientDescent alpha nn dataset =
  IterateData f () value0 gradient0Size
  where
    (value0, gradient0) = NN.targetFunctionGrad dataset nn
    gradient0Size = NN.size $ getGrad gradient0

    f :: () -> nn Double -> (Double, Grad nn Double, nn Double, ())
    f _ nn = (x, y, z, ())
      where
        (x, y, z) = gradientDescentStep alpha nn dataset


data DeltaInfo = DeltaInfo
  { delta0        :: Double
  , deltaMin      :: Double
  , deltaMax      :: Double
  , deltaIncrease :: Double
  , deltaDecrease :: Double
  }
  deriving (Show, Eq, Ord)

standardDeltaInfo :: DeltaInfo
standardDeltaInfo = DeltaInfo
  { delta0        = 0.1
  , deltaMin      = 1e-6
  , deltaMax      = 50
  , deltaIncrease = 1.2
  , deltaDecrease = 0.5
  }

data RPropState nn = RPropState
  { rpropDeltas :: nn Double
  , rpropGrad   :: Grad nn Double
  }

deriving instance (Show (nn Double)) => Show (RPropState nn)
deriving instance (Eq (nn Double)) => Eq (RPropState nn)
deriving instance (Ord (nn Double)) => Ord (RPropState nn)

{-# INLINABLE rprop #-}
rprop
  :: forall k nn v. (ConstrainedFunctor k nn, Zippable k nn, NNVectorLike k nn Double, NeuralNetwork k nn v Double)
  => (ElemConstraints k Double , ElemConstraints k V3)
  => DeltaInfo
  -> nn Double
  -> Vector (v Double, v Double)
  -> IterateData nn (RPropState nn)
rprop (DeltaInfo {delta0, deltaMin, deltaMax, deltaIncrease, deltaDecrease}) nn dataset =
  IterateData f (RPropState initialDeltas initialGrad) value0 gradient0Size
  where
    (value0, gradient0) = NN.targetFunctionGrad dataset nn
    gradient0Size       = NN.size $ getGrad gradient0

    initialDeltas = cfmap (const delta0) nn
    initialGrad   = Grad $ cfmap (const 0) nn
    f :: RPropState nn
      -> nn Double
      -> ( Double
         , Grad nn Double
         , nn Double
         , RPropState nn
         )
    f (RPropState deltas prevGradient) nn =
      (value, gradient, nn', (RPropState deltas' (Grad prevGradient')))
      where
        (value, gradient) = NN.targetFunctionGrad dataset nn
        upd               = zipWith4 g (getGrad prevGradient) (getGrad gradient) nn deltas
        nn'               = cfmap (\(V3 x _ _) -> x) upd
        deltas'           = cfmap (\(V3 _ y _) -> y) upd
        prevGradient'     = cfmap (\(V3 _ _ z) -> z) upd

        g :: Double -> Double -> Double -> Double -> V3
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

data IterateData nn s = IterateData
  { iterateFunc          :: s -> nn Double -> (Double, Grad nn Double, nn Double, s)
  , iterateFuncInitState :: s
  , iterateInitValue     :: Double
  , iterateInitGradSize  :: Double
  }

iteratedUpdates
  :: forall k nn s. (NNVectorLike k nn Double, ElemConstraints k Double)
  => IterateData nn s
  -> ErrInfo
  -> nn Double
  -> (Double, nn Double)
iteratedUpdates (IterateData f fState value0 gradient0Size) (ErrInfo {epsilon, errorFuncAbsoluteValue}) nn =
  go 0 (value0 + sqrt epsilon) nn fState
  where
    go :: Int -> Double -> nn Double -> s -> (Double, nn Double)
    go n prevTargetFuncVal nn state =
      trace (printf "#%d, error = %g, errDelta = %g" n targetFuncVal errDelta) $
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
  :: forall nn s. IterateData nn s
  -> Int
  -> nn Double
  -> (Double, nn Double)
constantUpdates (IterateData f fState _value0 _gradient0Size) iterations nn =
  go 0 nn fState
  where
    go :: Int -> nn Double -> s -> (Double, nn Double)
    go n nn state
      | n == iterations = (targetFuncVal, nn)
      | otherwise =
        go (n + 1) nn' state'
      where
        (targetFuncVal, _, nn', state') = f state nn
