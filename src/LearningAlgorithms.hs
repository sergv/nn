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

import Data.VectClass (Vect)
import NN.Specific
import qualified NN.Generic as NG
import Util

import Debug.Trace

data ErrInfo = ErrInfo
  { epsilon                :: Double
  , errorFuncAbsoluteValue :: Double
  }
  deriving (Show, Eq, Ord)

gradientDescentStep
  :: (Nonlinearity n, OutputType o n)
  => Double
  -> NN n o Double
  -> Vector (Vector Double, Vector Double)
  -> (Double, Grad (NN n o) Double, NN n o Double)
gradientDescentStep alpha nn dataset =
  (value, gradient, addScaled nn (- alpha) (getGrad gradient'))
  where
    (value, gradient) = targetFunctionGrad dataset nn
    gradient'         = fmap (* alpha) gradient

gradientDescent
  :: forall n o. (Nonlinearity n, OutputType o n)
  => Double
  -> NN n o Double
  -> Vector (Vector Double, Vector Double)
  -> IterateData (NN n o) ()
gradientDescent alpha nn dataset =
  IterateData f () value0 gradient0Size
  where
    (value0, gradient0) = targetFunctionGrad dataset nn
    gradient0Size = nnSize $ getGrad gradient0

    f :: () -> NN n o Double -> (Double, Grad (NN n o) Double, NN n o Double, ())
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
standardDeltaInfo = DeltaInfo 0.1 1e-6 50 1.2 0.5

-- rpropG :: forall nn n o . DeltaInfo
--        -> nn n o Double
--        -> Vector (Vector Double, Vector Double)
--        -> IterateData nn n o (nn n o Double, Grad (nn n o) Double)
-- rpropG (DeltaInfo {delta0, deltaMin, deltaMax, deltaIncrease, deltaDecrease}) nn dataset =
--   IterateData f (initialDeltas, initialGrad) value0 gradient0Size
--   where
--     (value0, gradient0) = targetFunctionGrad dataset nn
--     gradient0Size       = nnSize $ getGrad gradient0
--
--     initialDeltas = fmap (const delta0) nn
--     initialGrad   = Grad $ fmap (const 0) nn
--     f :: (nn n o Double, Grad (nn n o) Double)
--       -> nn n o Double
--       -> ( Double
--          , Grad (nn n o) Double
--          , NN n o Double
--          , (nn n o Double, Grad (nn n o) Double)
--          )
--     f (deltas, prevGradient) nn = (value, gradient, nn', (deltas', Grad prevGradient'))
--       where
--         (value, gradient) = targetFunctionGrad dataset nn
--         upd               = nnZipWith4 g (getGrad prevGradient) (getGrad gradient) nn deltas
--         nn'               = fmap (\(x, _, _) -> x) upd
--         deltas'           = fmap (\(_, y, _) -> y) upd
--         prevGradient'     = fmap (\(_, _, z) -> z) upd
--
--         g :: Double -> Double -> Double -> Double -> (Double, Double, Double)
--         g dwPrev dw w delta
--           | dwPrev * dw > 0 = let delta'  = min (delta * deltaIncrease) deltaMax
--                                   w'      = w - signum dw * delta'
--                                   dwPrev' = dw
--                               in w' `seq` delta' `seq` dwPrev' `seq`
--                                  (w', delta', dwPrev')
--           | dwPrev * dw < 0 = let delta'  = max (delta * deltaDecrease) deltaMin
--                                   w'      = w
--                                   dwPrev' = 0
--                               in w' `seq` delta' `seq` dwPrev' `seq`
--                                  (w', delta', dwPrev')
--           | otherwise       = let delta'  = delta
--                                   w'      = w - signum dw * delta'
--                                   dwPrev' = dw
--                               in w' `seq` delta' `seq` dwPrev' `seq`
--                                  (w', delta', dwPrev')

data RPropState nn = RPropState
  { rpropDeltas :: nn Double
  , rpropGrad   :: Grad nn Double
  }

deriving instance (Show (nn Double)) => Show (RPropState nn)
deriving instance (Eq (nn Double)) => Eq (RPropState nn)
deriving instance (Ord (nn Double)) => Ord (RPropState nn)

rprop
  :: forall n o. (Nonlinearity n, OutputType o n)
  => DeltaInfo
  -> NN n o Double
  -> Vector (Vector Double, Vector Double)
  -> IterateData (NN n o) (RPropState (NN n o))
rprop (DeltaInfo {delta0, deltaMin, deltaMax, deltaIncrease, deltaDecrease}) nn dataset =
  IterateData f (RPropState initialDeltas initialGrad) value0 gradient0Size
  where
    (value0, gradient0) = targetFunctionGrad dataset nn
    gradient0Size       = nnSize $ getGrad gradient0

    initialDeltas = fmap (const delta0) nn
    initialGrad   = Grad $ fmap (const 0) nn
    f :: RPropState (NN n o)
      -> NN n o Double
      -> ( Double
         , Grad (NN n o) Double
         , NN n o Double
         , RPropState (NN n o)
         )
    f (RPropState deltas prevGradient) nn =
      (value, gradient, nn', (RPropState deltas' (Grad prevGradient')))
      where
        (value, gradient) = targetFunctionGrad dataset nn
        upd               = nnZipWith4 g (getGrad prevGradient) (getGrad gradient) nn deltas
        nn'               = fmap (\(x, _, _) -> x) upd
        deltas'           = fmap (\(_, y, _) -> y) upd
        prevGradient'     = fmap (\(_, _, z) -> z) upd

        g :: Double -> Double -> Double -> Double -> (Double, Double, Double)
        g dwPrev dw w delta
          | dwPrev * dw > 0 = let delta'  = min (delta * deltaIncrease) deltaMax
                                  w'      = w - signum dw * delta'
                                  dwPrev' = dw
                              in w' `seq` delta' `seq` dwPrev' `seq`
                                 (w', delta', dwPrev')
          | dwPrev * dw < 0 = let delta'  = max (delta * deltaDecrease) deltaMin
                                  w'      = w
                                  dwPrev' = 0
                              in w' `seq` delta' `seq` dwPrev' `seq`
                                 (w', delta', dwPrev')
          | otherwise       = let delta'  = delta
                                  w'      = w - signum dw * delta'
                                  dwPrev' = dw
                              in w' `seq` delta' `seq` dwPrev' `seq`
                                 (w', delta', dwPrev')

data IterateData nn s = IterateData
  { iterateFunc          :: s -> nn Double -> (Double, Grad nn Double, nn Double, s)
  , iterateFuncInitState :: s
  , iterateInitValue     :: Double
  , iterateInitGradSize  :: Double
  }

iteratedUpdates :: forall n o s.
                   IterateData (NN n o) s
                -> ErrInfo
                -> NN n o Double
                -> (Double, NN n o Double)
iteratedUpdates (IterateData f fState value0 gradient0Size) (ErrInfo {epsilon, errorFuncAbsoluteValue}) nn =
  go 0 (value0 + sqrt epsilon) nn fState
  where
    go :: Int -> Double -> NN n o Double -> s -> (Double, NN n o Double)
    go n prevTargetFuncVal nn state =
      trace (printf "#%d, error = %g, errDelta = %g" n targetFuncVal errDelta) $
      if errDelta > epsilon &&
         gradientSize > epsilon * gradient0Size &&
         abs targetFuncVal > errorFuncAbsoluteValue
      then go (n + 1) targetFuncVal nn' state'
      else (targetFuncVal, nn)
      where
        (targetFuncVal, gradient, nn', state') = f state nn
        gradientSize                           = nnSize $ getGrad gradient
        errDelta                               = abs (targetFuncVal - prevTargetFuncVal)

constantUpdates :: forall nn n o s.
                   IterateData (nn n o) s
                -> Int
                -> nn n o Double
                -> (Double, nn n o Double)
constantUpdates (IterateData f fState _value0 _gradient0Size) iterations nn =
  go 0 nn fState
  where
    go :: Int -> nn n o Double -> s -> (Double, nn n o Double)
    go n nn state
      | n == iterations = (targetFuncVal, nn)
      | otherwise =
        go (n + 1) nn' state'
      where
        (targetFuncVal, _, nn', state') = f state nn


rprop' :: forall v n o. (Vect v, Nonlinearity n, OutputType o n)
       => DeltaInfo
       -> NG.NN v n o Double
       -> Vector (v Double, v Double)
       -> IterateData (NG.NN v n o) (NG.NN v n o Double, Grad (NG.NN v n o) Double)
rprop' (DeltaInfo {delta0, deltaMin, deltaMax, deltaIncrease, deltaDecrease}) nn dataset =
  IterateData f (initialDeltas, initialGrad) value0 gradient0Size
  where
    (value0, gradient0) = NG.targetFunctionGrad dataset nn
    gradient0Size       = NG.nnSize $ getGrad gradient0

    initialDeltas = fmap (const delta0) nn
    initialGrad   = Grad $ fmap (const 0) nn
    f :: (NG.NN v n o Double, Grad (NG.NN v n o) Double) ->
         NG.NN v n o Double                         ->
         (Double, Grad (NG.NN v n o) Double, NG.NN v n o Double, (NG.NN v n o Double, Grad (NG.NN v n o) Double))
    f (deltas, prevGradient) nn = (value, gradient, nn', (deltas', Grad prevGradient'))
      where
        (value, gradient) = NG.targetFunctionGrad dataset nn
        upd               = NG.nnZipWith4 g (getGrad prevGradient) (getGrad gradient) nn deltas
        nn'               = fmap (\(x, _, _) -> x) upd
        deltas'           = fmap (\(_, y, _) -> y) upd
        prevGradient'     = fmap (\(_, _, z) -> z) upd

        g :: Double -> Double -> Double -> Double -> (Double, Double, Double)
        g dwPrev dw w delta
          | dwPrev * dw > 0 = let delta'  = min (delta * deltaIncrease) deltaMax
                                  w'      = w - signum dw * delta'
                                  dwPrev' = dw
                              in w' `seq` delta' `seq` dwPrev' `seq`
                                 (w', delta', dwPrev')
          | dwPrev * dw < 0 = let delta'  = max (delta * deltaDecrease) deltaMin
                                  w'      = w
                                  dwPrev' = 0
                              in w' `seq` delta' `seq` dwPrev' `seq`
                                 (w', delta', dwPrev')
          | otherwise       = let delta'  = delta
                                  w'      = w - signum dw * delta'
                                  dwPrev' = dw
                              in w' `seq` delta' `seq` dwPrev' `seq`
                                 (w', delta', dwPrev')

iteratedUpdates' :: forall v n o s. (Vect v)
                 => IterateData (NG.NN v n o) s
                 -> ErrInfo
                 -> NG.NN v n o Double
                 -> (Double, NG.NN v n o Double)
iteratedUpdates' (IterateData f fState value0 gradient0Size) (ErrInfo {epsilon, errorFuncAbsoluteValue}) nn =
  go 0 (value0 + sqrt epsilon) nn fState
  where
    go :: Int -> Double -> NG.NN v n o Double -> s -> (Double, NG.NN v n o Double)
    go n prevTargetFuncVal nn state =
      trace (printf "#%d, error = %g, errDelta = %g" n targetFuncVal errDelta) $
      if errDelta > epsilon &&
         gradientSize > epsilon * gradient0Size &&
         abs targetFuncVal > errorFuncAbsoluteValue
      then go (n + 1) targetFuncVal nn' state'
      else (targetFuncVal, nn)
      where
        (targetFuncVal, gradient, nn', state') = f state nn
        gradientSize                           = NG.nnSize $ getGrad gradient
        errDelta                               = abs (targetFuncVal - prevTargetFuncVal)

