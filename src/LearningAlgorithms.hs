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
import Util.Zippable


import Debug.Trace

data ErrInfo = ErrInfo
  { epsilon                :: Double
  , errorFuncAbsoluteValue :: Double
  }
  deriving (Show, Eq, Ord)

gradientDescentStep
  :: (Functor nn, NNVectorLike nn Double, NeuralNetwork nn v Double)
  => Double
  -> nn Double
  -> Vector (v Double, v Double)
  -> (Double, Grad nn Double, nn Double)
gradientDescentStep alpha nn dataset =
  (value, gradient, NN.addScaled nn (- alpha) (getGrad gradient'))
  where
    (value, gradient) = NN.targetFunctionGrad dataset nn
    gradient'         = fmap (* alpha) gradient

gradientDescent
  :: forall nn v. (Functor nn, NNVectorLike nn Double, NeuralNetwork nn v Double)
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
  :: forall nn v. (Functor nn, Zippable nn, NNVectorLike nn Double, NeuralNetwork nn v Double)
  => DeltaInfo
  -> nn Double
  -> Vector (v Double, v Double)
  -> IterateData nn (RPropState nn)
rprop (DeltaInfo {delta0, deltaMin, deltaMax, deltaIncrease, deltaDecrease}) nn dataset =
  IterateData f (RPropState initialDeltas initialGrad) value0 gradient0Size
  where
    (value0, gradient0) = NN.targetFunctionGrad dataset nn
    gradient0Size       = NN.size $ getGrad gradient0

    initialDeltas = fmap (const delta0) nn
    initialGrad   = Grad $ fmap (const 0) nn
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

iteratedUpdates
  :: forall nn s. (NNVectorLike nn Double)
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
