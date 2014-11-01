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

{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LearningAlgorithms where

import Control.Applicative
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Printf

import NN
import Util

import Debug.Trace

data ErrInfo = ErrInfo
             { epsilon                :: Double
             , errorFuncAbsoluteValue :: Double
             }
             deriving (Show, Eq, Ord)

gradientDescentStep :: Double                           ->
                       NN n o Double                    ->
                       [(Vector Double, Vector Double)] ->
                       (Double, Grad (NN n o) Double, NN n o Double)
gradientDescentStep alpha nn dataset =
  (value, gradient, addScaled nn (- alpha) (getGrad gradient'))
  where
    (value, gradient) = targetFunctionGrad dataset nn
    gradient'         = fmap (* alpha) gradient

gradientDescent :: ErrInfo                          ->
                   Double                           ->
                   NN n o Double                    ->
                   [(Vector Double, Vector Double)] ->
                   (Double, NN n o Double)
gradientDescent errInfo alpha nn dataset =
  iteratedUpdates f
                  errInfo
                  value0
                  gradient0Size
                  nn
                  (error "unused initial state")
  where
    (value0, gradient0) = targetFunctionGrad dataset nn
    gradient0Size = nnSize $ getGrad gradient0

    f :: a -> NN n o Double -> (Double, Grad (NN n o) Double, NN n o Double, a)
    f _ nn = (x, y, z, error "unused state")
      where
        (x, y, z) = gradientDescentStep alpha nn dataset


data DeltaInfo = DeltaInfo
               { delta0   :: Double
               , deltaMin :: Double
               , deltaMax :: Double
               , deltaIncrease :: Double
               , deltaDecrease :: Double
               }
               deriving (Show, Eq, Ord)

standardDeltaInfo :: DeltaInfo
standardDeltaInfo = DeltaInfo 0.1 1e-6 50 1.2 0.5

rprop :: ErrInfo                          ->
         DeltaInfo                        ->
         NN n o Double                    ->
         [(Vector Double, Vector Double)] ->
         (Double, NN n o Double)
rprop errInfo (DeltaInfo {delta0, deltaMin, deltaMax, deltaIncrease, deltaDecrease}) nn dataset =
  iteratedUpdates f
                  errInfo
                  value0
                  gradient0Size
                  nn
                  (initialDeltas, initialGrad)
  where
    (value0, gradient0) = targetFunctionGrad dataset nn
    gradient0Size       = nnSize $ getGrad gradient0

    initialDeltas = fmap (const delta0) nn
    initialGrad   = Grad $ fmap (const 0) nn
    f :: (NN n o Double, Grad (NN n o) Double) ->
         NN n o Double                         ->
         (Double, Grad (NN n o) Double, NN n o Double, (NN n o Double, Grad (NN n o) Double))
    f (deltas, prevGradient) nn = (value, gradient, nn', (deltas', Grad prevGradient'))
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


iteratedUpdates :: forall a n o.
                   (a -> NN n o Double -> (Double, Grad (NN n o) Double, NN n o Double, a)) ->
                   ErrInfo       ->
                   Double        ->
                   Double        ->
                   NN n o Double ->
                   a             ->
                   (Double, NN n o Double)
iteratedUpdates f (ErrInfo {epsilon, errorFuncAbsoluteValue}) value0 gradient0Size nn fState =
  go 0 (value0 + sqrt epsilon) nn fState
  where
    go :: Int -> Double -> NN n o Double -> a -> (Double, NN n o Double)
    go n prevTargetFuncVal nn state =
      -- trace (printf "#%d, error = %g, errDelta = %g" n targetFuncVal errDelta) $
      if errDelta > epsilon &&
         gradientSize > epsilon * gradient0Size &&
         abs targetFuncVal > errorFuncAbsoluteValue
      then go (n + 1) targetFuncVal nn' state'
      else (targetFuncVal, nn)
      where
        (targetFuncVal, gradient, nn', state') = f state nn
        gradientSize                           = nnSize $ getGrad gradient
        errDelta                               = abs (targetFuncVal - prevTargetFuncVal)

