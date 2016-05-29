----------------------------------------------------------------------------
-- |
-- Module      :  NN
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module NN
  ( NeuralNetwork(..)
  , NNVectorLike(..)
  , module NN.Description
  ) where

import Data.Vector (Vector)

import Data.ConstrainedFunctor
import Data.Grad
import qualified Data.MatrixClass as MC
import Data.Nonlinearity
import Data.SpecialisedFunction
import qualified Data.VectClass as VC
import Data.Zippable
import NN.Description
import qualified NN.Generic as G
import qualified NN.Specific as S

class (ConstrainedFunctor nn) => NNVectorLike (nn :: * -> *) a where
  -- z = x + b * y
  -- addScaled :: (Floating a) => nn a -> a -> nn a -> nn a
  -- size      :: (Floating a) => nn a ->  a
  addScaled       :: (ElemConstraints nn a) => nn a -> a -> nn a -> nn a
  size            :: (ElemConstraints nn a) => nn a -> a
  differenceSize  :: (ElemConstraints nn a) => nn a -> nn a -> a

class (ConstrainedFunctor nn) => NeuralNetwork (nn :: * -> *) (v :: * -> *) a | nn -> v where
  -- | Evaluate neural network on some input, yielding some output
  forwardPropagate :: (ElemConstraints nn a) => nn a -> v a -> v a
  -- | Obtain gradient of target function evaluated on some input
  targetFunctionGrad
    :: (ElemConstraints nn a)
    => Vector (v a, v a) -- ^ Input data to evaluate on
    -> nn a              -- ^ Neural network to evaluate with
    -> (a, Grad nn a)

instance (Floating a, Show a) => NNVectorLike (S.NN n o) a where
  addScaled       = S.addScaled
  size            = S.nnSize
  differenceSize  = S.differenceSize

instance
  ( VectorisedNonlinearity n Vector
  , VectorisedNonlinearity o Vector
  , Floating a
  , Show a
  ) => NeuralNetwork (S.NN n o) Vector a where
  forwardPropagate   = S.forwardPropagate
  targetFunctionGrad = S.targetFunctionGradAD -- S.backprop

instance
  ( MC.Matrix w v
  , VC.Vect v
  , Zippable w
  , ConstrainedFunctor w
  , Floating a
  , Show a
  ) => NNVectorLike (G.NN w v n o) a where
  addScaled       = G.addScaled
  size            = G.nnSize
  differenceSize  = G.differenceSize

instance
  ( MC.Matrix w v
  , VC.Vect v
  , ConstrainedFunctor w
  , ConstrainedFunctor v
  , Zippable w
  , Floating a
  , Show a
  , VectorisedNonlinearity n v
  , VectorisedNonlinearity o v
  , SpecialisedFunction (FuncWithDeriv n) (w a) (w a, Grad w a)
  , SpecialisedFunction (FuncWithDeriv o) (w a) (w a, Grad w a)
  ) => NeuralNetwork (G.NN w v n o) v a where
  forwardPropagate   = G.forwardPropagate
  targetFunctionGrad = G.backprop batchSize
    where
      batchSize = 500
