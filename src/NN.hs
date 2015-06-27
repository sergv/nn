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

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NN where

import Data.Random.Source (MonadRandom)
import Data.Vector (Vector)

import qualified Data.MatrixClass as MC
import qualified Data.VectClass as VC
import qualified NN.Generic as G
import qualified NN.Specific as S
import Nonlinearity
import Unboxed.Functor (UnboxedFunctor, Unbox)
import Util

class NNVectorLike (nn :: * -> *) a where
  -- z = x + b * y
  -- addScaled :: (Floating a) => nn a -> a -> nn a -> nn a
  -- size      :: (Floating a) => nn a ->  a
  addScaled      :: nn a -> a -> nn a -> nn a
  size           :: nn a -> a
  differenceSize :: nn a -> nn a -> a
  make           :: (MonadRandom m) => Int -> [Int] -> Int -> m a -> m (nn a)

class NeuralNetwork (nn :: * -> *) (v :: * -> *) a where
  forwardPropagate :: nn a -> v a -> v a
  targetFunctionGrad :: Vector (v a, v a) -> nn a -> (a, Grad nn a)

instance (Nonlinearity n, OutputType o n, Floating a) => NNVectorLike (S.NN n o) a where
  addScaled      = S.addScaled
  size           = S.nnSize
  differenceSize = S.differenceSize
  make           = S.makeNN

instance (Nonlinearity n, OutputType o n, Floating a) => NeuralNetwork (S.NN n o) Vector a where
  forwardPropagate   = S.forwardPropagate
  targetFunctionGrad = S.backprop -- S.targetFunctionGrad


instance (Nonlinearity n, OutputType o n, MC.Matrix w v, VC.Vect v, Floating a) => NNVectorLike (G.NN w v n o) a where
  addScaled      = G.addScaled
  size           = G.nnSize
  differenceSize = G.differenceSize
  make           = G.makeNN

instance (Nonlinearity n, OutputType o n, MC.Matrix w v, VC.Vect v, Floating a, Unbox a, UnboxedFunctor w, UnboxedFunctor v) => NeuralNetwork (G.NN w v n o) v a where
  forwardPropagate   = G.forwardPropagate
  targetFunctionGrad = G.backprop -- G.targetFunctionGrad
