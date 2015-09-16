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

module NN where

import Control.Monad.Except
import Data.Vector (Vector)

import Data.ConstrainedFunctor
import qualified Data.MatrixClass as MC
import qualified Data.VectClass as VC
import Data.Zippable
import qualified NN.Generic as G
import qualified NN.Specific as S
import Nonlinearity
import Util

class (ConstrainedFunctor nn) => NNVectorLike (nn :: * -> *) a where
  -- z = x + b * y
  -- addScaled :: (Floating a) => nn a -> a -> nn a -> nn a
  -- size      :: (Floating a) => nn a ->  a
  fromWeightList :: (ElemConstraints nn a, MonadError String m, Show a) => [[[a]]] -> m (nn a)
  toWeightList   :: (ElemConstraints nn a) => nn a -> [[[a]]]
  addScaled      :: (ElemConstraints nn a) => nn a -> a -> nn a -> nn a
  size           :: (ElemConstraints nn a) => nn a -> a
  differenceSize :: (ElemConstraints nn a) => nn a -> nn a -> a
  make           :: (ElemConstraints nn a, Monad m) => Int -> [Int] -> Int -> m a -> m (nn a)

class (ConstrainedFunctor nn) => NeuralNetwork (nn :: * -> *) (v :: * -> *) a | nn -> v where
  forwardPropagate   :: (ElemConstraints nn a) => nn a -> v a -> v a
  targetFunctionGrad :: (ElemConstraints nn a) => Vector (v a, v a) -> nn a -> (a, Grad nn a)

instance (Nonlinearity n, OutputType o n, Floating a, Show a) => NNVectorLike (S.NN n o) a where
  fromWeightList = S.fromWeightList
  toWeightList   = S.toWeightList
  addScaled      = S.addScaled
  size           = S.nnSize
  differenceSize = S.differenceSize
  make           = S.makeNN

instance (Nonlinearity n, OutputType o n, Floating a, Show a) => NeuralNetwork (S.NN n o) Vector a where
  forwardPropagate   = S.forwardPropagate
  targetFunctionGrad = S.targetFunctionGrad -- S.backprop -- S.targetFunctionGrad


instance ( Nonlinearity n
         , OutputType o n
         , MC.Matrix w v
         , VC.Vect v
         , Zippable w
         , ConstrainedFunctor w
         , Floating a
         , Show a
         )
         => NNVectorLike (G.NN w v n o) a where
  fromWeightList = G.fromWeightList
  toWeightList   = G.toWeightList
  addScaled      = G.addScaled
  size           = G.nnSize
  differenceSize = G.differenceSize
  make           = G.makeNN

instance ( Nonlinearity n
         , OutputType o n
         , MC.Matrix w v
         , VC.Vect v
         , ConstrainedFunctor w
         , ConstrainedFunctor v
         , Zippable w
         , Floating a
         , Show a
         )
         => NeuralNetwork (G.NN w v n o) v a where
  forwardPropagate   = G.forwardPropagate
  targetFunctionGrad = G.backprop 500
