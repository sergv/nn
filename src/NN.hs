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

import Text.PrettyPrint.Leijen.Text (Pretty(..))

class NNVectorLike k (nn :: * -> *) a | nn -> k where
  -- z = x + b * y
  -- addScaled :: (Floating a) => nn a -> a -> nn a -> nn a
  -- size      :: (Floating a) => nn a ->  a
  fromWeightList :: (ElemConstraints k a, MonadError String m, Show a) => [[[a]]] -> m (nn a)
  toWeightList   :: (ElemConstraints k a) => nn a -> [[[a]]]
  addScaled      :: (ElemConstraints k a) => nn a -> a -> nn a -> nn a
  size           :: (ElemConstraints k a) => nn a -> a
  differenceSize :: (ElemConstraints k a) => nn a -> nn a -> a
  make           :: (ElemConstraints k a, Monad m) => Int -> [Int] -> Int -> m a -> m (nn a)

class NeuralNetwork k (nn :: * -> *) (v :: * -> *) a | nn -> k v where
  forwardPropagate   :: (ElemConstraints k a) => nn a -> v a -> v a
  targetFunctionGrad :: (ElemConstraints k a) => Vector (v a, v a) -> nn a -> (a, Grad nn a)

instance (Nonlinearity n, OutputType o n, Floating a, Show a) => NNVectorLike NoConstraints (S.NN n o) a where
  fromWeightList = S.fromWeightList
  toWeightList   = S.toWeightList
  addScaled      = S.addScaled
  size           = S.nnSize
  differenceSize = S.differenceSize
  make           = S.makeNN

instance (Nonlinearity n, OutputType o n, Floating a, Show a) => NeuralNetwork NoConstraints (S.NN n o) Vector a where
  forwardPropagate   = S.forwardPropagate
  targetFunctionGrad = S.backprop -- S.targetFunctionGrad


instance (Nonlinearity n, OutputType o n, MC.Matrix k w v, VC.Vect k v, Zippable k w, ConstrainedFunctor k w, Floating a, Show a) => NNVectorLike k (G.NN w v n o) a where
  fromWeightList = G.fromWeightList
  toWeightList   = G.toWeightList
  addScaled      = G.addScaled
  size           = G.nnSize
  differenceSize = G.differenceSize
  make           = G.makeNN

instance ( Nonlinearity n
         , OutputType o n
         , MC.Matrix k w v
         , VC.Vect k v
         , Floating a
         , ConstrainedFunctor k w
         , ConstrainedFunctor k v
         , Zippable k w

         , Show a
         ) => NeuralNetwork k (G.NN w v n o) v a where
  forwardPropagate   = G.forwardPropagate
  targetFunctionGrad = G.backprop 500
