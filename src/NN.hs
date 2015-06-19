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

import qualified Data.VectClass as VC
import qualified NN.Generic as G
import qualified NN.Specific as S
import Nonlinearity
import Util

class ZippableNN (nn :: * -> *) where
  zipWith   :: (a -> b -> c) -> nn a -> nn b -> nn c
  zipWith3  :: (a -> b -> c -> d) -> nn a -> nn b -> nn c -> nn d
  zipWith4  :: (a -> b -> c -> d -> e) -> nn a -> nn b -> nn c -> nn d -> nn e

class NNVectorLike (nn :: * -> *) a where
  -- z = x + b * y
  -- addScaled :: (Floating a) => nn a -> a -> nn a -> nn a
  -- size      :: (Floating a) => nn a ->  a
  addScaled      :: nn a -> a -> nn a -> nn a
  size           :: nn a -> a
  differenceSize :: nn a -> nn a -> a
  make           :: (MonadRandom m) => Int -> [Int] -> Int -> m (nn Double)

class NeuralNetwork (nn :: * -> *) (v :: * -> *) a where
  forwardPropagate :: nn a -> v a -> v a
  targetFunctionGrad :: Vector (v a, v a) -> nn a -> (a, Grad nn a)

instance ZippableNN (S.NN n o) where
  zipWith = S.nnZipWith
  zipWith3 = S.nnZipWith3
  zipWith4 = S.nnZipWith4

instance (VC.Vect v) => ZippableNN (G.NN v n o) where
  zipWith = G.nnZipWith
  zipWith3 = G.nnZipWith3
  zipWith4 = G.nnZipWith4

instance (Nonlinearity n, OutputType o n, Floating a) => NNVectorLike (S.NN n o) a where
  addScaled      = S.addScaled
  size           = S.nnSize
  differenceSize = S.differenceSize
  make           = S.makeNN

instance (Nonlinearity n, OutputType o n, Floating a) => NeuralNetwork (S.NN n o) Vector a where
  forwardPropagate   = S.forwardPropagate
  targetFunctionGrad = S.targetFunctionGrad


instance (Nonlinearity n, OutputType o n, VC.Vect v, Floating a) => NNVectorLike (G.NN v n o) a where
  addScaled      = G.addScaled
  size           = G.nnSize
  differenceSize = G.differenceSize
  make           = G.makeNN

instance (Nonlinearity n, OutputType o n, VC.Vect v, Floating a) => NeuralNetwork (G.NN v n o) v a where
  forwardPropagate   = G.forwardPropagate
  targetFunctionGrad = G.targetFunctionGrad
