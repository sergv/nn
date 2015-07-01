----------------------------------------------------------------------------
-- |
-- Module      :  Data.MatrixClass
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Data.MatrixClass where

import Prelude hiding (map)

import Data.VectClass (Vect)
import qualified Data.VectClass as VC
import Util.ConstrainedFunctor

class (Vect k v) => Matrix k w v | w -> v k where
  fromList   :: (ElemConstraints k a, Show a) => [[a]] -> w a
  toList     :: (ElemConstraints k a) => w a -> [[a]]
  rows       :: (ElemConstraints k a) => w a -> Int
  columns    :: (ElemConstraints k a) => w a -> Int
  replicateM :: (Monad m, ElemConstraints k a)
             => Int -- ^ rows
             -> Int -- ^ columns
             -> m a -- ^ action to create individual element
             -> m (w a)

  -- | Multiply column vector by row vector to obtain a matrix
  outerProduct :: (Num a, ElemConstraints k a) => v a -> v a -> w a

  vecMulRight :: (Num a, ElemConstraints k a) => w a -> v a -> v a
  vecMulLeft  :: (Num a, ElemConstraints k a) => v a -> w a -> v a
  -- matrixMult  :: w a -> w a -> w a

normL2Square
  :: (Matrix k w v, Num a, ConstrainedFunctor k w, ElemConstraints k a)
  => w a -> a
normL2Square matr = VC.sum $ vecMulRight matrSquares v
  where
    matrSquares = cfmap (\x -> x * x) matr
    v = VC.replicate (columns matr) 1
