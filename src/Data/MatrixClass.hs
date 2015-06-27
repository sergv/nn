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
import Util.Zippable

class (Vect v, Zippable w) => Matrix w v | w -> v where
  -- fromList   :: [a] -> v a
  -- toList     :: v a -> [a]
  map        :: (a -> b) -> w a -> w b
  rows       :: w a -> Int
  columns    :: w a -> Int
  replicateM :: (Monad m)
             => Int -- ^ rows
             -> Int -- ^ columns
             -> m a -- ^ action to create individual element
             -> m (w a)

  -- | Multiply column vector by row vector to obtain a matrix
  outerProduct :: (Num a) => v a -> v a -> w a

  vecMulRight :: (Num a) => w a -> v a -> v a
  vecMulLeft  :: (Num a) => v a -> w a -> v a
  -- matrixMult  :: w a -> w a -> w a

normL2Square :: (Matrix w v, Num a) => w a -> a
normL2Square matr = VC.sum $ vecMulRight matrSquares v
  where
    matrSquares = map (\x -> x * x) matr
    v = VC.replicate (columns matr) 1

