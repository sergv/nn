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

{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Data.MatrixClass where

import Data.VectClass (Vect)
import qualified Data.VectClass as VC
import Data.ConstrainedFunctor
import Prelude (Int, String, Monad, Floating, Show(..), Num, (.), ($), (++))
import qualified Prelude as P

import Util

class (Vect k v) => Matrix k w v | w -> v k where
  {-# INLINABLE sum                         #-}
  {-# INLINABLE addScaled                   #-}
  {-# INLINABLE matrixMultByTransposedLeft  #-}
  {-# INLINABLE matrixMultByTransposedRight #-}
  fromList   :: (ElemConstraints k a, Show a) => [[a]] -> w a
  toList     :: (ElemConstraints k a) => w a -> [[a]]
  rows       :: w a -> Int
  columns    :: w a -> Int
  replicateM :: (Monad m, ElemConstraints k a)
             => Int -- ^ rows
             -> Int -- ^ columns
             -> m a -- ^ action to create individual element
             -> m (w a)
  -- | Multiply column vector by row vector to obtain a matrix.
  outerProduct :: (Num a, ElemConstraints k a) => v a -> v a -> w a

  vecMulRight :: (Num a, ElemConstraints k a) => w a -> v a -> v a
  transpose :: (ElemConstraints k a) => w a -> w a
  matrixMult :: (ElemConstraints k a, Num a) => w a -> w a -> w a
  (|+|) :: (ElemConstraints k a, Num a) => w a -> w a -> w a
  addScaled  :: (ElemConstraints k a, Num a) => w a -> a -> w a -> w a
  default addScaled :: (ElemConstraints k a, Num a, ConstrainedFunctor k w)
                    => w a -> a -> w a -> w a
  addScaled xs c ys = xs |+| cfmap (*! c) ys
  sumColumns :: (ElemConstraints k a, Num a) => w a -> v a
  sum :: (ElemConstraints k a, Num a) => w a -> a
  sum = VC.sum . sumColumns
  -- | Multiply transposed first matrix by second.
  matrixMultByTransposedLeft :: (ElemConstraints k a, Num a) => w a -> w a -> w a
  matrixMultByTransposedLeft x y = matrixMult (transpose x) y
  -- | Multiply first matrix by transposed second.
  matrixMultByTransposedRight :: (ElemConstraints k a, Num a) => w a -> w a -> w a
  matrixMultByTransposedRight x y = matrixMult x (transpose y)

  normL2Square :: (Matrix k w v, Num a, ElemConstraints k a) => w a -> a
  default normL2Square
    :: (Matrix k w v, Num a, ConstrainedFunctor k w, ElemConstraints k a)
    => w a -> a
  normL2Square matr = sum $ cfmap (\x -> x *! x) matr
  -- normL2Square matr = VC.sum $ vecMulRight matrSquares v
  --   where
  --     matrSquares = cfmap (\x -> x * x) matr
  --     v = VC.replicate (columns matr) 1
  exp :: (ElemConstraints k a, Floating a) => w a -> w a
  default exp
    :: (ElemConstraints k a, Floating a, ConstrainedFunctor k w)
    => w a -> w a
  exp = cfmap P.exp

showMatrixSize :: (Matrix k w v) => w a -> String
showMatrixSize x = show (rows x) ++ "x" ++ show (columns x)
