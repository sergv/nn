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
{-# LANGUAGE TypeFamilies           #-}

module Data.MatrixClass where

import Data.VectClass (Vect)
import qualified Data.VectClass as VC
import Data.ConstrainedFunctor
import Prelude (Int, String, Monad, Floating, Show(..), Num, (.), ($), (++))
import qualified Prelude as P

import Util

class (Vect v, ElemConstraints v ~ ElemConstraints w) => Matrix w v | w -> v where
  {-# INLINABLE sum                         #-}
  {-# INLINABLE addScaled                   #-}
  {-# INLINABLE matrixMultByTransposedLeft  #-}
  {-# INLINABLE matrixMultByTransposedRight #-}
  fromList   :: (ElemConstraints w a, Show a) => [[a]] -> w a
  toList     :: (ElemConstraints w a) => w a -> [[a]]
  rows       :: w a -> Int
  columns    :: w a -> Int
  replicateM :: (Monad m, ElemConstraints w a)
             => Int -- ^ rows
             -> Int -- ^ columns
             -> m a -- ^ action to create individual element
             -> m (w a)
  -- | Multiply column vector by row vector to obtain a matrix.
  outerProduct :: (Num a, ElemConstraints w a) => v a -> v a -> w a

  vecMulRight :: (Num a, ElemConstraints w a) => w a -> v a -> v a
  transpose :: (ElemConstraints w a) => w a -> w a
  matrixMult :: (ElemConstraints w a, Num a) => w a -> w a -> w a
  (|+|) :: (ElemConstraints w a, Num a) => w a -> w a -> w a
  addScaled  :: (ElemConstraints w a, Num a) => w a -> a -> w a -> w a
  default addScaled :: (ElemConstraints w a, Num a, ConstrainedFunctor w)
                    => w a -> a -> w a -> w a
  addScaled xs c ys = xs |+| cfmap (*! c) ys
  sumColumns :: (ElemConstraints w a, Num a) => w a -> v a
  sum :: (ElemConstraints w a, Num a) => w a -> a
  sum = VC.sum . sumColumns
  -- | Multiply transposed first matrix by second.
  matrixMultByTransposedLeft :: (ElemConstraints w a, Num a) => w a -> w a -> w a
  matrixMultByTransposedLeft x y = matrixMult (transpose x) y
  -- | Multiply first matrix by transposed second.
  matrixMultByTransposedRight :: (ElemConstraints w a, Num a) => w a -> w a -> w a
  matrixMultByTransposedRight x y = matrixMult x (transpose y)

  normL2Square :: (Num a, ElemConstraints w a) => w a -> a
  default normL2Square
    :: (Num a, ConstrainedFunctor w, ElemConstraints w a)
    => w a -> a
  normL2Square matr = sum $ cfmap (\x -> x *! x) matr
  -- normL2Square matr = VC.sum $ vecMulRight matrSquares v
  --   where
  --     matrSquares = cfmap (\x -> x * x) matr
  --     v = VC.replicate (columns matr) 1

showMatrixSize :: (Matrix w v) => w a -> String
showMatrixSize x = show (rows x) ++ "x" ++ show (columns x)
