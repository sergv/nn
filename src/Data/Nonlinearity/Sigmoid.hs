----------------------------------------------------------------------------
-- |
-- Module      :  Data.Nonlinearity.Sigmoid
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Friday, 25 September 2015
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Nonlinearity.Sigmoid (Sigmoid, FuncWithDeriv(..)) where

import Data.ConstrainedFunctor
import Data.Nonlinearity.Internal
import Data.SpecialisedFunction
import Util

data Sigmoid
data instance FuncWithDeriv Sigmoid = SigmoidWithDeriv

instance PrettyProxy Sigmoid where
  prettyProxy _ = "Sigmoid"

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction Sigmoid (f a) (f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ = cfmap sigmoid

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction (FuncWithDeriv Sigmoid) (f a) (f a, f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap p w = (cfmap f w, cfmap g w)
    where
      f = nonlinearity (stripFuncWithDerivInProxy p)
      g = nonlinearityDeriv (stripFuncWithDerivInProxy p)
      -- g x = x' /! (x'' *! x'')
      --   where
      --     x'  = exp x
      --     x'' = (1 +! x')

instance Nonlinearity Sigmoid where
  {-# INLINABLE nonlinearity #-}
  nonlinearity _ = sigmoid
  nonlinearityDeriv _ x = x' /! (1 +! x')^2
    where
      x' = exp x

{-# INLINE sigmoid #-}
sigmoid :: (Floating a) => a -> a
sigmoid x = x' /! (1 +! x')
    where
      x' = exp x
