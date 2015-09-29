----------------------------------------------------------------------------
-- |
-- Module      :  Data.Nonlinearity.HyperbolicTangent
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

module Data.Nonlinearity.HyperbolicTangent where

import Data.ConstrainedFunctor
import Data.Nonlinearity.Internal
import Data.SpecialisedFunction
import Util

data HyperbolicTangent
data instance FuncWithDeriv HyperbolicTangent = HyperbolicTangentWithDeriv

instance PrettyProxy HyperbolicTangent where
  prettyProxy _ = "HyperbolicTangent"

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction HyperbolicTangent (f a) (f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ = cfmap tanh

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction (FuncWithDeriv HyperbolicTangent) (f a) (f a, f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap p w = (cfmap f w, cfmap g w)
    where
      f = nonlinearity (stripFuncWithDerivInProxy p)
      g = nonlinearityDeriv (stripFuncWithDerivInProxy p)
  -- sfmap _ w = (cfmap f w, cfmap g w)
  --   where
  --     f = tanh
  --     g x = 1 - x' *! x'
  --       where
  --         x'  = f x

instance Nonlinearity HyperbolicTangent where
  {-# INLINABLE nonlinearity #-}
  nonlinearity      _ x = tanh x
  nonlinearityDeriv p x = 1 -! nonlin *! nonlin
    where
      nonlin = nonlinearity p x
