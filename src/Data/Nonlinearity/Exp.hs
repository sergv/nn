
----------------------------------------------------------------------------
-- |
-- Module      :  Data.Nonlinearity.Exp
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

module Data.Nonlinearity.Exp where

import Data.ConstrainedFunctor
import Data.Nonlinearity.Internal
import Data.SpecialisedFunction
import Util ()

data Exp
data instance FuncWithDeriv Exp = ExpWithDeriv

instance PrettyProxy Exp where
  prettyProxy _ = "Exp"

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction Exp (f a) (f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ = cfmap exp

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction (FuncWithDeriv Exp) (f a) (f a, f a)
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

instance Nonlinearity Exp where
  {-# INLINABLE nonlinearity #-}
  nonlinearity _ = exp
  nonlinearityDeriv _ = exp
