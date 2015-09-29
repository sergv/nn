----------------------------------------------------------------------------
-- |
-- Module      :  Data.Nonlinearity.Linear
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

module Data.Nonlinearity.Linear where

import Data.ConstrainedFunctor
import Data.Nonlinearity.Internal
import Data.SpecialisedFunction

data Linear
-- data instance Deriv Linear = LinearDeriv
data instance FuncWithDeriv Linear = LinearWithDeriv

instance PrettyProxy Linear where
  prettyProxy _ = "Linear"

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction Linear (f a) (f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ = id

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction (FuncWithDeriv Linear) (f a) (f a, f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ w = (w, cfmap (const 1) w)

instance Nonlinearity Linear where
  {-# INLINABLE nonlinearity #-}
  nonlinearity _ = id
  nonlinearityDeriv _ = const 1
