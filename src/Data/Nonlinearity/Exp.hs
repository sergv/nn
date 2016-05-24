
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

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Nonlinearity.Exp (Exp) where

import Data.Functor.Identity

import Data.ConstrainedFunctor
import Data.Grad
import Data.Nonlinearity.Proxies
import Data.Proxy
import Data.SpecialisedFunction
import Util ()

data Exp
data instance Deriv Exp
data instance FuncWithDeriv Exp

instance PrettyProxy Exp where
  prettyProxy _ = "Exp"

instance {-# OVERLAPPABLE #-} (Floating a) =>
  SpecialisedFunction Exp (Identity a) (Identity a) where
  {-# INLINABLE sfmap #-}
  sfmap _ = fmap exp

instance {-# OVERLAPPABLE #-} (Floating a) =>
  SpecialisedFunction (Deriv Exp) (Identity a) (Grad Identity a) where
  {-# INLINABLE sfmap #-}
  sfmap _ = Grad . fmap exp

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction Exp (f a) (f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ = cfmap exp

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction (Deriv Exp) (f a) (Grad f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ = Grad . cfmap exp

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction (FuncWithDeriv Exp) (f a) (f a, Grad f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ w = (f w, g w)
    where
      f = sfmap (Proxy :: Proxy Exp)
      g = sfmap (Proxy :: Proxy (Deriv Exp))
