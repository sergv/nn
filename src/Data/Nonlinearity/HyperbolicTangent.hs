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

module Data.Nonlinearity.HyperbolicTangent (HyperbolicTangent) where

import Data.Functor.Identity

import Data.ConstrainedFunctor
import Data.Grad
import Data.Nonlinearity.Proxies
import Data.Proxy
import Data.SpecialisedFunction
import Util

data HyperbolicTangent
data instance Deriv HyperbolicTangent
data instance FuncWithDeriv HyperbolicTangent

instance PrettyProxy HyperbolicTangent where
  prettyProxy _ = "HyperbolicTangent"

{-# INLINE tanhDeriv #-}
tanhDeriv :: (Floating a) => a -> a
tanhDeriv x = 1 -! nonlin *! nonlin
  where
    nonlin = tanh x

instance {-# OVERLAPPABLE #-} (Floating a) =>
  SpecialisedFunction HyperbolicTangent (Identity a) (Identity a) where
  {-# INLINABLE sfmap #-}
  sfmap _ = fmap tanh

instance {-# OVERLAPPABLE #-} (Floating a) =>
  SpecialisedFunction (Deriv HyperbolicTangent) (Identity a) (Grad Identity a) where
  {-# INLINABLE sfmap #-}
  sfmap _ = Grad . fmap tanhDeriv

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction HyperbolicTangent (f a) (f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ = cfmap tanh

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction (Deriv HyperbolicTangent) (f a) (Grad f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ = Grad . cfmap tanhDeriv

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction (FuncWithDeriv HyperbolicTangent) (f a) (f a, Grad f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ w = (f w, g w)
    where
      f = sfmap (Proxy :: Proxy HyperbolicTangent)
      g = sfmap (Proxy :: Proxy (Deriv HyperbolicTangent))
