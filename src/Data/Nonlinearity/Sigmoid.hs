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

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Nonlinearity.Sigmoid (Sigmoid) where

import Data.Functor.Identity

import Data.ConstrainedFunctor
import Data.Grad
import Data.Nonlinearity.Proxies
import Data.Proxy
import Data.SpecialisedFunction
import Util

data Sigmoid
data instance Deriv Sigmoid
data instance FuncWithDeriv Sigmoid

instance PrettyProxy Sigmoid where
  prettyProxy _ = "Sigmoid"

{-# INLINE sigmoid #-}
sigmoid :: (Floating a) => a -> a
sigmoid x = x' /! (1 +! x')
    where
      x' = exp x

{-# INLINE sigmoidDeriv #-}
sigmoidDeriv :: (Floating a) => a -> a
sigmoidDeriv x = x' /! (1 +! x')^2
    where
      x' = exp x

instance {-# OVERLAPPABLE #-} (Floating a) =>
  SpecialisedFunction Sigmoid (Identity a) (Identity a) where
  {-# INLINABLE sfmap #-}
  sfmap _ = fmap sigmoid

instance {-# OVERLAPPABLE #-} (Floating a) =>
  SpecialisedFunction (Deriv Sigmoid) (Identity a) (Grad Identity a) where
  {-# INLINABLE sfmap #-}
  sfmap _ = Grad . fmap sigmoidDeriv

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction Sigmoid (f a) (f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ = cfmap sigmoid

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction (Deriv Sigmoid) (f a) (Grad f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ = Grad . cfmap sigmoidDeriv

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction (FuncWithDeriv Sigmoid) (f a) (f a, Grad f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ w = (f w, g w)
    where
      f = sfmap (Proxy :: Proxy Sigmoid)
      g = sfmap (Proxy :: Proxy (Deriv Sigmoid))
