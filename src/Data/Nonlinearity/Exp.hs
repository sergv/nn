
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

module Data.Nonlinearity.Exp (Exp) where

import Data.ConstrainedFunctor
import Data.Nonlinearity.Proxies
import Data.Proxy
import Data.SpecialisedFunction
import Util ()

data Exp
data instance Deriv Exp
data instance FuncWithDeriv Exp

instance PrettyProxy Exp where
  prettyProxy _ = "Exp"

instance {-# OVERLAPPABLE #-} (Floating a) => SpecialisedFunction Exp a a where
  {-# INLINABLE sfmap #-}
  sfmap _ = exp

instance {-# OVERLAPPABLE #-} (Floating a) => SpecialisedFunction (Deriv Exp) a a where
  {-# INLINABLE sfmap #-}
  sfmap _ = exp

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction Exp (f a) (f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ = cfmap exp

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction (Deriv Exp) (f a) (f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ = cfmap exp

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction (FuncWithDeriv Exp) (f a) (f a, f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ w = (f w, g w)
    where
      f = sfmap (Proxy :: Proxy Exp)
      g = sfmap (Proxy :: Proxy (Deriv Exp))
