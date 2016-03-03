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

module Data.Nonlinearity.Linear (Linear) where

import Data.ConstrainedFunctor
import Data.Nonlinearity.Proxies
import Data.Proxy
import Data.SpecialisedFunction

data Linear
data instance Deriv Linear
data instance FuncWithDeriv Linear

instance PrettyProxy Linear where
  prettyProxy _ = "Linear"

instance {-# OVERLAPPABLE #-} (Floating a) => SpecialisedFunction Linear a a where
  {-# INLINABLE sfmap #-}
  sfmap _ = id

instance {-# OVERLAPPABLE #-} (Floating a) => SpecialisedFunction (Deriv Linear) a a where
  {-# INLINABLE sfmap #-}
  sfmap _ = const 1

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction Linear (f a) (f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ = id

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a, Num a)
  => SpecialisedFunction (Deriv Linear) (f a) (f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ = cfmap (const 1)

instance {-# OVERLAPPABLE #-}
  (ConstrainedFunctor f, ElemConstraints f a, Floating a)
  => SpecialisedFunction (FuncWithDeriv Linear) (f a) (f a, f a)
  where
  {-# INLINABLE sfmap #-}
  sfmap _ w = (f w, g w)
    where
      f = sfmap (Proxy :: Proxy Linear)
      g = sfmap (Proxy :: Proxy (Deriv Linear))
