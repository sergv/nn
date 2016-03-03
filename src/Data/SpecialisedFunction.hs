----------------------------------------------------------------------------
-- |
-- Module      :  Data.SpecialisedFunction
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 16 September 2015
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}

module Data.SpecialisedFunction where

import Data.Proxy

-- | Class to generalize over proxies of 'a'.
class IsProxyFor p a | p -> a where

instance IsProxyFor (Proxy a) a

-- | Functor that applies statically known function. Allows to specialize it
-- to particular types in order to override some instances.
-- h - function that fmap would apply
-- p - proxy to determine the function h
class SpecialisedFunction (h :: k) a b | h a -> b where
  sfmap :: (IsProxyFor p h) => p -> a -> b
