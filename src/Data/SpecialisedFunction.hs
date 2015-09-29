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

class IsProxyFor p a | p -> a where

-- instance IsProxyFor (p a) a
--
-- instance IsProxyFor (p a b) a
-- -- instance IsProxyFor (p a b) b
--
-- instance IsProxyFor (p a b c) a
-- -- instance IsProxyFor (p a b c) b
-- -- instance IsProxyFor (p a b c) c
--
-- instance IsProxyFor (p a b c d) a
-- -- instance IsProxyFor (p a b c d) b
-- -- instance IsProxyFor (p a b c d) c
-- -- instance IsProxyFor (p a b c d) d

instance IsProxyFor (Proxy a) a

-- | Functor that applies statically known function.
-- h - function that fmap would apply
-- p - proxy to determine the function h
class SpecialisedFunction (h :: k) a b | h a -> b where
  sfmap :: (IsProxyFor p h) => p -> a -> b
