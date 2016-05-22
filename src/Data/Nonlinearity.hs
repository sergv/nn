----------------------------------------------------------------------------
-- |
-- Module      :  Data.Nonlinearity
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 23 September 2015
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Nonlinearity
  ( module Data.Nonlinearity.Exp
  , module Data.Nonlinearity.HyperbolicTangent
  , module Data.Nonlinearity.Linear
  , module Data.Nonlinearity.Sigmoid

  , module Data.Nonlinearity.Proxies

  , VectorisedNonlinearity(..)
  )
where

import Data.ConstrainedFunctor
import Data.SpecialisedFunction

import Data.Nonlinearity.Exp
import Data.Nonlinearity.HyperbolicTangent
import Data.Nonlinearity.Linear
import Data.Nonlinearity.Proxies
import Data.Nonlinearity.Sigmoid

-- | Nonlinearity function lifted to vectors.
class (ConstrainedFunctor v) => VectorisedNonlinearity n v where
  nonlinearity :: (IsProxyFor p n, ElemConstraints v a, Floating a) => p -> v a -> v a
  {-# INLINABLE nonlinearity #-}
  default nonlinearity
    :: (IsProxyFor p n, Floating a, SpecialisedFunction n (v a) (v a))
    => p
    -> v a
    -> v a
  nonlinearity = sfmap

instance (ConstrainedFunctor v) => VectorisedNonlinearity Linear v
instance (ConstrainedFunctor v) => VectorisedNonlinearity Exp v
instance (ConstrainedFunctor v) => VectorisedNonlinearity Sigmoid v
instance (ConstrainedFunctor v) => VectorisedNonlinearity HyperbolicTangent v
