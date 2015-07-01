----------------------------------------------------------------------------
-- |
-- Module      :  Nonlinearity
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Nonlinearity where

import Text.PrettyPrint.Leijen.Text (Doc)
import Util

-- Nonlinearity and Output types

data HyperbolicTangent
data Sigmoid

class Nonlinearity n where
  -- -- compute nonlinearity and it's derivative at the same point
  -- nonlinearity' :: (Floating a) => nn n o b -> a -> (# a, a #)
  nonlinearity :: (Floating a) => nn n o b -> a -> a
  -- compute nonlinearity and it's derivative at the same point
  nonlinearityDeriv
    :: (Floating a, Show a)
    => nn n o b -- ^ proxy neural network
    -> a        -- ^ input
    -> a        -- ^ derivative
  ppNonlinearity :: nn n o a -> Doc

instance Nonlinearity HyperbolicTangent where
  {-# INLINABLE nonlinearity      #-}
  {-# INLINABLE nonlinearityDeriv #-}
  nonlinearity _ x       = tanh x
  nonlinearityDeriv nn x = 1 -! nonlin *! nonlin
    where
      nonlin = nonlinearity nn x
  ppNonlinearity _      = "HyperbolicTangent"

instance Nonlinearity Sigmoid where
  {-# INLINABLE nonlinearity      #-}
  {-# INLINABLE nonlinearityDeriv #-}
  nonlinearity _ x = x' /! (1 +! x')
    where
      x' = exp x
  nonlinearityDeriv _ x = x' /! (1 +! x')^2
    where
      x' = exp x
  ppNonlinearity _ = "Sigmoid"

data Linear
data Nonlinear

class (Nonlinearity n) => OutputType o n where
  output      :: (Nonlinearity n, Floating a) => nn n o b -> a -> a
  outputDeriv :: (Nonlinearity n, Floating a, Show a) => nn n o b -> a -> a
  ppOutput    :: nn n o a -> Doc

instance (Nonlinearity n) => OutputType Linear n where
  {-# INLINABLE output      #-}
  {-# INLINABLE outputDeriv #-}
  output _ x      = x
  outputDeriv _ _ = 1
  ppOutput _      = "Linear"

instance (Nonlinearity n) => OutputType Nonlinear n where
  {-# INLINABLE output      #-}
  {-# INLINABLE outputDeriv #-}
  output      = nonlinearity
  outputDeriv = nonlinearityDeriv
  ppOutput _  = "Nonlinear"

