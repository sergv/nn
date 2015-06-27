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
    :: (Floating a)
    => nn n o b -- ^ proxy neural network
    -> a        -- ^ input
    -> (a, a)   -- ^ nonlinearity and its derivative
  ppNonlinearity :: nn n o a -> Doc

instance Nonlinearity HyperbolicTangent where
  nonlinearity _ x       = tanh x
  nonlinearityDeriv nn x = nonlin `seq` (nonlin, 1 -! nonlin * nonlin)
    where
      nonlin = nonlinearity nn x
  ppNonlinearity _      = "HyperbolicTangent"

instance Nonlinearity Sigmoid where
  nonlinearity _ x = x' /! (1 +! x')
    where
      x' = exp x
  nonlinearityDeriv nn x = (nonlin, nonlin /! (1 +! exp x))
    where
      nonlin = nonlinearity nn x
  ppNonlinearity _ = "Sigmoid"

data Linear
data Nonlinear

class (Nonlinearity n) => OutputType o n where
  output      :: (Nonlinearity n, Floating a) => nn n o b -> a -> a
  outputDeriv :: (Nonlinearity n, Floating a) => nn n o b -> a -> (a, a)
  ppOutput    :: nn n o a -> Doc

instance (Nonlinearity n) => OutputType Linear n where
  output _ x      = x
  outputDeriv _ x = (x, 1)
  ppOutput _      = "Linear"

instance (Nonlinearity n) => OutputType Nonlinear n where
  output      = nonlinearity
  outputDeriv = nonlinearityDeriv
  ppOutput _  = "Nonlinear"

