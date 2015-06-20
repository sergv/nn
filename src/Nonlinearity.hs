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
  nonlinearityDeriv
    :: (Floating a)
    => nn n o b -- ^ proxy neural network
    -> a        -- ^ value of nonlinearity on input (it may be faster to reuse it)
    -> a        -- ^ input
    -> a
  ppNonlinearity :: nn n o a -> Doc

instance Nonlinearity HyperbolicTangent where
  nonlinearity _ x      = tanh x
  nonlinearityDeriv _ nonlinX _ = 1 -! nonlinX^(2 :: Int)
  -- nonlinearityDeriv _ _ x = 1 -! (tanh x)^(2 :: Int)
  ppNonlinearity _      = "HyperbolicTangent"

instance Nonlinearity Sigmoid where
  nonlinearity _ x = x' /! (1 +! x')
    where
      x' = exp x
  nonlinearityDeriv _ nonlinX x = nonlinX /! (1 +! exp x)
  -- nonlinearityDeriv _ x = x' /! (1 +! x')^(2 :: Int)
  --   where
  --     x' = exp x
  ppNonlinearity _ = "Sigmoid"

data Linear
data Nonlinear

class (Nonlinearity n) => OutputType o n where
  output      :: (Nonlinearity n, Floating a) => nn n o b -> a -> a
  outputDeriv :: (Nonlinearity n, Floating a) => nn n o b -> a -> a -> a
  ppOutput    :: nn n o a -> Doc

instance (Nonlinearity n) => OutputType Linear n where
  output _ x        = x
  outputDeriv _ _ _ = 1
  ppOutput _        = "Linear"

instance (Nonlinearity n) => OutputType Nonlinear n where
  output      = nonlinearity
  outputDeriv = nonlinearityDeriv
  ppOutput _  = "Nonlinear"

