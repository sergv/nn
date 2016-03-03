----------------------------------------------------------------------------
-- |
-- Module      :  Data.Grad
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday,  3 March 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Grad where

import Data.ConstrainedFunctor

-- | Documentation wrapper to distinguish gradients from vanilla vectors.
newtype Grad f a = Grad { getGrad :: f a }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (ConstrainedFunctor f) => ConstrainedFunctor (Grad f) where
  type (ElemConstraints (Grad f)) = ElemConstraints f
  {-# INLINABLE cfmap #-}
  cfmap f (Grad x) = Grad $ cfmap f x

