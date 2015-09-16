----------------------------------------------------------------------------
-- |
-- Module      :  Data.ConstrainedConvert
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Data.ConstrainedConvert where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U

import Data.ConstrainedFunctor

class (ConstrainedFunctor f, ConstrainedFunctor f') => Convert f f' | f -> f' where
  convertTo   :: (ElemConstraints f a, ElemConstraints f' a) => f a  -> f' a
  convertFrom :: (ElemConstraints f a, ElemConstraints f' a) => f' a -> f a

instance Convert V.Vector V.Vector where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = id
  convertFrom = id

instance Convert [] [] where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = id
  convertFrom = id

instance Convert U.Vector U.Vector where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = id
  convertFrom = id

instance Convert S.Vector S.Vector where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = id
  convertFrom = id

{-# INLINABLE mapConverting #-}
mapConverting
  :: (Convert f f', ConstrainedFunctor f')
  => (ElemConstraints f a, ElemConstraints f' a)
  => (ElemConstraints f b, ElemConstraints f' b)
  => (a -> b)
  -> f a
  -> f b
mapConverting f = convertFrom . cfmap f . convertTo
