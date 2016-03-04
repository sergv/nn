----------------------------------------------------------------------------
-- |
-- Module      :  Data.ConstrainedIsomorphism
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

module Data.ConstrainedIsomorphism where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U

import Data.ConstrainedFunctor

class (ConstrainedFunctor f, ConstrainedFunctor f') => ConstrainedIsomorphism f f' | f -> f' where
  convertTo   :: (ElemConstraints f a, ElemConstraints f' a) => f a  -> f' a
  convertFrom :: (ElemConstraints f a, ElemConstraints f' a) => f' a -> f a

instance ConstrainedIsomorphism V.Vector V.Vector where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = id
  convertFrom = id

instance ConstrainedIsomorphism [] [] where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = id
  convertFrom = id

instance ConstrainedIsomorphism U.Vector U.Vector where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = id
  convertFrom = id

instance ConstrainedIsomorphism S.Vector S.Vector where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = id
  convertFrom = id

{-# INLINABLE mapConverting #-}
mapConverting
  :: (ConstrainedIsomorphism f f', ConstrainedFunctor f')
  => (ElemConstraints f a, ElemConstraints f' a)
  => (ElemConstraints f b, ElemConstraints f' b)
  => (a -> b)
  -> f a
  -> f b
mapConverting f = convertFrom . cfmap f . convertTo
