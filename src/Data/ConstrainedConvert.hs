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

class Convert k k' f f' | f -> k f' k', f' -> k' where
  convertTo   :: (ElemConstraints k a, ElemConstraints k' a) => f a  -> f' a
  convertFrom :: (ElemConstraints k a, ElemConstraints k' a) => f' a -> f a

instance Convert NoConstraints NoConstraints V.Vector V.Vector where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = id
  convertFrom = id

instance Convert NoConstraints NoConstraints [] [] where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = id
  convertFrom = id

instance Convert UnboxConstraint UnboxConstraint U.Vector U.Vector where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = id
  convertFrom = id

instance Convert StorableConstraint StorableConstraint S.Vector S.Vector where
  {-# INLINABLE convertTo   #-}
  {-# INLINABLE convertFrom #-}
  convertTo   = id
  convertFrom = id

{-# INLINABLE mapConverting #-}
mapConverting
  :: (Convert k k' f f', ConstrainedFunctor k' f')
  => (ElemConstraints k a, ElemConstraints k' a)
  => (ElemConstraints k b, ElemConstraints k' b)
  => (a -> b)
  -> f a
  -> f b
mapConverting f = convertFrom . cfmap f . convertTo
