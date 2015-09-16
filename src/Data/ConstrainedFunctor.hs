----------------------------------------------------------------------------
-- |
-- Module      :  Util.ConstrainedFunctor
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.ConstrainedFunctor where

import qualified Data.Vector as V
import Foreign (Storable)
import qualified Data.Vector.Storable as S
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import GHC.Exts (Constraint)

class IdConstraint a
instance IdConstraint a

class ConstrainedFunctor f where
  type ElemConstraints f :: * -> Constraint
  cfmap :: (ElemConstraints f a, ElemConstraints f b) => (a -> b) -> f a -> f b

instance ConstrainedFunctor [] where
  type ElemConstraints [] = IdConstraint
  {-# INLINABLE cfmap #-}
  cfmap = fmap

instance ConstrainedFunctor V.Vector where
  type ElemConstraints V.Vector = IdConstraint
  {-# INLINABLE cfmap #-}
  cfmap = V.map

instance ConstrainedFunctor U.Vector where
  type ElemConstraints U.Vector = Unbox
  {-# INLINABLE cfmap #-}
  cfmap = U.map

instance ConstrainedFunctor S.Vector where
  type ElemConstraints S.Vector = Storable
  {-# INLINABLE cfmap #-}
  cfmap = S.map

