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

{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Util.ConstrainedFunctor where

import qualified Data.Vector as V
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import GHC.Exts (Constraint)

data NoConstraints
data UnboxConstraint
data IsDoubleConstraint

class IdConstraint a
instance IdConstraint a

type family ElemConstraints k :: * -> Constraint
type instance ElemConstraints NoConstraints      = IdConstraint
type instance ElemConstraints UnboxConstraint    = Unbox
type instance ElemConstraints IsDoubleConstraint = (~) Double

class ConstrainedFunctor k f | f -> k where
  cfmap :: (ElemConstraints k a, ElemConstraints k b) => (a -> b) -> f a -> f b

instance ConstrainedFunctor NoConstraints [] where
  {-# INLINABLE cfmap #-}
  cfmap = fmap

instance ConstrainedFunctor NoConstraints V.Vector where
  {-# INLINABLE cfmap #-}
  cfmap = V.map

instance ConstrainedFunctor UnboxConstraint U.Vector where
  {-# INLINABLE cfmap #-}
  cfmap = U.map

