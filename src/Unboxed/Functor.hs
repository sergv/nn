----------------------------------------------------------------------------
-- |
-- Module      :  Unboxed.Functor
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Unboxed.Functor (UnboxedFunctor(..), Unbox) where

import qualified Data.Vector as V
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U

class UnboxedFunctor f where
  ufmap :: (Unbox a, Unbox b) => (a -> b) -> f a -> f b

instance UnboxedFunctor [] where
  ufmap = fmap

instance UnboxedFunctor V.Vector where
  ufmap = V.map

instance UnboxedFunctor U.Vector where
  ufmap = U.map
