----------------------------------------------------------------------------
-- |
-- Module      :  Util.Zippable
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util.Zippable where

import qualified Data.List as L
import Data.Vector (Vector)
import qualified Data.Vector as V

class Zippable f where
  zipWith   :: (a -> b -> c) -> f a -> f b -> f c
  zipWith3  :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
  zipWith4  :: (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e

instance Zippable Vector where
  zipWith  = V.zipWith
  zipWith3 = V.zipWith3
  zipWith4 = V.zipWith4

instance Zippable [] where
  zipWith  = L.zipWith
  zipWith3 = L.zipWith3
  zipWith4 = L.zipWith4
