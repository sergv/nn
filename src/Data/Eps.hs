----------------------------------------------------------------------------
-- |
-- Module      :  Data.Eps
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 30 September 2015
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Data.Eps where

class (Floating a) => Eps a where
  getEps :: p a -> a

instance Eps Float where
  {-# INLINE getEps #-}
  getEps _ = 1e-7

instance Eps Double where
  {-# INLINE getEps #-}
  getEps _ = 1e-15

