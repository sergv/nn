----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aligned.Double
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Aligned.Double
  ( AlignedDouble(..)
  , mkAlignedDouble
  , fromAlignedDouble
  )
where

import Control.DeepSeq
import Data.Proxy
import Foreign
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.Eps

import Graphics.Rendering.Chart (PlotValue)

#include <arithmetic.h>

newtype AlignedDouble = AlignedDouble
  { getAlignedDouble :: Double }
  deriving (Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat, NFData, PlotValue)

instance Eps AlignedDouble where
  {-# INLINE getEps #-}
  getEps _ = AlignedDouble $ getEps (Proxy :: Proxy Double)

instance Show AlignedDouble where
  show = show . getAlignedDouble

instance Pretty AlignedDouble where
  pretty = pretty . getAlignedDouble

instance Storable AlignedDouble where
  {-# INLINABLE sizeOf    #-}
  {-# INLINABLE alignment #-}
  {-# INLINABLE peek      #-}
  {-# INLINABLE poke      #-}
  sizeOf _                   = {#sizeof double_aligned#}
  alignment _                = {#const DOUBLE_ALIGNMENT#}
  peek ptr                   = AlignedDouble <$> peek (castPtr ptr)
  poke ptr (AlignedDouble x) = poke (castPtr ptr) x

{-# INLINABLE fromAlignedDouble #-}
fromAlignedDouble :: AlignedDouble -> Double
fromAlignedDouble = getAlignedDouble

{-# INLINABLE mkAlignedDouble #-}
mkAlignedDouble :: Double -> AlignedDouble
mkAlignedDouble = AlignedDouble
