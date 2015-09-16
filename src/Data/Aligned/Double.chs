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
  , toAlignedDouble
  , fromAlignedDouble
  )
where

import Control.DeepSeq
import Foreign
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.ConstrainedFunctor

import Graphics.Rendering.Chart (PlotValue)

#include <arithmetic.h>

newtype AlignedDouble = AlignedDouble
  { getAlignedDouble :: Double }
  deriving (Eq, Ord, Num, Fractional, Floating, NFData, PlotValue)

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

{-# INLINABLE toAlignedDouble #-}
toAlignedDouble :: Double -> AlignedDouble
toAlignedDouble = AlignedDouble
