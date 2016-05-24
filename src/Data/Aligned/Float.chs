----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aligned.Float
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

module Data.Aligned.Float
  ( AlignedFloat(..)
  , mkAlignedFloat
  , fromAlignedFloat
  )
where

import Control.DeepSeq
import Data.Proxy
import Foreign
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.Eps

import Graphics.Rendering.Chart (PlotValue)

#include <arithmetic.h>

newtype AlignedFloat = AlignedFloat
  { getAlignedFloat :: Float }
  deriving (Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat, NFData, PlotValue)

instance Eps AlignedFloat where
  {-# INLINE getEps #-}
  getEps _ = AlignedFloat $ getEps (Proxy :: Proxy Float)

instance Show AlignedFloat where
  show = show . getAlignedFloat

instance Pretty AlignedFloat where
  pretty = pretty . getAlignedFloat

instance Storable AlignedFloat where
  {-# INLINABLE sizeOf    #-}
  {-# INLINABLE alignment #-}
  {-# INLINABLE peek      #-}
  {-# INLINABLE poke      #-}
  sizeOf _                  = {#sizeof float_aligned#}
  alignment _               = {#const FLOAT_ALIGNMENT#}
  peek ptr                  = AlignedFloat <$> peek (castPtr ptr)
  poke ptr (AlignedFloat x) = poke (castPtr ptr) x

{-# INLINABLE fromAlignedFloat #-}
fromAlignedFloat :: AlignedFloat -> Float
fromAlignedFloat = getAlignedFloat

{-# INLINABLE mkAlignedFloat #-}
mkAlignedFloat :: Float -> AlignedFloat
mkAlignedFloat = AlignedFloat
