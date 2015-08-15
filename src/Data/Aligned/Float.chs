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
  , IsAlignedFloatConstraint
  , toAlignedFloat
  , fromAlignedFloat
  )
where

import Control.DeepSeq
import Foreign
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.ConstrainedFunctor

import Graphics.Rendering.Chart (PlotValue)

#include <arithmetic.h>

newtype AlignedFloat = AlignedFloat
  { getAlignedFloat :: Float }
  deriving (Eq, Ord, Num, Fractional, Floating, NFData, PlotValue)

data IsAlignedFloatConstraint
type instance ElemConstraints IsAlignedFloatConstraint = (~) AlignedFloat

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

{-# INLINABLE toAlignedFloat #-}
toAlignedFloat :: Float -> AlignedFloat
toAlignedFloat = AlignedFloat
