----------------------------------------------------------------------------
-- |
-- Module      :  ApproxEq
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Saturday, 21 May 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module ApproxEq
  ( ApproxEq(..)
  , ToDouble(..)
  , FromDouble(..)
  , machineEps
  , eps
  ) where

import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.Aligned.Double

data ApproxEq = forall a. (Show a, Pretty a, ToDouble a) => ApproxEq a

machineEps :: Double
machineEps = 1.11022302462516e-16

eps :: Double
eps = sqrt $ sqrt machineEps

instance Eq ApproxEq where
  ApproxEq x == ApproxEq y = abs (toDouble x - toDouble y) <= eps

instance Show ApproxEq where
  show (ApproxEq x) = show x

instance Pretty ApproxEq where
  pretty (ApproxEq x) = pretty x


class ToDouble a where
  toDouble :: a -> Double

instance ToDouble Double where
  toDouble = id

instance ToDouble AlignedDouble where
  toDouble = getAlignedDouble

class FromDouble a where
  fromDouble :: Double -> a

instance FromDouble Double where
  fromDouble = id

instance FromDouble AlignedDouble where
  fromDouble = mkAlignedDouble
