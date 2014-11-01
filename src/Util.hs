----------------------------------------------------------------------------
-- |
-- Module      :  Util
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

module Util where

import Data.Vector (Vector)
import qualified Data.Vector as V


newtype Grad f a = Grad { getGrad :: f a }
                 deriving (Show, Eq, Ord, Functor)

dot :: (Floating a) => Vector a -> Vector a -> a
dot xs ys = if V.length xs /= V.length ys
            then error "cannot take dot products for vectors of different length"
            else V.foldr (+) 0 $ V.zipWith (*) xs ys

{-# INLINABLE seqIt #-}
seqIt :: a -> a
seqIt x = seq x x

vectorSize :: (Floating a) => Vector a -> a
vectorSize = sqrt . V.sum . V.map (\x -> x * x) -- (^(2 :: Int))

linspace :: Int -> Double -> Double -> [Double]
linspace n low hi = map (\k -> low + fromIntegral k * delta) [0..n]
  where
    delta = (hi - low) / fromIntegral (n - 1)

