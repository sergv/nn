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

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util where

import Control.Monad
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.PrettyPrint.Leijen.Text (Doc, Pretty(..))
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.ConstrainedFunctor

-- Other utils

newtype Grad f a = Grad { getGrad :: f a }
                 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (ConstrainedFunctor f) => ConstrainedFunctor (Grad f) where
  type (ElemConstraints (Grad f)) = ElemConstraints f
  {-# INLINABLE cfmap #-}
  cfmap f (Grad x) = Grad $ cfmap f x

linspace :: Int -> Double -> Double -> [Double]
linspace n low hi = map (\k -> low + fromIntegral k * delta) [0..n]
  where
    delta = (hi - low) / fromIntegral (n - 1)

-- Pretty utils

prettyShow :: (Show a) => a -> Doc
prettyShow = PP.text . T.pack . show

display :: (Pretty a) => a -> Text
display = PP.displayT . PP.renderPretty 0.9 100 . PP.pretty

display' :: (Pretty a) => a -> String
display' = T.unpack . display

{-# INLINABLE (+!) #-}
(+!) :: (Num a) => a -> a -> a
(+!) x y = z `seq` z
  where
    z = x + y

infixl 6 +!

{-# INLINABLE (-!) #-}
(-!) :: (Num a) => a -> a -> a
(-!) x y = z `seq` z
  where
    z = x - y

infixl 6 -!

{-# INLINABLE (*!) #-}
(*!) :: (Num a) => a -> a -> a
(*!) x y = z `seq` z
  where
    z = x * y

infixl 7 *!

{-# INLINABLE (/!) #-}
(/!) :: (Fractional a) => a -> a -> a
(/!) x y = z `seq` z
  where
    z = x / y

infixl 7 /!

instance (Pretty a) => Pretty (Vector a) where
  pretty = pretty . V.toList


makeWeightList :: forall m a. (Monad m) => Int -> [Int] -> Int -> m a -> m [[[a]]]
makeWeightList inputLayerSize hiddenLayerSizes finalLayerSize mkElem = do
  (lastHiddenSize, hiddenLayersRev) <- foldM f (inputLayerSize, []) hiddenLayerSizes
  finalLayer                        <- mkLayer finalLayerSize lastHiddenSize
  return $ reverse $ finalLayer : hiddenLayersRev
  where
    mkLayer :: Int -> Int -> m [[a]]
    mkLayer size prevSize = replicateM size (replicateM prevSizeWithBias mkElem)
      where
        prevSizeWithBias = prevSize + 1

    f :: (Int, [[[a]]]) -> Int -> m (Int, [[[a]]])
    f (prevSize, layers) size = do
      layer <- mkLayer size prevSize
      return (size, layer : layers)

splitVec :: Int -> Vector a -> ([Vector a], Vector a)
splitVec n vs =
  (map (mkVec . (, n)) [0..lastSlice - 1 - i], mkVec (lastSlice - i, lastSize'))
  where
    mkVec (k, m) = V.unsafeSlice (k * n) m vs
    (lastSlice, lastSize) = V.length vs `divMod` n
    (i, lastSize') | lastSize == 0 = (1, n)
                   | otherwise     = (0, lastSize)

{-# INLINABLE takeBy #-}
takeBy :: Int -> [a] -> [[a]]
takeBy _ [] = []
takeBy n xs = ys : takeBy n zs
  where
    (ys, zs) = splitAt n xs
