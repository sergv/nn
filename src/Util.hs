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
                 deriving (Show, Eq, Ord, ConstrainedFunctor k, Functor, Foldable, Traversable)

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


