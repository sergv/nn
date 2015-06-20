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

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}

module Util where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Leijen.Text (Doc, Pretty)
import qualified Text.PrettyPrint.Leijen.Text as PP

-- Other utils

newtype Grad f a = Grad { getGrad :: f a }
                 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

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

infixl 6 *!

{-# INLINABLE (/!) #-}
(/!) :: (Fractional a) => a -> a -> a
(/!) x y = z `seq` z
  where
    z = x / y

infixl 7 /!

