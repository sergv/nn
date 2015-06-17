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
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Util where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Leijen.Text (Doc, Pretty)
import qualified Text.PrettyPrint.Leijen.Text as PP

-- Nonlinearity and Output types

data HyperbolicTangent
data Sigmoid

class Nonlinearity n where
  nonlinearity :: (Floating a) => nn n o b -> a -> a
  ppNonlinearity :: nn n o a -> Doc

instance Nonlinearity HyperbolicTangent where
  nonlinearity _ x = tanh x
  ppNonlinearity _ = "HyperbolicTangent"

instance Nonlinearity Sigmoid where
  nonlinearity _ x = x' / (1 + x')
    where
      x' = exp x
  ppNonlinearity _ = "Sigmoid"

data Linear
data Nonlinear

class (Nonlinearity n) => OutputType o n where
  output :: (Nonlinearity n, Floating a) => nn n o b -> a -> a
  ppOutput :: nn n o a -> Doc

instance (Nonlinearity n) => OutputType Linear n where
  output _ x = x
  ppOutput _ = "Linear"

instance (Nonlinearity n) => OutputType Nonlinear n where
  output = nonlinearity
  ppOutput _ = "Nonlinear"

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
display = PP.displayT . PP.renderPretty 0.8 80 . PP.pretty

