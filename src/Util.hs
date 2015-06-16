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

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDecls    #-}

module Util where

import Control.DeepSeq
import qualified Data.Text.Lazy as T

import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

-- Nonlinearity and Output types

data HyperbolicTangent
data Sigmoid

data NonlinearityType a = HyperbolicTangent
                        | Sigmoid
                        deriving (Show, Eq, Ord)

prettyShow :: (Show a) => a -> Doc
prettyShow = PP.text . T.pack . show

instance Pretty (NonlinearityType a) where
  pretty = prettyShow

instance NFData (NonlinearityType a) where
  rnf HyperbolicTangent = ()
  rnf Sigmoid           = ()

hyperbolicTangentNT :: NonlinearityType HyperbolicTangent
hyperbolicTangentNT = HyperbolicTangent

sigmoidNT :: NonlinearityType Sigmoid
sigmoidNT = Sigmoid

data Linear
data Nonlinear

data OutputType a = Linear
                  | Nonlinear
                  deriving (Show, Eq, Ord)

instance Pretty (OutputType a) where
  pretty = prettyShow

instance NFData (OutputType a) where
  rnf Linear    = ()
  rnf Nonlinear = ()

linearOut :: OutputType Linear
linearOut = Linear

nonlinearOut :: OutputType Nonlinear
nonlinearOut = Nonlinear

-- data NonlinearityType a where
--   HyperbolicTangent :: NonlinearityType HyperbolicTangent
--   Sigmoid :: NonlinearityType Sigmoid
--
-- deriving instance (Show (NonlinearityType a))
-- deriving instance (Eq (NonlinearityType a))
-- deriving instance (Ord (NonlinearityType a))

-- Other utils

newtype Grad f a = Grad { getGrad :: f a }
                 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

linspace :: Int -> Double -> Double -> [Double]
linspace n low hi = map (\k -> low + fromIntegral k * delta) [0..n]
  where
    delta = (hi - low) / fromIntegral (n - 1)

