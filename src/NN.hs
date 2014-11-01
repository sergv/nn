----------------------------------------------------------------------------
-- |
-- Module      :  NN
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NN where

import Control.Applicative
import Control.Arrow
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Identity
import Data.Foldable (Foldable)
import Data.List
import Data.Monoid
import Data.Traversable (Traversable)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Numeric.AD hiding (gradientDescent, Grad)

import Data.Random.Distribution.Normal (stdNormal)
import Data.Random.Sample (sample)
import Data.Random.Source (MonadRandom)
import Data.Random.Source.PureMT ()

import Util

data HyperbolicTangent
data Sigmoid

data NonlinearityType a = HyperbolicTangent
                        | Sigmoid
                        deriving (Show, Eq, Ord)

prettyShow :: (Show a) => a -> Doc
prettyShow = PP.text . T.pack . show

instance Pretty (NonlinearityType a) where
  pretty = prettyShow

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

data NN n o a = NN {-# UNPACK #-} !(NonlinearityType n)
                   {-# UNPACK #-} !(OutputType o)
                   [Vector (Vector a)] -- hidden layers
                   (Vector (Vector a)) -- final layer
              deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

nnZipWith :: (a -> b -> c) -> NN n o a -> NN n o b -> NN n o c
nnZipWith f (NN nonlinType outType xs finX) (NN _ _ ys finY) =
  NN nonlinType
     outType
     (zipWith zipLayers xs ys)
     (zipLayers finX finY)
  where
    zipLayers = V.zipWith (V.zipWith f)

nnZipWith3 :: (a -> b -> c -> d) -> NN n o a -> NN n o b -> NN n o c -> NN n o d
nnZipWith3 f (NN nonlinType outType xs finX) (NN _ _ ys finY) (NN _ _ zs finZ) =
  NN nonlinType
     outType
     (zipWith3 zipLayers xs ys zs)
     (zipLayers finX finY finZ)
  where
    zipLayers = V.zipWith3 (V.zipWith3 f)

nnZipWith4 :: (a -> b -> c -> d -> e) -> NN n o a -> NN n o b -> NN n o c -> NN n o d -> NN n o e
nnZipWith4 f (NN nonlinType outType xs finX) (NN _ _ ys finY) (NN _ _ zs finZ) (NN _ _ ws finW) =
  NN nonlinType
     outType
     (zipWith4 zipLayers xs ys zs ws)
     (zipLayers finX finY finZ finW)
  where
    zipLayers = V.zipWith4 (V.zipWith4 f)

-- nnZ = b * nnX + NNy
addScaled :: (Floating a) => NN n o a -> a -> NN n o a -> NN n o a
-- addScaled b (NN xs) (NN ys) = NN $ zipWith (\x y -> V.zipWith (\x' y' -> V.zipWith () x' y') x y) xs ys
addScaled nn b addend = nnZipWith (\x y -> x + b * y) nn addend

nnSize :: (Floating a) => NN n o a -> a
nnSize (NN _ _ layers fin) =
  sqrt $ sum (map layerSize layers) + layerSize fin
  where
    layerSize = V.sum . V.map (V.sum . V.map (^(2 :: Int)))

differenceSize :: (Floating a) => NN n o a -> NN n o a -> a
differenceSize xs ys = nnSize $ addScaled xs (-1) ys

-- layer size should be specified without accounting for bias
makeNN :: forall m n o. (Applicative m, MonadRandom m) =>
          NonlinearityType n ->
          OutputType o       ->
          Int                ->
          [Int]              ->
          Int                ->
          m (NN n o Double)
makeNN nonlinType outType firstLayerSize hiddenLayerSizes finalLayerSize = do
  (lastHiddenSize, hiddenLayers) <- second reverse <$> foldM f (firstLayerSize, []) hiddenLayerSizes
  finalLayer <- mkLayer finalLayerSize lastHiddenSize
  return $ NN nonlinType outType hiddenLayers finalLayer
  where
    mkLayer :: Int -> Int -> m (Vector (Vector Double))
    mkLayer size prevSize = V.replicateM size (V.replicateM prevSizeWithBias (sample stdNormal))
      where
        prevSizeWithBias = prevSize + 1

    f :: (Int, [Vector (Vector Double)]) -> Int -> m (Int, [Vector (Vector Double)])
    f (prevSize, layers) size = do
      layer <- mkLayer size prevSize
      return (size, layer : layers)

nonlinearity :: (Floating a) => NonlinearityType n -> a -> a
nonlinearity HyperbolicTangent x = tanh x
nonlinearity Sigmoid           x = x' / (1 + x')
  where
    x' = exp x

output :: (Floating a) => NonlinearityType n -> OutputType o -> a -> a
output _      Linear    = id
output nonlin Nonlinear = nonlinearity nonlin

forwardPropagate :: (Floating a) => NN n o a -> Vector a -> Vector a
forwardPropagate (NN nonlinType outType hiddenLayers finalLayer) input =
  f (output nonlinType outType)
    (foldl' (f (nonlinearity nonlinType)) input hiddenLayers)
    finalLayer
  where
    f activation prev layer = V.map (\v -> seqIt $
                                           activation $
                                           V.head v + dot prev (V.tail v))
                                    layer

targetFunction :: (Floating a, Mode a) =>
                  [(Vector (Scalar a), Vector (Scalar a))] ->
                  NN n o a                                 ->
                  a
targetFunction dataset nn =
  sum $
  map (\(x, y) -> vectorSize $
                  V.zipWith (-) (forwardPropagate nn x) y)
      (map (V.map auto *** V.map auto) dataset)

targetFunctionGrad :: [(Vector Double, Vector Double)] ->
                      NN n o Double                    ->
                      (Double, Grad (NN n o) Double)
targetFunctionGrad dataset = \nn -> second Grad $ grad' (targetFunction dataset) nn

instance forall n o a. (Show a) => Pretty (NN n o a) where
  pretty (NN nonlinType outType hiddenLayers finalLayer) =
    "Nonlinearity: " <> prettyShow nonlinType <> PP.line <>
    "Output: "       <> prettyShow outType    <> PP.line <>
    "HiddenLayers: " <> prettyShow outType    <> PP.line <>
                        (combine $
                         PP.punctuate (PP.line <> PP.line) $
                         map showLayer hiddenLayers) <> PP.line <>
    "OutputLayer: "  <> showLayer finalLayer
    where
      showLayer :: Vector (Vector a) -> Doc
      showLayer = PP.vsep .
                  map (combine . PP.punctuate PP.comma . map prettyShow . V.toList) .
                  V.toList

      combine = foldr (<>) PP.empty

ppNN :: forall a n o. (Show a) => NN n o a -> Text
ppNN nn = PP.displayT $ PP.renderPretty 0.8 80 $ pretty nn

