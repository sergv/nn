----------------------------------------------------------------------------
-- |
-- Module      :  NN.Description
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Sunday, 22 May 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module NN.Description
  ( Description(..)
  , relaxDescription
  , mkDescription
  , NNDescription(..)
  , ShrinkType(..)
  , shrinkDescription
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Sequence (Seq, ViewL(..), (|>))
import qualified Data.Sequence as Seq
import GHC.Generics
import Test.QuickCheck
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.ConstrainedFunctor
import Data.Nonlinearity.Proxies
import Util (drop1, interleave)

data Description n o a = Description
  { descriptionInputSize    :: Int -- ^ Dimensionality of input
  , descriptionOutputSize   :: Int -- ^ Dimensionality of output
  -- | Hidden layers including input layer, may be empty. Each layer is
  -- accompanied by its bias.
  , descriptionHiddenLayers :: [(NonEmpty a, NonEmpty (NonEmpty a))]
  , descriptionFinalLayer   :: (NonEmpty a, NonEmpty (NonEmpty a))
  } deriving (Show, Eq, Ord, Functor, Generic)

instance
  ( PrettyProxy n
  , PrettyProxy o
  , Pretty a
  ) => Pretty (Description n o a) where
  pretty descr@(Description inputSize outputSize hiddenLayers outputLayer) =
    PP.vsep
      [ "Nonlinearity: " <> prettyProxy (NonlinearityProxy descr)
      , "Output: "       <> prettyProxy (OutputProxy descr)
      , "Input size: "   <> pretty inputSize
      , "Output size: "  <> pretty outputSize
      , "HiddenLayers: " <> PP.hcat
                            (PP.punctuate (PP.line <> PP.line) $
                             map showLayer hiddenLayers)
      , "OutputLayer: "  <> showLayer outputLayer
      ]
    where
      showLayer :: (NonEmpty a, NonEmpty (NonEmpty a)) -> Doc
      showLayer (bias, weights) =
        "bias:   " <> showWeights bias PP.<$>
        "weighs: " <> PP.align (PP.vsep $ map showRow $ toList weights)
      showWeights :: NonEmpty a -> Doc
      showWeights = PP.hcat . PP.punctuate PP.comma . map pretty . toList
      showRow :: NonEmpty a -> Doc
      showRow = PP.hcat . PP.punctuate PP.comma . map pretty . toList

relaxDescription :: Description n o a -> Description n' o' a
relaxDescription Description{..} = Description{..}

mkDescription
  :: forall m n o a. (Monad m)
  => Int -- ^ Size of inputs NN would operate on
  -> [Int]
  -> Int -- ^ Size of outputs NN would produce
  -> m a
  -> m (Description n o a)
mkDescription inputSize hiddenLayerSizes outputSize mkElem
  | inputSize > 0 && all (>0) hiddenLayerSizes && outputSize > 0 = do
    (lastHiddenSize, hiddenLayersRev) <- foldM mkHiddenLayer (inputSize, []) hiddenLayerSizes
    outputLayer                        <- mkLayer outputSize lastHiddenSize
    return Description
      { descriptionInputSize    = inputSize
      , descriptionOutputSize   = outputSize
      , descriptionHiddenLayers = reverse hiddenLayersRev
      , descriptionFinalLayer   = outputLayer
      }
    -- return $ reverse $ outputLayer : hiddenLayersRev
  | otherwise = fail "Cannot create description when one of the sizes is zero"
  where
    mkLayer :: Int -> Int -> m (NonEmpty a, NonEmpty (NonEmpty a))
    mkLayer size prevSize =
      (,) <$> replicateMNE size mkElem
          <*> replicateMNE size (replicateMNE prevSize mkElem)

    mkHiddenLayer
      :: (Int, [(NonEmpty a, NonEmpty (NonEmpty a))])
      -> Int
      -> m (Int, [(NonEmpty a, NonEmpty (NonEmpty a))])
    mkHiddenLayer (prevSize, layers) size = do
      layer <- mkLayer size prevSize
      return (size, layer : layers)

replicateMNE :: (Monad m) => Int -> m a -> m (NonEmpty a)
replicateMNE n action
  | n > 0     = (:|) <$> action <*> replicateM (n - 1) action
  | otherwise =
    fail $ "Cannot create non-empty from zero repetitons: " ++ show n


class NNDescription (nn :: * -> * -> * -> *) where
  fromDescription
    :: (ElemConstraints (nn n o) a, MonadError String m, Show a)
    => Description n o a -> m (nn n o a)
  toDescription
    :: (ElemConstraints (nn n o) a, MonadError String m)
    => nn n o a -> m (Description n o a)

instance (Arbitrary a) => Arbitrary (Description n o a) where
  arbitrary = do
    Positive (inputSize :: Int) <- arbitrary
    Positive (outputSize :: Int) <- arbitrary
    hiddenSizes                 <- fmap getPositive <$> listOf1 arbitrary
    mkDescription inputSize hiddenSizes outputSize arbitrary
  shrink = map snd . shrinkDescription

data ShrinkType =
  -- | Input size decreased
    ShrinkInput
      Int -- ^ Which input vector index was deleted
  -- | Some layer size decreased
  | ShrinkLayer
  -- | Number or layers decreased. Nor input nor final layer decrease.
  | ShrinkNumberOfLayers
  -- | Final size decreased
  | ShrinkOutput
      Int -- ^ Which final vector index was deleted
  deriving (Show, Eq, Ord)

-- | Invariant: inputSize and outputSize can only decrease
shrinkDescription
  :: forall n o a. Description n o a -> [(ShrinkType, Description n o a)]
shrinkDescription (Description inputSize outputSize hiddenLayers outputLayer) =
  map (first ShrinkInput)                  (shrinkInputLayer allLayers) `interleave`
  map (first ShrinkOutput)                 (shrinkOutputLayer outputLayer) `interleave`
  map ((ShrinkLayer, ) . mkDescr)          (shrinkHiddenLayers drop1InHiddenLayer allLayers) `interleave`
  map ((ShrinkNumberOfLayers, ) . mkDescr) (throwAwayLayers allLayers)
  where
    allLayers :: NonEmpty (NonEmpty a, NonEmpty (NonEmpty a))
    allLayers = NE.fromList $ hiddenLayers ++ [outputLayer]

    shrinkInputLayer
      :: NonEmpty (NonEmpty a, NonEmpty (NonEmpty a))
      -> [(Int, Description n o a)]
    shrinkInputLayer (firstLayer :| rest) =
      [ (n, Description inputSize' outputSize (NE.init layers) (NE.last layers))
      | let inputSize' = inputSize - 1
      , inputSize' > 0
      , (n, firstLayer') <- drop1InInputLayer firstLayer
      , let layers = firstLayer' :| rest
      ]

    drop1InInputLayer
      :: (NonEmpty a, NonEmpty (NonEmpty a))
      -> [(Int, (NonEmpty a, NonEmpty (NonEmpty a)))]
    drop1InInputLayer (bias, weights) =
      zipWith
        (\n wss' -> (n, (bias, wss')))
        [0..]
        (getZipList $ traverse (ZipList . drop1) weights)


    shrinkOutputLayer
      :: (NonEmpty a, NonEmpty (NonEmpty a))
      -> [(Int, Description n o a)]
    shrinkOutputLayer layer =
      [ (n, Description inputSize outputSize' hiddenLayers layer')
      | let outputSize' = outputSize - 1
      , outputSize' > 0
      , (n, layer') <- drop1InOutputLayer layer
      ]

    drop1InOutputLayer
      :: (NonEmpty a, NonEmpty (NonEmpty a))
      -> [(Int, (NonEmpty a, NonEmpty (NonEmpty a)))]
    drop1InOutputLayer (bias, weights) =
      zipWith3 (\n b ws -> (n, (b, ws))) [0..] (drop1 bias) (drop1 weights)

-- This does not change the number of hidden layers
shrinkHiddenLayers :: (b -> b -> [(b, b)]) -> NonEmpty b -> [NonEmpty b]
shrinkHiddenLayers _ (_ :| [])      = [] -- final layer remains unchanged
shrinkHiddenLayers f (x :| x' : xs) =
  [ y :| y' : xs
  | (y, y') <- f x x'
  ] `interleave`
  map (NE.cons x) (shrinkHiddenLayers f (x' :| xs))

-- After dropping something from some layer we must fix the following
-- layer.
drop1InHiddenLayer
  :: forall a. (NonEmpty a, NonEmpty (NonEmpty a))
  -> (NonEmpty a, NonEmpty (NonEmpty a))
  -> [((NonEmpty a, NonEmpty (NonEmpty a)), (NonEmpty a, NonEmpty (NonEmpty a)))]
drop1InHiddenLayer (bias, weights) _followingLayer@(followingBias, followingWeights)
  | all ((> 1) . NE.length) followingWeights =
    zipWith3
      (\n bs ws ->
         ( (bs, ws)
         , (followingBias, NE.fromList . toList . dropNth n <$> followingWeights')
         ))
      [0..]
      (drop1 bias)
      (drop1 weights)
  | otherwise = []
  where
    followingWeights' :: NonEmpty (Seq a)
    followingWeights' = Seq.fromList . toList <$> followingWeights

throwAwayLayers
  :: NonEmpty (NonEmpty a, NonEmpty (NonEmpty a))
  -> [NonEmpty (NonEmpty a, NonEmpty (NonEmpty a))]
throwAwayLayers xs =
  toList $ execWriter $ collect xs mempty
  where
    collect
      :: NonEmpty (b, NonEmpty (NonEmpty c))
      -> Seq (b, NonEmpty (NonEmpty c))
      -> Writer (DList (NonEmpty (b, NonEmpty (NonEmpty c)))) ()
    collect xs prefix =
      case xs of
        _ :| []     ->
          tell $ maybe mempty DL.singleton $ seqToNE prefix
        x :| _ : [] ->
          tell $ DL.singleton $ NE.fromList $ toList $ prefix |> x
        x :| x' : xs'@(x'' : xs'') -> do
          when (compatibleWeights (snd x) (snd x'')) $
            tell $ DL.singleton $ x :| x'' : toList xs''
          collect (x' :| xs') $ prefix |> x

-- Check whether weighs @us@ can follow weighs @ws@ in the neural network.
-- I.e. check whether inputs produced by @ws@ can be handled by @us@.
compatibleWeights :: NonEmpty (NonEmpty a) -> NonEmpty (NonEmpty a) -> Bool
compatibleWeights ws us = wsOutput == usInput
  where
    wsOutput = NE.length ws
    usInput  = NE.length $ NE.head us

seqToNE :: Seq a -> Maybe (NonEmpty a)
seqToNE xs =
  case Seq.viewl xs of
    EmptyL   -> Nothing
    x :< xs' -> Just $ x :| toList xs'

mkDescr :: NonEmpty (NonEmpty a, NonEmpty (NonEmpty a)) -> Description n o a
mkDescr hiddenLayers'@((_, row :| _) :| _) =
  Description
    { descriptionInputSize    = NE.length row
    , descriptionOutputSize   = NE.length finalBias
    , descriptionHiddenLayers = NE.init hiddenLayers'
    , descriptionFinalLayer   = finalLayer'
    }
  where
    finalLayer'@(finalBias, _) = NE.last hiddenLayers'

dropNth :: Int -> Seq a -> Seq a
dropNth n xs =
  case Seq.viewl suffix of
    EmptyL  -> prefix
    _ :< ys -> prefix <> ys
  where
    (prefix, suffix) = Seq.splitAt n xs
