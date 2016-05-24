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

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util
  ( linspace
  , prettyShow
  , display
  , display'
  , (+!)
  , (-!)
  , (*!)
  , (/!)
  , splitVec
  , drop1
  , interleave
  , takeBy
  ) where

import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid
import Data.Sequence (Seq, ViewL(..))
import qualified Data.Sequence as Seq
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.PrettyPrint.Leijen.Text (Doc, Pretty(..))
import qualified Text.PrettyPrint.Leijen.Text as PP

-- Other utils

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

-- | Split vector @vs@ into chunks of size @n@ and a leftover chunk, whose
-- size is not multiple of @n@.
splitVec :: Int -> Vector a -> ([Vector a], Vector a)
splitVec n vs =
  (map (mkVec . (, n)) [0..lastSlice - 1 - i], mkVec (lastSlice - i, lastSize'))
  where
    mkVec (k, m) = V.unsafeSlice (k * n) m vs
    (lastSlice, lastSize) = V.length vs `divMod` n
    (i, lastSize') | lastSize == 0 = (1, n)
                   | otherwise     = (0, lastSize)

-- | Remove single element from list in a multitude of ways.
drop1 :: forall a. NonEmpty a -> [NonEmpty a]
drop1 (_ :| []) = []
drop1 (x :| xs@(y : ys)) =
  (y :| ys) : map ((x :|) . toList) (go (Seq.fromList xs) mempty [])
  where
    go :: Seq a -> Seq a -> [Seq a] -> [Seq a]
    go xs prefix yss =
      case Seq.viewl xs of
        EmptyL   -> reverse yss
        x :< xs' ->
          go xs' (prefix Seq.|> x) (prefix <> xs' : yss)

interleave :: [a] -> [a] -> [a]
interleave []     ys     = ys
interleave xs     []     = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

{-# INLINABLE takeBy #-}
takeBy :: Int -> [a] -> [[a]]
takeBy _ [] = []
takeBy n xs = ys : takeBy n zs
  where
    (ys, zs) = splitAt n xs
