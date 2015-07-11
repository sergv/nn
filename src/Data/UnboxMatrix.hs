----------------------------------------------------------------------------
-- |
-- Module      :  Data.UnboxMatrix
-- Copyright   :  (c) Sergey Vinokurov 2015
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
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.UnboxMatrix (UnboxMatrix) where

import Prelude hiding (zipWith, zipWith3)
import Control.DeepSeq
import Data.Monoid
import qualified Data.List as L
import qualified Data.Vector.Unboxed as U
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.MatrixClass
import Data.VectClass ((.+.))
import qualified Data.VectClass as VC
import Util
import Util.ConstrainedFunctor
import Util.Zippable

data UnboxMatrix a = UnboxMatrix
  { umRows    :: {-# UNPACK #-} !Int
  , umColumns :: {-# UNPACK #-} !Int
  , umData    :: {-# UNPACK #-} !(U.Vector a)
  }

{-# INLINABLE takeBy #-}
takeBy :: Int -> [a] -> [[a]]
takeBy _ [] = []
takeBy n xs = ys : takeBy n zs
  where
    (ys, zs) = splitAt n xs

unboxedMatrixToList :: (ElemConstraints UnboxConstraint a) => UnboxMatrix a -> [[a]]
unboxedMatrixToList (UnboxMatrix _ cols xs) = takeBy cols $ VC.toList xs

instance (ElemConstraints UnboxConstraint a, Pretty a) => Pretty (UnboxMatrix a) where
  pretty um@(UnboxMatrix rows cols _) =
    "Matrix " <> PP.int rows <> "x" <> PP.int cols <> " (double)" PP.<$>
    PP.vsep (L.map showRow $ unboxedMatrixToList um)
    where
      showRow :: [a] -> Doc
      showRow = PP.hcat . PP.punctuate PP.comma . L.map pretty

instance (ElemConstraints UnboxConstraint a) => NFData (UnboxMatrix a) where
  rnf (UnboxMatrix rows cols xs) = rnf rows `seq` rnf cols `seq` rnf xs

instance ConstrainedFunctor UnboxConstraint UnboxMatrix where
  {-# INLINABLE cfmap #-}
  cfmap f um = um { umData = cfmap f $ umData um }

instance Zippable UnboxConstraint UnboxMatrix where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith f (UnboxMatrix xRows xCols xs) (UnboxMatrix yRows yCols ys)
    | xRows == yRows && xCols == yCols =
      UnboxMatrix xRows xCols $ zipWith f xs ys
    | otherwise = error "UnboxMatrix.zipWith: cannot zip matrices of different shapes"
  zipWith3 f (UnboxMatrix xRows xCols xs) (UnboxMatrix yRows yCols ys) (UnboxMatrix zRows zCols zs)
    | xRows == yRows && yRows == zRows && xCols == yCols && yCols == zCols =
      UnboxMatrix xRows xCols $ zipWith3 f xs ys zs
    | otherwise = error "UnboxMatrix.zipWith3: cannot zip matrices of different shapes"
  zipWith4 f (UnboxMatrix xRows xCols xs) (UnboxMatrix yRows yCols ys) (UnboxMatrix zRows zCols zs) (UnboxMatrix wRows wCols ws)
    | xRows == yRows && yRows == zRows && zRows == wRows && xCols == yCols && yCols == zCols && zCols == wCols =
      UnboxMatrix xRows xCols $ zipWith4 f xs ys zs ws
    | otherwise = error "UnboxMatrix.zipWith4: cannot zip matrices of different shapes"

instance Matrix UnboxConstraint UnboxMatrix U.Vector where
  {-# INLINABLE rows         #-}
  {-# INLINABLE columns      #-}
  {-# INLINABLE outerProduct #-}
  {-# INLINABLE vecMulRight  #-}
  {-# INLINABLE vecMulLeft   #-}
  fromList [] =
    error "UnboxMatrix.fromList: cannot create PureMatrix from empty list of rows"
  fromList wss@(ws:_)
    | columns > 0 && all (== columns) (L.map length wss) =
      UnboxMatrix
        { umRows    = rows
        , umColumns = columns
        , umData    = VC.fromList $ L.concat wss
        }
    | otherwise =
      error $ "UnboxMatrix.fromList: cannot create PureMatrix from list " ++ show wss
    where
      rows    = length wss
      columns = length ws
  toList (UnboxMatrix _ cols xs) = takeBy cols $ VC.toList xs
  rows    = umRows
  columns = umColumns
  replicateM rows cols action =
    UnboxMatrix rows cols <$> VC.replicateM (rows *! cols) action
  outerProduct columnVec rowVec =
    UnboxMatrix
      { umRows    = VC.length columnVec
      , umColumns = VC.length rowVec
      , umData    = U.concatMap (\c -> cfmap (c *!) rowVec) columnVec
      }
  vecMulRight (UnboxMatrix rows cols xs) ys =
    VC.fromList $ L.map (\zs -> VC.dot zs ys) $ vecTakeBy rows cols xs
  vecMulLeft xs (UnboxMatrix rows cols ys) =
    L.foldr (.+.) zeroVector $
    L.zipWith (\x ys -> cfmap (x *!) ys) (VC.toList xs) $ vecTakeBy rows cols ys
    where
      zeroVector = VC.replicate cols 0

{-# INLINABLE vecTakeBy #-}
vecTakeBy
  :: (ElemConstraints UnboxConstraint a)
  => Int
  -> Int
  -> U.Vector a
  -> [U.Vector a]
vecTakeBy rows cols vs =
  map (\r -> {-U.unsafeSlice-} U.slice (r *! cols) cols vs) [0..rows -! 1]
