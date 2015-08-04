----------------------------------------------------------------------------
-- |
-- Module      :  Data.MatrixDouble
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

module Data.MatrixDouble (MatrixDouble) where

import Prelude hiding (zipWith, zipWith3)
import Control.DeepSeq
import Data.Monoid
import qualified Data.List as L
import qualified Data.Vector.Unboxed as U
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.ConstrainedFunctor
import Data.VectorDouble (VectorDouble)
import qualified Data.VectorDouble as VD
import Data.MatrixClass
import qualified Data.VectClass as VC
import Data.Zippable
import Util

data MatrixDouble a = MatrixDouble
  { mdRows    :: {-# UNPACK #-} !Int
  , mdColumns :: {-# UNPACK #-} !Int
  , mdData    :: {-# UNPACK #-} !(VectorDouble a)
  }

{-# INLINABLE takeBy #-}
takeBy :: Int -> [a] -> [[a]]
takeBy _ [] = []
takeBy n xs = ys : takeBy n zs
  where
    (ys, zs) = splitAt n xs

unboxedMatrixToList :: (ElemConstraints IsDoubleConstraint a) => MatrixDouble a -> [[a]]
unboxedMatrixToList (MatrixDouble _ cols xs) = takeBy cols $ VC.toList xs

instance (ElemConstraints IsDoubleConstraint a) => Pretty (MatrixDouble a) where
  pretty md@(MatrixDouble rows cols _) =
    "Matrix " <> PP.int rows <> "x" <> PP.int cols <> " (double)" PP.<$>
    PP.vsep (L.map showRow $ unboxedMatrixToList md)
    where
      showRow :: [Double] -> Doc
      showRow = PP.hcat . PP.punctuate PP.comma . L.map pretty

instance (ElemConstraints IsDoubleConstraint a) => NFData (MatrixDouble a) where
  rnf (MatrixDouble rows cols xs) = rnf rows `seq` rnf cols `seq` rnf xs

instance ConstrainedFunctor IsDoubleConstraint MatrixDouble where
  {-# INLINABLE cfmap #-}
  cfmap f md = md { mdData = cfmap f $ mdData md }

instance Zippable IsDoubleConstraint MatrixDouble where
  {-# INLINABLE zipWith  #-}
  {-# INLINABLE zipWith3 #-}
  {-# INLINABLE zipWith4 #-}
  zipWith f (MatrixDouble xRows xCols xs) (MatrixDouble yRows yCols ys)
    | xRows == yRows && xCols == yCols =
      MatrixDouble xRows xCols $ zipWith f xs ys
    | otherwise = error "MatrixDouble.zipWith: cannot zip matrices of different shapes"
  zipWith3 f (MatrixDouble xRows xCols xs) (MatrixDouble yRows yCols ys) (MatrixDouble zRows zCols zs)
    | xRows == yRows && yRows == zRows && xCols == yCols && yCols == zCols =
      MatrixDouble xRows xCols $ zipWith3 f xs ys zs
    | otherwise = error "MatrixDouble.zipWith3: cannot zip matrices of different shapes"
  zipWith4 f (MatrixDouble xRows xCols xs) (MatrixDouble yRows yCols ys) (MatrixDouble zRows zCols zs) (MatrixDouble wRows wCols ws)
    | xRows == yRows && yRows == zRows && zRows == wRows && xCols == yCols && yCols == zCols && zCols == wCols =
      MatrixDouble xRows xCols $ zipWith4 f xs ys zs ws
    | otherwise = error "MatrixDouble.zipWith4: cannot zip matrices of different shapes"

instance Matrix IsDoubleConstraint MatrixDouble VectorDouble where
  {-# INLINABLE rows         #-}
  {-# INLINABLE columns      #-}
  {-# INLINABLE outerProduct #-}
  {-# INLINABLE vecMulRight  #-}
  {-# INLINABLE transpose    #-}
  fromList [] =
    error "MatrixDouble.fromList: cannot create PureMatrix from empty list of rows"
  fromList wss@(ws:_)
    | columns > 0 && all (== columns) (L.map length wss) =
      MatrixDouble
        { mdRows    = rows
        , mdColumns = columns
        , mdData    = VC.fromList $ L.concat wss
        }
    | otherwise =
      error $ "MatrixDouble.fromList: cannot create PureMatrix from list " ++ show wss
    where
      rows    = length wss
      columns = length ws
  toList (MatrixDouble _ cols xs) = takeBy cols $ VC.toList xs
  rows    = mdRows
  columns = mdColumns
  replicateM rows cols action =
    MatrixDouble rows cols <$> VC.replicateM (rows *! cols) action
  outerProduct columnVec rowVec =
    MatrixDouble
       { mdRows    = VC.length columnVec
       , mdColumns = VC.length rowVec
       , mdData    = VD.concatMap (\c -> cfmap (c *!) rowVec) columnVec
       }
  vecMulRight (MatrixDouble rows cols xs) ys =
    VD.fromList $ L.map (\zs -> VC.dot zs ys) $ VD.takeBy rows cols xs
  transpose (MatrixDouble rows cols xs) =
    MatrixDouble cols rows xs'
    where
      xs' = VD.unsafeBackpermute xs $ U.fromList
              [ c + cols * r
              | c <- [0..cols - 1]
              , r <- [0..rows - 1]
              ]
