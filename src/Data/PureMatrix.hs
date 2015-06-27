----------------------------------------------------------------------------
-- |
-- Module      :  Data.PureMatrix
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.PureMatrix where

import Prelude hiding (zipWith, zipWith3, map)
import Control.DeepSeq
import qualified Data.List as L
import Data.Monoid
-- import Data.Vector (Vector)
-- import qualified Data.Vector as V
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.MatrixClass
import Data.VectClass (Vect, (.+.))
import qualified Data.VectClass as VC

import Unboxed.Functor
import Util
import Util.Zippable


data PureMatrix v a = PureMatrix
  { pmRows :: Int
  , pmCols :: Int
  , pmData :: v (v a)
  }
  deriving (Functor, Foldable, Traversable)

deriving instance (Show (v (v a))) => Show (PureMatrix v a)
deriving instance (Eq (v (v a))) => Eq (PureMatrix v a)
deriving instance (Ord (v (v a))) => Ord (PureMatrix v a)

instance forall v a. (Vect v, Pretty a) => Pretty (PureMatrix v a) where
  pretty (PureMatrix rows cols xss) =
    "Matrix " <> PP.int rows <> "x" <> PP.int cols PP.<$>
    PP.vsep (L.map showRow $ VC.toList xss)
    where
      showRow :: v a -> Doc
      showRow = PP.hcat . PP.punctuate PP.comma . L.map pretty . VC.toList

instance (NFData (v (v a))) => NFData (PureMatrix v a) where
  rnf (PureMatrix rows cols xss) = rnf rows `seq` rnf cols `seq` rnf xss

instance (Vect v) => UnboxedFunctor (PureMatrix v) where
  ufmap f (PureMatrix rows cols xss) = PureMatrix rows cols (VC.map (VC.map f) xss)

instance (Zippable v) => Zippable (PureMatrix v) where
  zipWith f (PureMatrix xRows xCols xss) (PureMatrix yRows yCols yss)
    | xRows == yRows && xCols == yCols =
      PureMatrix xRows xCols $ zipWith (zipWith f) xss yss
    | otherwise = error "MatrixClass.zipWith: cannot zip matrices of different shapes"
  zipWith3 f (PureMatrix xRows xCols xss) (PureMatrix yRows yCols yss) (PureMatrix zRows zCols zss)
    | xRows == yRows && yRows == zRows && xCols == yCols && yCols == zCols =
      PureMatrix xRows xCols $ zipWith3 (zipWith3 f) xss yss zss
    | otherwise = error "MatrixClass.zipWith3: cannot zip matrices of different shapes"
  zipWith4 f (PureMatrix xRows xCols xss) (PureMatrix yRows yCols yss) (PureMatrix zRows zCols zss) (PureMatrix wRows wCols wss)
    | xRows == yRows && yRows == zRows && zRows == wRows && xCols == yCols && yCols == zCols && zCols == wCols =
      PureMatrix xRows xCols $ zipWith4 (zipWith4 f) xss yss zss wss
    | otherwise = error "MatrixClass.zipWith4: cannot zip matrices of different shapes"

instance (Vect v) => Matrix (PureMatrix v) v where
  map f (PureMatrix rows cols xss) = PureMatrix rows cols $ VC.map (VC.map f) xss
  rows    = pmRows
  columns = pmCols
  replicateM rows cols action =
    PureMatrix rows cols <$> VC.replicateM rows (VC.replicateM cols action)
  outerProduct columnVec rowVec =
    PureMatrix (VC.length columnVec) (VC.length rowVec) $
    VC.map (\c -> VC.map (c *!) rowVec) columnVec
  vecMulRight (PureMatrix _ _ xss) ys = VC.map (\xs -> VC.dot xs ys) xss
  vecMulLeft xs (PureMatrix _ cols yss) =
    VC.foldr (.+.) zeroVector $
    zipWith (\x ys -> VC.map (x *!) ys) xs yss
    where
      zeroVector = VC.replicate cols 0

-- instance Matrix (PureMatrix Vector) Vector where
--   map f (PureMatrix rows cols xss) = PureMatrix rows cols $ fmap (fmap f) xss
--   zipWith f (PureMatrix xss) (PureMatrix yss) = PureMatrix $ V.zipWith (V.zipWith f) xss yss

