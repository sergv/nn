----------------------------------------------------------------------------
-- |
-- Module      :  TestUtils
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 30 September 2015
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module TestUtils where

import Data.Function
import Data.Proxy
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Test.Tasty.HUnit (Assertion, (@?=))

import Data.Aligned (Aligned)
import Data.Aligned.Double (AlignedDouble)
import Data.Aligned.Float (AlignedFloat)
import Data.ConstrainedFunctor
import Data.MatrixClass (Matrix)
import qualified Data.MatrixClass as MC
import Data.Nonlinearity
import Data.VectClass (Vect)
import qualified Data.VectClass as VC

newtype ApproxRelEqFloating a = ApproxRelEqFloating a
  deriving (Show, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat, S.Storable, Aligned)

deriving instance (U.Unbox a, GM.MVector UM.MVector a, G.Vector U.Vector a) => U.Unbox (ApproxRelEqFloating a)

newtype instance UM.MVector s (ApproxRelEqFloating a) = MV_ApproxRelEqFloating (UM.MVector s a)
newtype instance U.Vector     (ApproxRelEqFloating a) = V_ApproxRelEqFloating (U.Vector a)

instance (GM.MVector UM.MVector a) => GM.MVector UM.MVector (ApproxRelEqFloating a) where
  {-# INLINE basicLength      #-}
  basicLength (MV_ApproxRelEqFloating v) = GM.basicLength v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice n m (MV_ApproxRelEqFloating v) = MV_ApproxRelEqFloating $ GM.basicUnsafeSlice n m v
  {-# INLINE basicOverlaps    #-}
  basicOverlaps (MV_ApproxRelEqFloating v) (MV_ApproxRelEqFloating v') = GM.basicOverlaps v v'
  {-# INLINE basicUnsafeNew   #-}
  basicUnsafeNew n = MV_ApproxRelEqFloating <$> GM.basicUnsafeNew n
  {-# INLINE basicInitialize  #-}
  basicInitialize (MV_ApproxRelEqFloating v) = GM.basicInitialize v
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeRead (MV_ApproxRelEqFloating v) n = ApproxRelEqFloating <$> GM.basicUnsafeRead v n
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_ApproxRelEqFloating v) n (ApproxRelEqFloating x) = GM.basicUnsafeWrite v n x

instance (GM.MVector UM.MVector a, G.Vector U.Vector a) => G.Vector U.Vector (ApproxRelEqFloating a) where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_ApproxRelEqFloating v) = V_ApproxRelEqFloating <$> G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeThaw   #-}
  basicUnsafeThaw (V_ApproxRelEqFloating v) = MV_ApproxRelEqFloating <$> G.basicUnsafeThaw v
  {-# INLINE basicLength       #-}
  basicLength (V_ApproxRelEqFloating v) = G.basicLength v
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice n m (V_ApproxRelEqFloating v) = V_ApproxRelEqFloating $ G.basicUnsafeSlice n m v
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_ApproxRelEqFloating v) n = ApproxRelEqFloating <$> G.basicUnsafeIndexM v n

instance (Ord a, Floating a) => Eq (ApproxRelEqFloating a) where
  (==) (ApproxRelEqFloating x) (ApproxRelEqFloating y) =
    abs relDiff <= 1e-6
    where
      mn      = min x y
      mx      = max x y
      avg     = mn + (mx - mn) / 2
      relDiff = (mx - mn) / avg

doubleProxy :: Proxy Double
doubleProxy = Proxy

alignedDoubleProxy :: Proxy AlignedDouble
alignedDoubleProxy = Proxy

alignedFloatProxy :: Proxy AlignedFloat
alignedFloatProxy = Proxy

expProxy :: Proxy Exp
expProxy = Proxy

ivec :: (ElemConstraints v a, Vect v) => [a] -> v a
ivec = VC.fromList

imat :: (ElemConstraints w a, Matrix w v, Show a) => [[a]] -> w a
imat = MC.fromList

-- | Do approximate comparison of two ConstrainedFunctors
(@?~)
  :: (ConstrainedFunctor v, ElemConstraints v a, ElemConstraints v (ApproxRelEqFloating a))
  => (Eq (v (ApproxRelEqFloating a)), Show (v (ApproxRelEqFloating a)))
  => v a -- ^ The actual value
  -> v a -- ^ The expected value
  -> Assertion
(@?~) = (@?=) `on` cfmap ApproxRelEqFloating
