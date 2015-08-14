----------------------------------------------------------------------------
-- |
-- Module      :  Data.V3
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.V3 where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable

data V3 a =
  V3
    {- # UNPACK #-} !a
    {- # UNPACK #-} !a
    {- # UNPACK #-} !a
  deriving (Show, Eq, Ord)

newtype instance U.MVector s (V3 a) =
  MV_StrictDoubleTriple (U.MVector s (a, a, a))
newtype instance U.Vector    (V3 a) =
  V_StrictDoubleTriple  (U.Vector    (a, a, a))

instance (Unbox a) => GM.MVector U.MVector (V3 a) where
  {-# INLINE basicLength          #-}
  {-# INLINE basicUnsafeSlice     #-}
  {-# INLINE basicOverlaps        #-}
  {-# INLINE basicUnsafeNew       #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead      #-}
  {-# INLINE basicUnsafeWrite     #-}
  {-# INLINE basicClear           #-}
  {-# INLINE basicSet             #-}
  {-# INLINE basicUnsafeCopy      #-}
  {-# INLINE basicUnsafeMove      #-}
  {-# INLINE basicUnsafeGrow      #-}
  basicLength (MV_StrictDoubleTriple v) = GM.basicLength v
  basicUnsafeSlice x y (MV_StrictDoubleTriple v) =
    MV_StrictDoubleTriple $ GM.basicUnsafeSlice x y v
  basicOverlaps (MV_StrictDoubleTriple v) (MV_StrictDoubleTriple v') =
    GM.basicOverlaps v v'
  basicUnsafeNew n = MV_StrictDoubleTriple <$> GM.basicUnsafeNew n
  basicInitialize (MV_StrictDoubleTriple xs) = GM.basicInitialize xs
  basicUnsafeReplicate n (V3 x y z) =
    MV_StrictDoubleTriple <$> GM.basicUnsafeReplicate n (x, y, z)
  basicUnsafeRead (MV_StrictDoubleTriple v) n =
    (\(x, y, z) -> V3 x y z) <$> GM.basicUnsafeRead v n
  basicUnsafeWrite (MV_StrictDoubleTriple v) n (V3 x y z) =
    GM.basicUnsafeWrite v n (x, y, z)
  basicClear (MV_StrictDoubleTriple v) = GM.basicClear v
  basicSet (MV_StrictDoubleTriple v) (V3 x y z) = GM.basicSet v (x, y, z)
  basicUnsafeCopy (MV_StrictDoubleTriple target) (MV_StrictDoubleTriple source) =
    GM.basicUnsafeCopy target source
  basicUnsafeMove (MV_StrictDoubleTriple target) (MV_StrictDoubleTriple source) =
    GM.basicUnsafeMove target source
  basicUnsafeGrow (MV_StrictDoubleTriple target) n =
    MV_StrictDoubleTriple <$> GM.basicUnsafeGrow target n

instance (Unbox a) => G.Vector U.Vector (V3 a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy   #-}
  {-# INLINE elemseq           #-}
  basicUnsafeFreeze (MV_StrictDoubleTriple v) =
    V_StrictDoubleTriple <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_StrictDoubleTriple v) =
    MV_StrictDoubleTriple <$> G.basicUnsafeThaw v
  basicLength (V_StrictDoubleTriple v) = G.basicLength v
  basicUnsafeSlice n m (V_StrictDoubleTriple v) =
    V_StrictDoubleTriple $ G.basicUnsafeSlice n m v
  basicUnsafeIndexM (V_StrictDoubleTriple v) n =
    (\(x, y, z) -> V3 x y z) <$> G.basicUnsafeIndexM v n
  basicUnsafeCopy (MV_StrictDoubleTriple v) (V_StrictDoubleTriple v') =
    G.basicUnsafeCopy v v'
  elemseq (V_StrictDoubleTriple v) (V3 x y z) w =
    G.elemseq v (x, y, z) w

instance (Unbox a) => Unbox (V3 a)

instance forall a. (Storable a) => Storable (V3 a) where
  sizeOf _    = 3 * sizeOf (undefined :: a)
  alignment _ = 3 * alignment (undefined :: a)
  peek ptr = do
    x <- peek (castPtr ptr)
    let ptr' = plusPtr ptr $ sizeOf x
    y <- peek ptr'
    let ptr'' = plusPtr ptr' $ sizeOf y
    z <- peek ptr''
    return $ V3 x y z
  poke ptr (V3 x y z) = do
    poke ptr' x
    poke ptr'' y
    poke ptr''' z
    where
      ptr'   = castPtr ptr
      ptr''  = plusPtr ptr' $ sizeOf x
      ptr''' = plusPtr ptr'' $ sizeOf y
