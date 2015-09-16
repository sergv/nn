----------------------------------------------------------------------------
-- |
-- Module      :  VectorTests
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module VectorTests (tests) where

import Data.Vector (Vector)
import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit

import Data.Aligned.Double (AlignedDouble)
import Data.Aligned.Float (AlignedFloat)
import Data.AlignedStorableVector (AlignedStorableVector)
import Data.ConstrainedFunctor
import Data.VectClass (Vect)
import qualified Data.VectClass as VC

tests :: TestTree
tests = testGroup "Vector tests"
  [ vectorTests "Data.Vector" vectorProxy intProxy
  , vectorTests "Data.AlignedStorableVector, Double" alignedStorableVectorProxy alignedDoubleProxy
  , vectorTests "Data.AlignedStorableVector, Float" alignedStorableVectorProxy alignedFloatProxy
  ]

vectorTests
  :: forall v a. (Vect v, ElemConstraints v a, Show (v a), Eq (v a), Show a, Eq a, Num a)
  => String
  -> Proxy v
  -> Proxy a
  -> TestTree
vectorTests name _ _ = testGroup name
  [ testCase "toList . fromList == id" $
    (VC.toList (VC.fromList [1, 2, 3, 4, 5] :: v a)) @?= [1, 2, 3, 4, 5]
  , testCase "sum" $
    VC.sum testVector @?= 15
  , testCase ".+." $
    testVector VC..+. testVector @?= ivec [2, 4, 6, 8, 10]
  , testCase "addScaled" $
    VC.addScaled testVector 2 testVector @?= ivec [3, 6, 9, 12, 15]
  , testCase "length" $
    VC.length testVector @?= 5
  , testCase "dot" $
    VC.dot testVector testVector @?= 55
  ]
  where
    testVector :: v a
    testVector = ivec [1, 2, 3, 4, 5]

intProxy :: Proxy Int
intProxy = Proxy

alignedFloatProxy :: Proxy AlignedFloat
alignedFloatProxy = Proxy

alignedDoubleProxy :: Proxy AlignedDouble
alignedDoubleProxy = Proxy

vectorProxy :: Proxy Vector
vectorProxy = Proxy

alignedStorableVectorProxy :: Proxy AlignedStorableVector
alignedStorableVectorProxy = Proxy

ivec :: (ElemConstraints v a, Vect v) => [a] -> v a
ivec = VC.fromList
