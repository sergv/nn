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

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module VectorTests (tests) where

import Data.Vector (Vector)
import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit

import Data.AlignedStorableVector (AlignedStorableVector)
import Data.ConstrainedFunctor
import Data.Nonlinearity
import Data.SpecialisedFunction
import Data.VectClass (Vect)
import qualified Data.VectClass as VC

import TestUtils

tests :: TestTree
tests = testGroup "Vector tests"
  [ vectorTests "Data.Vector" vectorProxy doubleProxy
  , vectorTests "Data.AlignedStorableVector, Float" alignedStorableVectorProxy alignedFloatProxy
  , vectorTests "Data.AlignedStorableVector, Double" alignedStorableVectorProxy alignedDoubleProxy
  ]

vectorTests
  :: forall v a. (Vect v, ElemConstraints v a, Show (v a), Eq (v a))
  => (Show a, Eq a, Num a, Floating a)
  => (Show (v (ApproxRelEqFloating a)), Eq (v (ApproxRelEqFloating a)))
  => (ElemConstraints v (ApproxRelEqFloating a))
  => (SpecialisedFunction Exp (v a) (v a))
  => (ConstrainedFunctor v)
  => String
  -> Proxy v
  -> Proxy a
  -> TestTree
vectorTests name _ _ = testGroup name
  [ testCase "toList . fromList == id" $
    (VC.toList (VC.fromList [1, 2, 3, 4, 5] :: v a)) @?= [1, 2, 3, 4, 5]
  , testCase "sum #1" $
    VC.sum testVector @?= 15
  , testCase "sum #2" $
    VC.sum testVectorLong @?= 55
  , testCase ".+. #1" $
    testVector VC..+. testVector @?= ivec [2, 4, 6, 8, 10]
  , testCase ".+. #2" $
    testVectorLong VC..+. testVectorLong @?= ivec [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
  , testCase "addScaled #1" $
    VC.addScaled testVector 2 testVector @?= ivec [3, 6, 9, 12, 15]
  , testCase "addScaled #2" $
    VC.addScaled testVectorLong 2 testVectorLong @?= ivec [3, 6, 9, 12, 15, 18, 21, 24, 27, 30]
  , testCase "length #1" $
    VC.length testVector @?= 5
  , testCase "length #2" $
    VC.length testVectorLong @?= 10
  , testCase "dot #1" $
    VC.dot testVector testVector @?= 55
  , testCase "dot #2" $
    VC.dot testVectorLong testVectorLong @?= 385 -- (10 * (10 + 1) * (2 * 10 + 1)) `div` 6
  , testCase "exp #1" $
    sfmap expProxy testVector @?~ cfmap exp testVector
  , testCase "exp #2" $
    sfmap expProxy testVectorLong @?~ cfmap exp testVectorLong
  ]
  where
    testVector :: v a
    testVector = ivec [1, 2, 3, 4, 5]
    testVectorLong :: v a
    testVectorLong = ivec [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

vectorProxy :: Proxy Vector
vectorProxy = Proxy

alignedStorableVectorProxy :: Proxy AlignedStorableVector
alignedStorableVectorProxy = Proxy
