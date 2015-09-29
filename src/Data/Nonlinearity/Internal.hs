----------------------------------------------------------------------------
-- |
-- Module      :  Data.Nonlinearity.Internal
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Friday, 25 September 2015
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.Nonlinearity.Internal where

import Data.Proxy
import Text.PrettyPrint.Leijen.Text (Doc)

import Data.SpecialisedFunction

class PrettyProxy a where
  prettyProxy :: (IsProxyFor p a) => p -> Doc

newtype NonlinearityProxy nn n o a = NonlinearityProxy (nn n o a)
instance IsProxyFor (NonlinearityProxy nn n o a) n

newtype OutputProxy nn n o a = OutputProxy (nn n o a)
instance IsProxyFor (OutputProxy nn n o a) o

-- data family Deriv a :: *
data family FuncWithDeriv a :: *

addFuncWithDerivInProxy :: (IsProxyFor p a) => p -> Proxy (FuncWithDeriv a)
addFuncWithDerivInProxy _ = Proxy

stripFuncWithDerivInProxy :: (IsProxyFor p (FuncWithDeriv a)) => p -> Proxy a
stripFuncWithDerivInProxy _ = Proxy

class Nonlinearity n where
  nonlinearity :: (IsProxyFor p n, Floating a) => p -> a -> a
  nonlinearityDeriv :: (IsProxyFor p n, Floating a) => p -> a -> a
