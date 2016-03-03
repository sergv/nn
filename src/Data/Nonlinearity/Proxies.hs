----------------------------------------------------------------------------
-- |
-- Module      :  Data.Nonlinearity.Proxies
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

module Data.Nonlinearity.Proxies where

import Data.Proxy
import Text.PrettyPrint.Leijen.Text (Doc)

import Data.SpecialisedFunction

class PrettyProxy a where
  prettyProxy :: (IsProxyFor p a) => p -> Doc

newtype NonlinearityProxy nn n o a = NonlinearityProxy (nn n o a)
instance IsProxyFor (NonlinearityProxy nn n o a) n

newtype OutputProxy nn n o a = OutputProxy (nn n o a)
instance IsProxyFor (OutputProxy nn n o a) o

data family Deriv a :: *
data family FuncWithDeriv a :: *

addDerivInProxy :: (IsProxyFor p a) => p -> Proxy (Deriv a)
addDerivInProxy _ = Proxy

stripDerivInProxy :: (IsProxyFor p (Deriv a)) => p -> Proxy a
stripDerivInProxy _ = Proxy

addFuncWithDerivInProxy :: (IsProxyFor p a) => p -> Proxy (FuncWithDeriv a)
addFuncWithDerivInProxy _ = Proxy

stripFuncWithDerivInProxy :: (IsProxyFor p (FuncWithDeriv a)) => p -> Proxy a
stripFuncWithDerivInProxy _ = Proxy

