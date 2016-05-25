
----------------------------------------------------------------------------
-- |
-- Module      :  HLint
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 25 May 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module HLint where

import "hint" HLint.Default

ignore "Avoid lambda" = NN.Generic.targetFunctionGrad
ignore "Use fst" = NN.Generic.backprop
ignore "Redundant seq" = (Util.+!) (Util.-!) (Util.*!) (Util./!)
ignore "Use sqrt" = NNTests.nnTests
