{-
To run test cases we need test-framework package easily installed from
Hackage.

More details here:
http://batterseapower.github.com/test-framework/

To run tests try `make test`.

-}

{-# LANGUAGE FlexibleInstances #-}

module Tests where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.SmallCheck

import Test.HUnit

import Test.SmallCheck
import Test.SmallCheck.Series

import Pretty
import Parser
import Types
import Interpreter hiding (main)

main :: IO ()
main = defaultMain tests

-- TODO: add tests for typed
tests = []

