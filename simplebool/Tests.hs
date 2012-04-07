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
tests =
  [ testGroup "pretty . parse = identity" parsingPrettyTests
  ]

parsingPrettyTests = map (uncurry mkTestCase)
  [ identical "x"
  , identical "x y"
  , identical "x y z"
  , identical "x (y z) t"
  , identical "\\x:Bool.x"
  , identical "\\x:Bool.\\y:Bool.x y"
  , identical "(\\x:Bool.x x) (\\x:Bool.x x)"
  , identical "(\\x:Bool.x x) (\\z:Bool.z)"
  , identical "(\\x:Bool.x x) t"
  , identical "\\x:Bool.\\y:Bool.\\z:Bool.x y z"
  , identical "\\x:Bool.x (y z)"
  , identical "\\x:Bool.x (y z) t"
  , identical "(\\x:Bool.x) y"
  , identical "(\\x:Bool.x y) z"
  , identical "x (\\y:Bool.y)"
  , identical "x y (\\y:Bool.y)"
  , identical "x ((\\x:Bool.x) (x x))"

  , identical "\\f:Bool -> Bool.f"
  , identical "\\f:Bool -> Bool.\\x:Bool.f x"

  , ("(x)", "x")
  , ("(x y)", "x y")
  , ("(((x y)))", "x y")
  , ("(x (y) z)", "x y z")
  , ("((x y) z)", "x y z")
  , ("((x y) z) t", "x y z t")
  , ("(b k) ((x y) z) t", "b k (x y z) t")
  , ("\\x:Bool.(x y z)", "\\x:Bool.x y z")
  , ("\\x:Bool.(x y) z", "\\x:Bool.x y z")
  , ("(\\x:Bool.x x) (t)", "(\\x:Bool.x x) t")
  ]
  where
    identical x = (x, x)
    mkTestCase inp exp =
      testCase ("pretty-printing " ++ inp) $
      pretty (parseExpr inp) @?= exp

mkTestCase inp exp =
  testCase ("pretty-printing " ++ inp) $
      pretty (parseExpr inp) @?= exp
