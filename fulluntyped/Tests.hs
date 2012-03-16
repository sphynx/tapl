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

tests =
  [ testGroup "Pretty printing unit tests" prettyTests,
    testGroup "Properties" smallCheckTests
  ]

prettyTests = map (uncurry mkTestCase)
  [ identical "x"
  , identical "x y"
  , identical "x y z"
  , identical "x (y z) t"
  , identical "\\x.x"
  , identical "\\x.\\y.x y"
  , identical "(\\x.x x) (\\x.x x)"
  , identical "(\\x.x x) (\\z.z)"
  , identical "(\\x.x x) t"
  , identical "\\x.\\y.\\z.x y z"
  , identical "\\x.x (y z)"
  , identical "\\x.x (y z) t"
  , identical "(\\x.x) y"
  , identical "(\\x.x y) z"
  , identical "x (\\y.y)"
  , identical "x y (\\y.y)"
  , identical "x ((\\x.x) (x x))"

  , ("(x)", "x")
  , ("(x y)", "x y")
  , ("(((x y)))", "x y")
  , ("(x (y) z)", "x y z")
  , ("((x y) z)", "x y z")
  , ("((x y) z) t", "x y z t")
  , ("(b k) ((x y) z) t", "b k (x y z) t")
  , ("\\x.(x y z)", "\\x.x y z")
  , ("\\x.(x y) z", "\\x.x y z")
  , ("(\\x.x x) (t)", "(\\x.x x) t")
  ]
  where
    identical x = (x, x)
    mkTestCase inp exp =
      testCase ("pretty-printing " ++ inp) $
      pretty (parseExpr inp) @?= exp


newtype VarName = VarName Name

var :: VarName -> Term
var (VarName v) = Var v

abstr :: VarName -> Term -> Term
abstr (VarName v) = Abs v

instance Serial VarName where
  series = const [VarName [c] | c <- "xy"]
  coseries = undefined

instance Serial (Expr Name) where
  series =    cons1 var
           \/ cons2 abstr
           \/ cons2 App
  coseries = undefined

smallCheckTests = [
  withDepth 4 $ testProperty "(parse . pretty) should be id"
              $ (\t -> parseExpr (pretty t) == t )

  ]
