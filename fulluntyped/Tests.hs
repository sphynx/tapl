{-
To run test cases we need test-framework package easily installed from
Hackage.

More details here:
http://batterseapower.github.com/test-framework/

To run tests try `make test`.

-}

module Tests where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit

import Pretty
import Parser

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "Pretty printing tests" prettyTests
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
