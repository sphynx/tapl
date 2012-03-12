{-

Pretty printer which omits all redundant brackets. It takes into
account three facts:

1. Application is left-associative, that is (x y) z = x y z

2. Lambda abstraction body stretches to the right as far as
possible: \x.y z k = \x.(y z k)

3. Application binds tighter than abstraction. I.e. we need parens
in the second expression here: (\x.x x) (\x.x x)

-}

module Pretty
  ( pretty
  ) where

import Text.PrettyPrint
import Types

 -- for more convenient testcases
import Parser hiding (lambda, test)

-- pretty printing depth, a nice idea by Jón Fairbairn mentioned in
-- Oleg's interpreter, so we can print even divergent terms.
maxDepth :: Int
maxDepth = 10

pretty :: Term -> String
pretty = render . go maxDepth where
  go _ (Var x)    = text x
  go d _ | d <= 0 = ellipsis -- print ellipsis if we have reached
                             -- max depth
  go d t          = case t of
    Abs v t1 ->
      lambda <> text v <> dot <> go' t1
    App t1 t2@(App {}) ->
      go' t1 <+> parens (go' t2)
    App t1@(Abs {}) t2@(Abs {}) ->
      parens (go' t1) <+> parens (go' t2)
    App t1@(Abs {}) t2 ->
      parens (go' t1) <+> go' t2
    App t1 t2 ->
      go' t1 <+> go' t2

    where
      go' term = go (d-1) term

-- helper stuff

lambda :: Doc
lambda = char '\\'

dot :: Doc
dot = char '.'

ellipsis :: Doc
ellipsis = space <> text "..." <> space


-- Test suite.

-- returns a list of failures: (actual, expected)
tests :: [(String, String)]
tests = map (\(r,a,e) -> (a,e)) .
        filter (\(r,_,_) -> not r) .
        map (uncurry check) $
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
    check inp exp = let act = pretty (parseExpr inp)
                    in (exp == act, act, exp)
