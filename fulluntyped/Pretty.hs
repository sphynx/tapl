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
import Parser

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
      go' = go (d-1)

-- helper stuff

lambda :: Doc
lambda = char '\\'

dot :: Doc
dot = char '.'

ellipsis :: Doc
ellipsis = space <> text "..." <> space


