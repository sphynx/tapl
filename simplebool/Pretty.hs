{-

Pretty printer which omits all redundant brackets. It takes into
account three facts:

1. Application is left-associative, that is (x y) z = x y z

2. Lambda abstraction body stretches to the right as far as
possible: \x.y z k = \x.(y z k)

-}

module Pretty
  ( pretty
  ) where

import Text.PrettyPrint
import Types

-- pretty printing depth, a nice idea by Jón Fairbairn mentioned in
-- Oleg's interpreter, so we can print even divergent terms.
maxDepth :: Int
maxDepth = 10

pretty :: Term -> String
pretty = render . go maxDepth where
  go _ (Var x)    = text x
  go _ (VBool True) = text "true"
  go _ (VBool False) = text "false"
  go d _ | d <= 0 = ellipsis -- print ellipsis if we have reached
                             -- max depth

  -- TODO: rewrite this (it's not tolerant to changes to Expr)
  go d t          = case t of
    Abs v tp t1 ->
      lambda <> text v <> colon <> ppType tp <> dot <> go' t1
    App t1@(Abs {}) t2@(Var {}) ->  --- abs var
      parens (go' t1) <+> go' t2
    App t1@(Abs {}) t2 -> -- abs _
      parens (go' t1) <+> parens (go' t2)
    App t1 t2@(App {}) -> -- _ app
      go' t1 <+> parens (go' t2)
    App t1 t2@(Abs {}) -> --- var abs
      go' t1 <+> parens (go' t2)
    App t1 t2 -> --- _ _
      go' t1 <+> go' t2
    _ ->
      -- this is here to please "-Wall"
      error "should not happen, since it is handled earlier"

    where
      go' = go (d-1)

ppType :: Ty -> Doc
ppType TyBool = text "bool"
ppType (TyArrow t1@(TyArrow {}) t2) =
  parens (ppType t1) <+> arrow <+> ppType t2
ppType (TyArrow t1 t2) =
  ppType t1 <+> arrow <+> ppType t2

-- helper stuff
lambda, dot, ellipsis, arrow :: Doc
lambda = char '\\'
dot = char '.'
arrow = text "->"
ellipsis = space <> text "..." <> space
