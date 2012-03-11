module Pretty where

import Types

pretty :: Term -> String
pretty (Var x) = x
pretty (Abs v t1) = "(\\" ++ v ++ "." ++ pretty t1 ++ ")"
pretty (App t1 t2) = "(" ++ pretty t1 ++ " " ++ pretty t2 ++ ")"

prettyNameless :: NamelessTerm -> String
prettyNameless (Var k) = show k
prettyNameless (Abs v t1) = "(\\." ++ prettyNameless t1 ++ ")"
prettyNameless (App t1 t2) = "(" ++ prettyNameless t1 ++ " " ++ prettyNameless t2 ++ ")"
