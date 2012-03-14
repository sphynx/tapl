module Types where

import Data.Set ((\\), singleton, union, Set)

data Expr a =
  Var a
  | Abs a (Expr a)
  | App (Expr a) (Expr a)
  deriving (Show, Eq)

data NamelessTerm =
  NVar Int
   -- we keep names in String here to restore them back later
  | NAbs String NamelessTerm
  | NApp NamelessTerm NamelessTerm
  deriving (Show, Eq)

type Name = String
type Term = Expr Name

type NamingCtx = [Name]

freeVars :: Term -> Set Name
freeVars (Var v) = singleton v
freeVars (Abs v t) = freeVars t \\ singleton v
freeVars (App t1 t2) = freeVars t1 `union` freeVars t2

