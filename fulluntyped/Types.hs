module Types where

import Data.Set ((\\), singleton, union, Set)

type Name = String

-- Expr here is parametrized.
-- 1) "a = String" for named terms
-- 2) "a = Int" for nameless, de Bruijn-style terms.
data Expr a =
  Var a
  | Abs String (Expr a)
  | App (Expr a) (Expr a)
  deriving Show

instance Functor Expr where
  fmap f (Var t) = Var (f t)
  fmap f (Abs name t) = Abs name (fmap f t)
  fmap f (App t1 t2) = App (fmap f t1) (fmap f t2)

type Term = Expr Name
type NamelessTerm = Expr Int

type NamingCtx = [Name]

freeVars :: Term -> Set Name
freeVars (Var v) = singleton v
freeVars (Abs v t) = freeVars t \\ singleton v
freeVars (App t1 t2) = freeVars t1 `union` freeVars t2

