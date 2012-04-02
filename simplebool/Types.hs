module Types where

import Data.Set ((\\), singleton, union, empty, Set)

data Expr a =
  Var a
  | Abs a Ty (Expr a)
  | App (Expr a) (Expr a)
  | VBool Bool
  deriving (Show, Eq)

data NamelessTerm =
  NVar Int
   -- we keep names in String here to restore them back later
  | NAbs String Ty NamelessTerm
  | NApp NamelessTerm NamelessTerm
  | NVBool Bool
  deriving (Show, Eq)

type Name = String
type Term = Expr Name

type NamingCtx = [BoundName]

data BoundName = BoundName
  { getName    :: Name
  , getBinding :: Binding
  } deriving (Eq, Ord, Show)

data Binding =
    NameBind
  | VarBind Ty
  deriving (Eq, Ord, Show)

data Ty =
    TyBool
  | TyArrow Ty Ty
  deriving (Eq, Ord, Show)

mkBoundName :: Name -> BoundName
mkBoundName = flip BoundName NameBind

freeVars :: Term -> Set Name
freeVars (Var v) = singleton v
freeVars (Abs v _ t) = freeVars t \\ singleton v
freeVars (App t1 t2) = freeVars t1 `union` freeVars t2
freeVars (VBool _) = empty

