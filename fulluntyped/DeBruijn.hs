{-

Convert terms to nameless de Bruijn representation when we used indices instead
of variables names.

-}

module DeBruijn
       (removeNames, restoreNames, NamingCtx)
       where

import Parser
import Data.List
import Control.Monad.Reader
import Control.Applicative

type NamelessTerm = Expr Int
type NamingCtx = [Name]

type Env = Reader NamingCtx

-- conversion between names and nameless terms
removeNames :: NamingCtx -> Term -> NamelessTerm
removeNames ctx = flip runReader ctx . removeNamesM

restoreNames :: NamingCtx -> NamelessTerm -> Term
restoreNames ctx = flip runReader ctx . restoreNamesM


-- monadic remove/restore names in Reader monad
-- (naming context is teh env)
removeNamesM :: Term -> Env NamelessTerm
removeNamesM t =
  case t of
    Var name -> do
      idx <- asks $ elemIndex name
      maybe (error $ "Unknown var: " ++ name) (return . Var) idx
    App t1 t2 -> App <$> removeNamesM t1 <*> removeNamesM t2
    Abs name t1 -> Abs "" <$> local (name:) (removeNamesM t1)

restoreNamesM :: NamelessTerm -> Env Term
restoreNamesM t =
  case t of
    Var idx -> Var <$> asks (!! idx) -- FIXME: use safe (!!) here
    App t1 t2 -> App <$> restoreNamesM t1 <*> restoreNamesM t2
    Abs _ t1 -> do
      name <- asks nextName
      t <- local (name:) (restoreNamesM t1)
      return $ Abs name t

-- not a very clever name supply, but I don't bother too much here
nextName :: NamingCtx -> Name
nextName = (++ "1") . last . sort

-- default context from TAPL
defCtx :: NamingCtx
defCtx = ["b","a","z","y","x"]

