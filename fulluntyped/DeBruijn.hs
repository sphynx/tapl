{-
Convert terms to nameless de Bruijn representation when we used indices
instead of variables names.
-}

{-# OPTIONS_GHC -Wall #-}

module DeBruijn (
  removeNames,
  restoreNames,
  shift,
  substitute,
  substituteTop
  ) where

import Data.List
import Data.Char

import Control.Monad.Reader
import Control.Applicative

import Types

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
    Abs name t1 -> Abs name <$> local (name:) (removeNamesM t1)

restoreNamesM :: NamelessTerm -> Env Term
restoreNamesM t =
  case t of
    Var idx -> Var <$> asks (!! idx) -- FIXME: use safe (!!) here
    App t1 t2 -> App <$> restoreNamesM t1 <*> restoreNamesM t2
    Abs n t1 -> do
      name <- asks $ nextName n
      subt <- local (name:) (restoreNamesM t1)
      return $ Abs name subt

--
-- shifts indices by amount d, incrementing free variables indices and
-- keeping bound variables (less than "cutoff" parameter) the same.
--
shift :: Int -> NamelessTerm -> NamelessTerm
shift d = go 0 where
  go cutoff t = case t of
    Var k -> Var $ if k < cutoff then k else k + d
    Abs n t1 -> Abs n $ go (cutoff+1) t1
    App t1 t2 -> App (go cutoff t1) (go cutoff t2)

--
-- performs substitution [j |-> s] t
--
substitute :: Int -> NamelessTerm -> NamelessTerm -> NamelessTerm
substitute j s t = case t of
  Var k -> if j == k then s else Var k
  Abs n t1 -> Abs n (substitute (j + 1) (shift 1 s) t1)
  App t1 t2 -> App (substitute j s t1) (substitute j s t2)

--
-- beta-reduction substitution (\.t1) v2
-- i.e. [0 |-> v2] t1   and some additionals shifts
--
-- 0 in t1 is substituted with shifted t2 term and then the resulting
-- term is "unshifted", since the variable is "used-up", it disappears
substituteTop :: NamelessTerm -> NamelessTerm -> NamelessTerm
substituteTop v2 t1 = shift (-1) $ substitute 0 (shift 1 v2) t1

--
-- generates a new name (by adding a non-clasing numerical suffix) if
-- the given name is already present in the context, otherwise returns
-- the passed name
--
nextName :: Name -> NamingCtx -> Name
nextName name ctx
  | name `elem` ctx =
    let base = fst $ canonical name
        f n lst = let (base', no) = canonical n
                  in if base == base' then no : lst else lst
        nextNo = succ . maximum . foldr f [] $ ctx
    in base ++ show nextNo
  | otherwise = name


-- utils
parseInt :: String -> Maybe Int
parseInt str = case reads str of
  [(num, "")] -> Just num
  _ -> Nothing

canonical :: Name -> (Name, Int)
canonical name =
  let (base, no) = break isDigit name
  in case parseInt no of
    Nothing -> (name, 0)
    Just x  -> (base, x)

