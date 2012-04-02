{-
Convert terms to nameless de Bruijn representation when we used indices
instead of variables names.
-}

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

extendEnv :: Name -> Ty -> Env a -> Env a
extendEnv name ty = local (BoundName name (VarBind ty) :)

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
    VBool b -> pure $ NVBool b
    Var name -> do
      idx <- asks $ elemIndex (mkBoundName name)
      maybe (error $ "Unknown var: " ++ name) (return . NVar) idx
    App t1 t2 -> NApp <$> removeNamesM t1 <*> removeNamesM t2
    Abs name ty t1 -> NAbs name ty <$> extendEnv name ty (removeNamesM t1)

restoreNamesM :: NamelessTerm -> Env Term
restoreNamesM t =
  case t of
    NVBool b -> pure $ VBool b
    NVar idx -> Var <$> asks (\e -> getName $ e !! idx) -- FIXME: use safe (!!) here
    NApp t1 t2 -> App <$> restoreNamesM t1 <*> restoreNamesM t2
    NAbs n ty t1 -> do
      name <- asks $ nextName n
      body <- extendEnv name ty (restoreNamesM t1)
      return $ Abs name ty body

--
-- shifts indices by amount d, incrementing free variables indices and
-- keeping bound variables (less than "cutoff" parameter) the same.
--
shift :: Int -> NamelessTerm -> NamelessTerm
shift d = go 0 where
  go cutoff t = case t of
    NVBool b -> NVBool b
    NVar k -> NVar $ if k < cutoff then k else k + d
    NAbs n tp t1 -> NAbs n tp $ go (cutoff+1) t1
    NApp t1 t2 -> NApp (go cutoff t1) (go cutoff t2)

--
-- performs substitution [j |-> s] t
--
substitute :: Int -> NamelessTerm -> NamelessTerm -> NamelessTerm
substitute j s t = case t of
  NVBool b -> NVBool b
  NVar k -> if j == k then s else NVar k
  NAbs n tp t1 -> NAbs n tp (substitute (j + 1) (shift 1 s) t1)
  NApp t1 t2 -> NApp (substitute j s t1) (substitute j s t2)

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
  | mkBoundName name `elem` ctx =
    let base = fst $ canonical name
        f n lst = let (base', no) = canonical (getName n)
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

