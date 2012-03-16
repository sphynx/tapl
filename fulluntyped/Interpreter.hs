module Interpreter where

import Types
import Parser
import DeBruijn
import Pretty

import Control.Applicative
import Control.Monad

import qualified Data.Set as Set

-- REPL
parseRepl :: IO ()
parseRepl = forever (getLine >>= print . parseExpr)

evalRepl :: IO ()
evalRepl = forever (getLine >>= putStrLn . run)

-- one step of evaluation (as per TAPL).
-- Nothing means "no rules can be applied".
eval1 :: NamelessTerm -> Maybe NamelessTerm
eval1 t = case t of
  NApp (NAbs _ t1) v2
    | isValue v2 -> pure $ substituteTop v2 t1
  NApp v1 t2
    | isValue v1 -> NApp <$> pure v1 <*> eval1 t2
  NApp t1 t2 ->
    NApp <$> eval1 t1 <*> pure t2
  _ ->
    Nothing

evalBig :: NamelessTerm -> NamelessTerm
evalBig t = case t of
  lam@(NAbs {}) -> lam
  NApp t1 t2 ->
    let v2 = evalBig t2
        NAbs _ t12 = evalBig t1
    in evalBig (substituteTop v2 t12)
  t' -> t'

-- applies one-step evaluation until no more rules can be applied
eval :: NamelessTerm -> NamelessTerm
eval t =
  case eval1 t of
    Nothing -> t
    Just t' -> eval t'

run :: String -> String
run s =
  let namedTerm = parseExpr s
      ctx = Set.toList $ freeVars namedTerm
      namelessTerm = removeNames ctx namedTerm
  in pretty . restoreNames ctx . eval $ namelessTerm


-- test true x y -> x
term1 :: Term
term1 = parseExpr "test true"

nTerm1 :: NamelessTerm
nTerm1 = removeNames [] term1

-- whether term is a value
isValue :: NamelessTerm -> Bool
isValue (NAbs {}) = True
isValue _ = False

main :: IO ()
main = evalRepl
