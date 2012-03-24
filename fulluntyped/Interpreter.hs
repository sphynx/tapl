module Interpreter where

import Types
import Parser
import DeBruijn
import Pretty

import Control.Applicative
import Control.Monad

import qualified Data.Set as Set

type Strategy = NamelessTerm -> NamelessTerm

-- REPL
parseRepl :: IO ()
parseRepl = forever (getLine >>= print . parseExpr)

evalRepl :: IO ()
evalRepl = forever $ do
  str <- getLine
  let e = parseExpr str
      small = evalNamedTermSmall e
      big = evalNamedTermBig e
  putStrLn $ "Small-step eval: " ++ small
  putStrLn $ "Big-step eval:   " ++ big

maxSteps :: Int
maxSteps = 20

-- applies one-step evaluation until no more rules can be applied
-- or max steps number is reached (to cope with divergent terms)
evalSmall :: NamelessTerm -> NamelessTerm
evalSmall = go maxSteps where

  go :: Int -> NamelessTerm -> NamelessTerm
  go steps t
    | steps <= 0 = t
    | otherwise =
      case evalOneStep t of
        Nothing -> t
        Just t' -> go (steps - 1) t'

  -- one step of evaluation (as per TAPL).
  -- Nothing means "no rules can be applied".
  evalOneStep :: NamelessTerm -> Maybe NamelessTerm
  evalOneStep term = case term of
    NApp (NAbs _ t1) v2
      | isValue v2 -> pure $ substituteTop v2 t1
    NApp v1 t2
      | isValue v1 -> NApp <$> pure v1 <*> evalOneStep t2
    NApp t1 t2 ->
      NApp <$> evalOneStep t1 <*> pure t2
    _ ->
      Nothing

-- big-step semantics
evalBig :: NamelessTerm -> NamelessTerm
evalBig = go maxSteps where

  go steps t
    | steps <= 0 = t
    | otherwise = case t of
      lam@(NAbs {}) -> lam
      app@(NApp t1 t2) ->
        let v1 = go steps t1
            v2 = go steps t2
        in if isValue v2
           then case v1 of
             NAbs _ t12 -> go (steps - 1) (substituteTop v2 t12)
             _ -> app
           else
             app
      t' -> t'

run :: String -> String
run = evalNamedTermSmall . parseExpr

evalWithStrategy :: Term -> Strategy -> String
evalWithStrategy t strategy =
  let ctx = map mkBoundName $ Set.toList $ freeVars t
      namelessTerm = removeNames ctx t
  in pretty . restoreNames ctx . strategy $ namelessTerm

evalNamedTermSmall :: Term -> String
evalNamedTermSmall t = evalWithStrategy t evalSmall

evalNamedTermBig :: Term -> String
evalNamedTermBig t = evalWithStrategy t evalBig

-- whether term is a value
isValue :: NamelessTerm -> Bool
isValue (NAbs {}) = True
isValue _ = False

main :: IO ()
main = evalRepl
