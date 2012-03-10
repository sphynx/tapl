module Arith where

import Text.Parsec
import Text.Parsec.String
import Prelude hiding (succ, pred)
import Control.Applicative hiding ((<|>))
import qualified Data.Set as Set

data Term =
    Zero
  | T
  | F
  | Succ Term
  | Pred Term
  | If Term Term Term
  | IsZero Term
  deriving (Show, Eq, Ord)

data Value =
  BValue Bool
  | IValue Int
  deriving (Show)

expr :: Parser Term
expr = (t <|> f <|> zero <|> succ <|> pred <|> try isZero <|> iff)
       <* spaces

t, f, zero, succ, pred, iff, isZero :: Parser Term
t = T <$ string "true"
f = F <$ string "false"
zero = Zero <$ string "zero"
succ = Succ <$> (string "succ" *> spaces *> expr)
pred = Pred <$> (string "pred" *> spaces *> expr)
isZero = IsZero <$> (string "iszero" *> spaces *> expr)
iff = If <$> (string "if" *> spaces *> expr)
         <*> (string "then" *> spaces *> expr)
         <*> (string "else" *> spaces *> expr)

parseTerm :: String -> Term
parseTerm s = case parse expr "" s of
  Left e -> error $ show e
  Right r -> r

isNumVal :: Term -> Bool
isNumVal Zero = True
isNumVal (Succ t) = isNumVal t
isNumVal _ = False

isVal :: Term -> Bool
isVal T = True
isVal F = True
isVal t
  | isNumVal t = True
  | otherwise  = False

ev :: Term -> Term
ev (If T t2 _) = t2
ev (If F _ t3) = t3
ev (If t1 t2 t3) = If (ev t1) t2 t3
ev (Succ t) = Succ (ev t)
ev (Pred Zero) = Zero
ev (Pred (Succ nv)) | isNumVal nv = nv
ev (Pred t) = Pred (ev t)
ev (IsZero Zero) = T
ev (IsZero (Succ _)) = F
ev (IsZero t) = IsZero (ev t)
ev t
  | isVal t = t
  | otherwise = error $ "Cannot evaluate term: " ++ show t


untilRepeats xs = fst $ head $ dropWhile (uncurry (/=)) $ zip xs (tail xs)

eval2 :: Term -> Term
eval2 = untilRepeats . iterate ev

eval :: Term -> Value
eval Zero = IValue 0
eval (Succ e) = let IValue x = eval e in IValue (x + 1)
eval (Pred e) = let IValue x = eval e in IValue (x - 1)
eval T = BValue True
eval F = BValue False
eval (IsZero e) = case eval e of
  IValue 0 -> BValue True
  _ -> BValue False
eval (If c t e) = case eval c of
  BValue True -> eval t
  BValue False -> eval e

consts :: Term -> Set.Set Term
consts Zero = Set.singleton Zero
consts T = Set.singleton T
consts F = Set.singleton F
consts (Succ e) = consts e
consts (Pred e) = consts e
consts (IsZero e) = consts e
consts (If c t e) = consts c `Set.union` consts t `Set.union` consts e

size :: Term -> Int
size Zero = 1
size T = 1
size F = 1
size (Succ e) = 1 + size e
size (Pred e) = 1 + size e
size (IsZero e) = 1 + size e
size (If c t e) = 1 + size c + size t + size e

depth :: Term -> Int
depth Zero = 1
depth T = 1
depth F = 1
depth (Succ e) = 1 + size e
depth (Pred e) = 1 + size e
depth (IsZero e) = 1 + size e
depth (If c t e) = 1 + maximum [size c, size t, size e]



-- we need to prove that Set.size (consts e) <= size e

{-

Let's prove this by induction on depth

Assume that P(t) holds on all t with depth d-1
then prove it holds on depth d as well.

P(Zero, T, F): 1 <= 1 - true

P(Succ t):

Consts(Succ t) =bydef= Consts t <= (by induction hypothesis) <= size t < size
(Succ t)

Pred, IsZero .. the same

Consts(If c t e) =bydef= Union (consts c, consts t, const e) <=
size(const c) + size(consts t) + size(consts e) <= size c + size t + size e <= size (If ...)


-}
