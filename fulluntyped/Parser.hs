module Parser where

import Control.Applicative hiding ((<|>))
import Control.Monad

import Text.Parsec
import Text.Parsec.String (Parser)

import Lexer
import Types

variable :: Parser Term
variable = Var <$> identifier

lambda :: Parser Term
lambda = Abs <$> (reservedOp "\\" *> identifier)
             <*> (reservedOp "." *> expr)

-- \s -> \z -> s (s (s ... (s z)) ... )
number :: Parser Term
number = do
  n <- natural
  let
    ss = replicate (fromIntegral n) (Var "s")
    encoded = foldr App (Var "z") ss
  return $ Abs "s" (Abs "z" encoded)

bool :: Parser Term
bool = tru <|> fls <|> test

-- tru = \x y -> x
tru :: Parser Term
tru = do
  reserved "true"
  return $ "x" `Abs` ("y" `Abs` Var "x")

-- fls = \x y -> y
fls :: Parser Term
fls = do
  reserved "false"
  return $ "x" `Abs` ("y" `Abs` Var "y")

-- test = \c t e -> c t e
test :: Parser Term
test = do
  reserved "test"
  let body = Var "c" `App` Var "t" `App` Var "e"
  return $ "c" `Abs` ("t" `Abs` ("e" `Abs` body))

atomicExpr :: Parser Term
atomicExpr =
  parens expr
  <|> variable
  <|> lambda
  <|> number
  <|> bool

expr :: Parser Term
expr = foldl1 App <$> many1 atomicExpr

parseExpr :: String -> Term
parseExpr t =
  case parse (allOf expr) "" t of
    Left err -> error (show err)
    Right ast -> ast


