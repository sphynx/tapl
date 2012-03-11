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

atomicExpr :: Parser Term
atomicExpr =
  parens expr
  <|> variable
  <|> lambda

expr :: Parser Term
expr = foldl1 App <$> many1 atomicExpr

parseExpr :: String -> Term
parseExpr t =
  case parse (allOf expr) "" t of
    Left err -> error (show err)
    Right ast -> ast


