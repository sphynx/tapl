module Parser where

import Control.Applicative hiding ((<|>))
import Control.Monad

import Text.Parsec
import Text.Parsec.String (Parser)

import Lexer

type Name = String

-- Expr here is parametrized.
-- 1) "a = String" for named terms
-- 2) "a = Int" for nameless, de Bruijn-style terms.
data Expr a =
  Var a
  | Abs String (Expr a)
  | App (Expr a) (Expr a)
  deriving Show

type Term = Expr Name

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


