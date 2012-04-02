module Parser
       ( parseExpr
       )  where

import Control.Applicative hiding ((<|>))

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

import Lexer
import Types

variable :: Parser Term
variable = Var <$> identifier

lambda :: Parser Term
lambda = Abs <$> (reservedOp "\\" *> identifier)
             <*> (reservedOp ":" *> anyType)
             <*> (reservedOp "." *> expr)

anyType :: Parser Ty
anyType = buildExpressionParser table atomicType where
    atomicType = parens anyType <|> boolType
    table = [[ Infix (TyArrow <$ reservedOp "->") AssocRight ]]
    boolType = TyBool <$ reserved "Bool"

bool :: Parser Term
bool = tru <|> fls

tru :: Parser Term
tru = VBool True <$ reserved "true"

fls :: Parser Term
fls = VBool False <$ reserved "false"

atomicExpr :: Parser Term
atomicExpr =
  parens expr
  <|> variable
  <|> lambda
  <|> bool

expr :: Parser Term
expr = foldl1 App <$> many1 atomicExpr

parseExpr :: String -> Term
parseExpr t =
  case parse (allOf expr) "" t of
    Left err -> error $ show err
    Right ast -> ast
