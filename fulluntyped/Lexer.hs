module Lexer where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser $ Lang.emptyDef
  { -- for lambda abstractions
    Tok.reservedOpNames = ["\\", "."]
  , Tok.reservedNames = ["true", "false", "test"]
    -- to allow "\x.\y" without ".\" treated as a new operator
  , Tok.opLetter = oneOf "."
  }

-- here, we re-export a couple of useful token-based parsers
parens :: Parser a -> Parser a
parens = Tok.parens lexer

natural :: Parser Integer
natural = Tok.natural lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

allOf :: Parser a -> Parser a
allOf p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r
