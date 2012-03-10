module Interpreter where

import Parser
import DeBruijn

import Control.Monad

parseRepl :: IO ()
parseRepl = forever (getLine >>= print . parseExpr)

