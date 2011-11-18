module Parser where

import Prelude hiding (lex)
import Lexer
import Syntax
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T

-- Based on:
--   https://github.com/brownplt/webbits/blob/master/src/BrownPLT/JavaScript/Parser.hs

-- Root says NewUser Arjun <: Root.
-- Root says for Arjun Allow(*).


user = do
  name <- identifier
  return (User name)

flow = do
  reservedOp "*"
  return (Flow "*")

prin = user <|> flow

newUser = do
  reserved "NewUser"
  newPrin <- prin
  reservedOp "<:"
  parentPrin <- prin
  return (NewUser newPrin parentPrin)

number = do
  n <- T.integer lex
  return (Number n)

reservation = do
  reserved "reservation"
  p <- parens prin
  return (Reservation p)

op = (reservedOp "<=" >> return NumLEq) <|> (reservedOp "<" >> return NumLT)
  <|> (reservedOp "==" >> return NumEq)


numExpr = number <|> reservation

numPred = do
  e1 <- numExpr
  o <- op
  e2 <- numExpr
  return (NumPred e1 o e2)


boolExpr = numPred


boolStmt = do
  reserved "mask"
  p <- prin
  e <- boolExpr
  return (Stmt p e)
  
parseStmt :: CharParser st (Prin, Stmt)
parseStmt = do
  speaker <- prin
  reserved "says"
  stmt <- newUser <|> boolStmt
  dot
  return (speaker, stmt)

parseStmts = do
  ss <- many parseStmt
  eof
  return ss

parseFromFile :: String -> IO [(Prin, Stmt)]
parseFromFile filename = do
  str <- readFile filename
  case parse parseStmts filename str of
    Left err -> fail (show err)
    Right stmts -> return stmts
