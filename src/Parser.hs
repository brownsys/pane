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

-- explicitly indicated user
expUser = do
  reserved "user"
  reservedOp "="
  name <- identifier
  return (User name)

expApp = do
  reserved "app"
  reservedOp "="
  name <- identifier
  return (App name)

{-
expNet = do
  reserved "net"
  reservedOp "="
  name <- identifier
  return (Network name)
-}

flow = do
  reservedOp "*"
  return (Flow "*")

prin = flow <|> expUser <|> expApp -- <|> expNet

newUser = do
  reserved "NewUser"
  newPrin <- user
  reservedOp "<:"
  parentPrin <- user
  return (NewUser newPrin parentPrin)

number = do
  n <- T.integer lex
  return (Number n)

reservation = do
  reserved "reservation"
  p <- parens prin
  return (Reservation p)

latency = do
  reserved "latency"
  l <- parens prin
  return (Latency l)

jitter = do
  reserved "jitter"
  j <- parens prin
  return (Jitter j)

ratelimit = do
  reserved "ratelimit"
  l <- parens prin
  return (Ratelimit l)

op = (reservedOp "<=" >> return NumLEq) <|> (reservedOp "<" >> return NumLT)
  <|> (reservedOp "=" >> return NumEq) <|> (reservedOp ">=" >> return NumGEq)
  <|> (reservedOp ">" >> return NumGT)


numExpr = number <|> reservation <|> latency <|> jitter <|> ratelimit

numPred = do
  e1 <- numExpr
  o <- op
  e2 <- numExpr
  return (NumPred e1 o e2)


boolExpr = numPred


boolStmt = do
  e <- boolExpr
  return (Stmt e)

maskStmt = do
  reserved "mask"
  p <- user
  e <- boolExpr
  return (Mask p e) -- WTF?? should indicate that we have a mask; new Syntax needed
  
parseStmt :: CharParser st (Prin, Stmt)
parseStmt = do
  speaker <- user
  reserved "says"
  stmt <- newUser <|> maskStmt <|> boolStmt
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
