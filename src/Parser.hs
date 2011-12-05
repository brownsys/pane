module Parser where

import Prelude hiding (lex)
import Lexer
import Syntax
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Data.Set as Set

-- Based on:
--   https://github.com/brownplt/webbits/blob/master/src/BrownPLT/JavaScript/Parser.hs

-- Root : AddUser Arjun <: Root.
-- Root : for Arjun Allow(*).


-- implicitly indicated user
user = do
  name <- identifier
  return (User name)

network = do
  name <- identifier
  return (Network name)

shareName = do
  name <- identifier
  return name -- return (ShareName name) ??

-- explicitly indicated user
expUser = do
  reserved "user"
  reservedOp "="
  name <- identifier
  return (User name)

expApp = do
  reserved "app"
  reservedOp "="
  port <- T.integer lex  -- names  in the future ??
  return (App port)

expNet = do
  reserved "net"
  reservedOp "="
  name <- identifier
  return (Network name)

flow = do
  reservedOp "*"
  return (Flow "*")

prin = flow <|> expUser <|> expApp <|> expNet

addUser = do
  reserved "AddUser"
  newPrin <- user
  return (AddUser newPrin)

addNetwork = do
  reserved "AddNetwork"
  newPrin <- network
  reservedOp "<:"
  parentPrin <- network
  return (AddNetwork newPrin parentPrin)

number = do
  n <- T.integer lex
  return (Number n)

prinList = do
  newPrin <- sepBy prin comma
  return newPrin

reservation = do
  reserved "reservation"
  p <- parens prinList
  let s = Set.fromList p
  return (Reservation s)

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

allow = do
  reserved "allow"
  p <- parens prinList
  let s = Set.fromList p
  return (Allow s)

deny = do
  reserved "deny"
  p <- parens prinList
  let s = Set.fromList p
  return (Deny s)

boolExpr = numPred <|> allow <|> deny


boolStmt = do
  e <- boolExpr
  reserved "on"
  share <- shareName
  return (Stmt e share)

newShareStmt = do
  reserved "NewShare"
  name <- shareName
  tmp <- parens (sepBy user comma)
  let users = Set.fromList tmp
  stmt <- boolStmt
  return (NewShare name users stmt)
 
 
parseStmt :: CharParser st (Prin, Stmt)
parseStmt = do
  speaker <- user
  reserved ":"
  stmt <- addUser <|> addNetwork <|> newShareStmt <|> boolStmt
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
