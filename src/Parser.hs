module Parser where

import Prelude hiding (lex)
import Lexer
import Syntax hiding (False)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Set as Set
import Data.Maybe (catMaybes)
import FlowControllerLang
import FlowController

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

addUser "root" = do
  reserved "AddUser"
  newUser <- identifier
  return (createSpeakerM newUser)

addUser _ = fail "user other than root tried to add user"

{- addNetwork = do
  reserved "AddNetwork"
  newPrin <- network
  reservedOp "<:"
  parentPrin <- network
  return (AddNetwork newPrin parentPrin)

number = do
  n <- T.integer lex
  return (Number n)
-}

prinList = do
  newPrin <- sepBy prin comma
  return newPrin

forUser (User s) = Just s
forUser _ = Nothing

forApp (App n) = Just n
forApp _ = Nothing

maybeAll [] = Set.all
maybeAll xs = Set.fromList xs

flowGroup = do
  p <- parens prinList
  let flowSend = maybeAll (catMaybes (map forUser p))
  let flowDestPort = maybeAll (catMaybes (map forApp p))
  return (FlowGroup flowSend Set.all Set.all flowDestPort)


{- reservation = do
  reserved "reservation"
  p <- parens prinList
  let s = Set.fromList p
  return (Reservation s) -}

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


-- numExpr = number <|> latency <|> jitter <|> ratelimit

{- numPred = do
  e1 <- numExpr
  o <- op
  e2 <- numExpr
  return (NumPred e1 o e2) -}

{- allow = do
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
-}

newShareStmt spk = do
  reserved "NewShare"
  name <- identifier
  tmp <- parens (sepBy identifier comma)
  let users = Set.fromList tmp
  reserved "reservation" -- TODO: should be some kind of generic resource? reserve?
  fg <- flowGroup
  reservedOp "<="
  size <- T.integer lex
  reserved "on"
  parent <- identifier
  return (newShareM spk parent name users fg (DiscreteLimit size))

resv = do
  fg <- flowGroup
  reservedOp "="
  size <- T.integer lex
  reserved "on"
  share <- identifier
  return (Resv share fg 0 NoLimit size)

reservation spk = do
  reserved "reservation" -- TODO: change to 'reserve'
  r <- resv
  return (reserveM spk r)
  
 
-- parseStmt :: CharParser st (Prin, Stmt)
parseStmt = do
  spk <- identifier
  reserved ":"
  stmt <- addUser spk <|> newShareStmt spk <|> reservation spk
  dot
  return stmt

parseStmts = do
  ss <- many parseStmt
  eof
  return (sequence ss)

parseStmt' = do
  s <- parseStmt
  eof
  return s

parseStmtFromStdin :: IO (DNP Bool)
parseStmtFromStdin = do
  str <- getLine
  case parse parseStmt' "<stdin>" str of
    Left err -> do
      putStrLn ("Parse failed: " ++ show err)
      return (return False)
    Right cmd -> do
      return cmd

parseFromFile :: String -> IO (DNP [Bool])
parseFromFile filename = do
  str <- readFile filename
  case parse parseStmts filename str of
    Left err -> fail (show err)
    Right stmts -> return stmts
