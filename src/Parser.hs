module Parser where

import Prelude hiding (lex)
import Lexer
import Syntax
import Text.Parsec
import qualified Text.Parsec.Token as T
import qualified Set as Set
import Data.Maybe (catMaybes)
import FlowControllerLang
import FlowController hiding (tick)
import Control.Monad.IO.Class


-- Based on:
--   https://github.com/brownplt/webbits/blob/master/src/BrownPLT/JavaScript/Parser.hs

-- Root : AddUser Arjun <: Root.
-- Root : for Arjun Allow(*).


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

boolean = do reserved "True"
             return True
      <|> do reserved "False"
             return False


-----------------------------

{-
  addNetwork = do
  reserved "AddNetwork"
  newPrin <- network
  reservedOp "<:"
  parentPrin <- network
  return (AddNetwork newPrin parentPrin)
-}


{-
latency = do
  reserved "latency"
  p <- parens prinList
  let l = Set.fromList p
  return (Latency l)

jitter = do
  reserved "jitter"
  p <- parens prinList
  let j = Set.fromList p
  return (Jitter j)

ratelimit = do
  reserved "ratelimit"
  p <- parens prinList
  let r = Set.fromList p
  return (Ratelimit r)
-}

{-
op = (reservedOp "<=" >> return NumLEq) <|> (reservedOp "<" >> return NumLT)
  <|> (reservedOp "=" >> return NumEq) <|> (reservedOp ">=" >> return NumGEq)
  <|> (reservedOp ">" >> return NumGT)


numExpr = number <|> latency <|> jitter <|> ratelimit

numPred = do
  e1 <- numExpr
  o <- op
  e2 <- numExpr
  return (NumPred e1 o e2)
-}

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

tick "root" = do
  reserved "Tick"
  t <- T.integer lex
  return (tickM t)

tick _ = fail "only root can tick the clock"

addUser "root" = do
  reserved "AddUser"
  newUser <- identifier
  return (createSpeakerM newUser)

addUser _ = fail "user other than root tried to add user"

grantUse spk = do
  reserved "GrantUse"
  newUser <- identifier
  reserved "on"
  share <- identifier 
  return (giveReferenceM spk share newUser)

grantDefaultUse spk = do
  reserved "GrantDefaultUse"
  reserved "on"
  share <- identifier 
  return (giveDefaultReferenceM spk share)

newShareStmt spk = do
  reserved "NewShare"
  name <- identifier
  tmp <- parens (sepBy identifier comma)
  let users = Set.fromList tmp
  reserved "reserve" -- TODO: should be some generic resource? (actually, case on resource)
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
  reserved "reserve"
  r <- resv
  -- TODO: Here we want some kind of (maybe "from") & (maybe "to")
  -- examples: from 1730 to 1830, from 1730 to +60m, from Now to +60m, from Now to Forever
  -- WELL, NewShare would always have "from Now to Forever" at least as we've constructed
  -- things so far. We also want NewShare to have tokenBucket (eventually) ... this is a
  -- rate limit, rather than the fixed limit of "from"/"to"
  -- OR, should it go in the boolExpr as *part* of the prinList, that is: user=blah,from=,to=
  return (reserveM spk r)

-----------------------------

-- TODO: Can we have a statement which returns something other than a DNP Bool? how?
parseStmt spk = do
  stmt <- tick spk <|> addUser spk <|> newShareStmt spk <|> reservation spk <|> grantUse spk
          <|> grantDefaultUse spk
  dot
  return stmt
 
parseTestStmt = do
  res <- boolean
  reserved "<-"
  spk <- identifier
  reserved ":"
  stmt <- parseStmt spk
  return (res, stmt)

parseTestStmts = do
  tmp <- many parseTestStmt
  eof
  let (res, ss) = unzip tmp
  return (res, (sequence ss))

parseInteractive spk = do
  s <- (parseStmt spk)
  eof
  return s

parseStmtFromStdin :: String -> IO (DNP Bool)
parseStmtFromStdin spk = do
  str <- getLine
  case parse (parseInteractive spk) "<stdin>" str of
    Left err -> do
      putStrLn ("Parse failed: " ++ show err)
      return (return False)
    Right cmd -> do
      return cmd

parseFromTestFile :: String -> IO ([Bool], DNP [Bool])
parseFromTestFile filename = do
  str <- readFile filename
  case parse parseTestStmts filename str of
    Left err -> fail (show err)
    Right stmts -> return stmts

parseStmtFromString :: String -> String -> IO (DNP Bool)
parseStmtFromString spk str = do
  case parse (parseInteractive spk) "<string>" str of
    Left err -> do
      putStrLn ("Parse failed: " ++ show err)
      return (return False)
    Right cmd -> do
      return cmd

parseInteractive' :: String  -- ^speaker
                  -> String  -- ^input stream
                  -> (DNP Bool -> a -> IO a)
                  -> a
                  -> IO a
parseInteractive' spk inStream action acc = do
  putStrLn $ "Trying to parse " ++ inStream
  let p acc = do
        liftIO $ putStrLn "Waiting to parse ..."
        s <- parseStmt spk
        liftIO $ putStrLn "parsed something."
        liftIO (action s acc)
  let pars acc = 
                 (do { eof; liftIO $ putStrLn "EOF"; return acc }) <|>
                 (do { acc' <- p acc; pars acc' })
  a <- runParserT (pars acc) () "network-in" inStream
  case a of
    Left err -> do
      putStrLn $ "parse error: " ++ show err
      return acc
    Right a -> return a
