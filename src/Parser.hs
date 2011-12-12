module Parser where

import Base
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
import Control.Monad.Trans
import Test.HUnit

-- Based on:
--   https://github.com/brownplt/webbits/blob/master/src/BrownPLT/JavaScript/Parser.hs

-- Root : AddUser Arjun <: Root.
-- Root : for Arjun Allow(*).
nat = T.natural lex


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
  reserved "reserve" -- TODO: should be some generic resource? (actually, case on resource)
  fg <- flowGroup
  reservedOp "<="
  size <- T.integer lex
  reserved "on"
  parent <- identifier
  return (newShareM spk parent name fg (DiscreteLimit size))

timeNotForever = now <|> absolute <|> relative
  where now      = do { reserved "now"; return (Relative 0) }
        absolute = do { n <- nat; return (Absolute n) }
        relative = incr <|> decr
        incr     = do { reservedOp "+"; n <- nat; return (Relative n) }
        decr     = do { reservedOp "-"; n <- nat; return (Relative (-n)) }

time = timeNotForever <|> forever
  where forever = do { reserved "forever"; return Forever }

from = (do { reserved "from"; timeNotForever }) <|> (return (Absolute 0))

to = (do { reserved "to"; time }) <|> (return Forever)

reservation spk = do
  reserved "reserve"
  fg <- flowGroup
  reservedOp "="
  size <- T.integer lex
  reserved "on"
  share <- identifier
  fromTime <- from
  toTime <- to
  let cmd = do
        now <- getTimeM
        let absFrom = timeToInteger now fromTime
        let absTo = timeToLimit now toTime
        requestM spk (Req share fg absFrom absTo (ReqResv size))
  return cmd

allow spk = do
  reserved "allow"
  fg <- flowGroup
  reserved "on"
  share <- identifier
  fromTime <- from
  toTime <- to
  let cmd = do
        now <- getTimeM
        let absFrom = timeToInteger now fromTime
        let absTo = timeToLimit now toTime
        requestM spk (Req share fg absFrom absTo ReqAllow)
  return cmd

deny spk = do
  reserved "deny"
  fg <- flowGroup
  reserved "on"
  share <- identifier
  fromTime <- from
  toTime <- to
  let cmd = do
        now <- getTimeM
        let absFrom = timeToInteger now fromTime
        let absTo = timeToLimit now toTime
        requestM spk (Req share fg absFrom absTo ReqDeny)
  return cmd


-----------------------------

-- TODO: Can we have a statement which returns something other
-- than a DNP Bool? how?
parseStmt spk = do
  stmt <- tick spk <|> addUser spk <|> newShareStmt spk <|> reservation spk
          <|> allow spk <|> deny spk <|> grantUse spk <|> grantDefaultUse spk
  dot
  return stmt
 
parseTestStmt = do
  res <- boolean
  p <- getPosition
  reserved "<-"
  spk <- identifier
  reserved ":"
  stmt <- parseStmt spk
  return (res, p, stmt)

parseTestStmts = do
  tmp <- many parseTestStmt
  eof
  let (expected, positions, ss) = unzip3 tmp
  let result = evalDNP (sequence ss)
  let assertions = mapM_ (\(exp, pos, res) -> assertEqual (show pos) exp res) 
                         (zip3 expected positions result)
  return (TestCase assertions)

parseCompleteString spk = do
  s <- (parseStmt spk)
  eof
  return s

parseStmtFromStdin :: String -> IO (DNP Bool)
parseStmtFromStdin spk = do
  str <- getLine
  case parse (parseCompleteString spk) "<stdin>" str of
    Left err -> do
      putStrLn ("Parse failed: " ++ show err)
      return (return False)
    Right cmd -> do
      return cmd

parseFromTestFile :: String -> IO Test
parseFromTestFile filename = do
  str <- readFile filename
  case parse parseTestStmts filename str of
    Left err -> fail (show err)
    Right stmts -> return stmts

parseStmtFromString :: String -> String -> IO (DNP Bool)
parseStmtFromString spk str = do
  case parse (parseCompleteString spk) "<string>" str of
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
parseInteractive' spk inBuf action acc = do
  let p acc = do
        T.whiteSpace lex
        s <- parseStmt spk
        lift (action s acc)
  let pars acc = 
                 (do { eof; return acc }) <|>
                 (do { T.whiteSpace lex; eof; return acc }) <|>
                 (do { acc' <- p acc; pars acc' })
  a <- runParserT (pars acc) () "network-in" inBuf 
  case a of
    Left err -> do
      putStrLn $ "parse error:\n" ++ show err
      return acc
    Right a -> return a
