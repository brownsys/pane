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
import FlowController hiding (tick, newShare)
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad
import Test.HUnit
import qualified TokenBucket as TB

-- Based on:
--   https://github.com/brownplt/webbits/blob/master/src/BrownPLT/JavaScript/Parser.hs

nat = T.natural lex

-- |'NoLimit' means different things in different contexts; supply a word for
-- 'NoLimit'
limit noLimitStr = 
  (do { n <- nat; return (DiscreteLimit n) }) <|>
  (do { reserved noLimitStr; return NoLimit })

boolean = do reserved "True"
             return True
      <|> do reserved "False"
             return False

tokenBucket = do
  reserved "bucket"
  reservedOp "("
  init <- limit "unlimited"
  comma
  capacity <- limit "unlimited"
  comma
  when (init > capacity) (fail "initial tokens exceeds capacity")
  mintRate <- nat
  reservedOp ")"
  return (TB.new init capacity mintRate)

-----------------------------
-- Flow Group handling
-----------------------------

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

-----------------------------
-- Share Permissions
-----------------------------

resvLimit = do
  reserved "reserve"
  reservedOp "<="
  size <- T.integer lex
  return (DiscreteLimit size)

-- TODO: Obviously, this is redundant and perhaps doesn't make sense, but I
-- want to leave open the question of whether this should be ternary (as in,
-- yes/no/inherit-from-parent) or if not, which case should be the default when
-- unspecified (perhaps 'allow=True' should be required, and 'allow=False' should
-- be the default since it means user must grant this authority explicitly)
allowPerm = (do
  reserved "allow"
  reservedOp "="
  b <- boolean
  return b) <|> (return True)

denyPerm = (do
  reserved "deny"
  reservedOp "="
  b <- boolean
  return b) <|> (return True)

-- perm = resvLimit <|> allowPerm <|> denyPerm 

permList = do
-- TODO: Convert to be able to specify in any order; need defaults when missing
--  newPerm <- sepBy perm comma
  lim <- resvLimit
  ca <- allowPerm
  cd <- denyPerm
  return (lim, ca, cd)

sharePerms = do
  p <- brackets permList
  return p

-----------------------------
-- System Management Functions
-----------------------------

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

-----------------------------
-- Share Management Functions
-----------------------------

grant spk = do
  reserved "Grant"
  share <- identifier
  reserved "to"
  newUser <- identifier
  return (giveReferenceM spk share newUser)

grantDefault spk = do
  reserved "GrantDefault"
  share <- identifier 
  return (giveDefaultReferenceM spk share)

newShare spk = do
  reserved "NewShare"
  name <- identifier
  reserved "for"
  fg <- flowGroup
  (lim, ca, cd) <- sharePerms
  reserved "on"
  parent <- identifier
  resvBucket <- do { reserved "throttle"; tokenBucket } <|> return TB.unlimited
  let s = Share name fg (Set.singleton spk) emptyShareReq lim
            ca cd resvBucket
  return (newShareM spk parent s)

-----------------------------
-- Helper Functions for Verbs
-----------------------------

timeNotForever = now <|> absolute <|> relative
  where now      = do { reserved "now"; return (Relative 0) }
        absolute = do { n <- nat; return (Absolute n) }
        relative = incr <|> decr
        incr     = do { reservedOp "+"; n <- nat; return (Relative n) }
        decr     = do { reservedOp "-"; n <- nat; return (Relative (-n)) }

time = timeNotForever <|> forever
  where forever = do { reserved "forever"; return Forever }

from = (do { reserved "from"; timeNotForever }) <|> (return (Relative 0))

to = (do { reserved "to"; time }) <|> (return Forever)

-----------------------------
-- Verb Functions
-----------------------------

-- TODO: This might be clearer if it is "strict reserve(..." and
-- "partial reserve(..." but I don't know the parser well-enough
strict = (do { reserved "strict"; return True }) <|>
         (do { reserved "partial"; return False }) <|>
         (return True) -- choosing defaults again...

reserve spk = do
  reserved "reserve"
  fg <- flowGroup
  reservedOp "="
  size <- T.integer lex
  s <- strict
  reserved "on"
  share <- identifier
  fromTime <- from
  toTime <- to
  let cmd = do
        now <- getTimeM
        let absFrom = timeToInteger now fromTime
        let absTo = timeToLimit now toTime
        requestM spk (Req share fg absFrom absTo (ReqResv size) s)
  return cmd

allow spk = do
  reserved "allow"
  fg <- flowGroup
  s <- strict
  reserved "on"
  share <- identifier
  fromTime <- from
  toTime <- to
  let cmd = do
        now <- getTimeM
        let absFrom = timeToInteger now fromTime
        let absTo = timeToLimit now toTime
        requestM spk (Req share fg absFrom absTo ReqAllow s)
  return cmd

deny spk = do
  reserved "deny"
  fg <- flowGroup
  s <- strict
  reserved "on"
  share <- identifier
  fromTime <- from
  toTime <- to
  let cmd = do
        now <- getTimeM
        let absFrom = timeToInteger now fromTime
        let absTo = timeToLimit now toTime
        requestM spk (Req share fg absFrom absTo ReqDeny s)
  return cmd


-----------------------------
-- Top-level Parsing Functions
-----------------------------

-- TODO: Can we have a statement which returns something other
-- than a DNP Bool? how?

verb spk = reserve spk <|> allow spk <|> deny spk
shareManage spk = newShare spk <|> grant spk <|> grantDefault spk
sysManage spk = tick spk <|> addUser spk

parseStmt spk = do
  stmt <- verb spk <|> shareManage spk <|> sysManage spk
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
  let f (exp, pos, result) = do
          assertEqual (show pos) exp result
  let assertions = do
        results <- evalDNP (sequence ss)
        mapM_ f (zip3 expected positions results)
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
                 try(do { eof; return acc }) <|>
                 try(do { T.whiteSpace lex; eof; return acc }) <|>
                 (do { acc' <- p acc; pars acc' })
  a <- runParserT (pars acc) () "network-in" inBuf 
  case a of
    Left err -> do
      putStrLn $ "parse error:\n" ++ show err
      return acc
    Right a -> return a
