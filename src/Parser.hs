module Parser where

import Base
import Prelude hiding (lex)
import Lexer
import Text.Parsec
import qualified Text.Parsec.Token as T
import qualified Set as Set
import Data.Maybe (catMaybes)
import FlowControllerLang
import FlowController hiding (newShare, getSchedule)
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad
import Test.HUnit
import qualified TokenGraph as TG
import Nettle.IPv4.IPAddress hiding (ipAddressParser)
import qualified Nettle.OpenFlow as OF
import Data.Word
import qualified Flows as Flows
import ShareSemantics
import Control.Monad.State
import qualified Data.List as List

type Node = String

type ShareName = String

nat = T.natural lex

ipAddressParser :: Monad m => ParsecT String u m IPAddress
ipAddressParser = do a <- many1 digit
                     char '.'
                     b <- many1 digit
                     char '.'
                     c <- many1 digit
                     char '.'
                     d <- many1 digit
                     return $ ipAddress (read a) (read b) (read c) (read d)

-- |'NoLimit' means different things in different contexts; supply a word for
-- 'NoLimit'
limit noLimitStr = 
  (do { n <- nat; return (DiscreteLimit n) }) <|>
  (do { reserved noLimitStr; return NoLimit })

boolean = do reserved "True"
             return True
      <|> do reserved "False"
             return False

hexByte = do
  let hexChar = oneOf ([ '0' .. '9' ] ++ [ 'a' .. 'f' ] ++ [ 'A' .. 'F' ])
  ch1 <- hexChar
  ch2 <- hexChar
  let b :: Word8 = read ['0','x',ch1,ch2]
  return b
  
-----------------------------
-- Flow Group handling
-----------------------------

expUser = do
  reserved "user"
  reservedOp "="
  name <- identifier
  return (Flows.fromMatch OF.matchAny)

expSrcPort = do
  reserved "srcPort"
  reservedOp "="
  port <- T.integer lex
  return $ Flows.fromMatch $ 
    OF.matchAny { OF.srcTransportPort = Just (fromIntegral port) }

expDstPort = do
  reserved "dstPort"
  reservedOp "="
  port <- T.integer lex
  return $ Flows.fromMatch $ 
    OF.matchAny { OF.dstTransportPort = Just (fromIntegral port) }

expSrcHost = do
  reserved "srcHost"
  reservedOp "="
  host <- ipAddressParser
  return $ Flows.fromMatch $ OF.matchAny { 
    OF.srcIPAddress = (host, 32), 
    OF.ethFrameType = Just OF.ethTypeIP
  }

expDstHost = do
  reserved "dstHost"
  reservedOp "="
  host <- ipAddressParser
  return $ Flows.fromMatch $ OF.matchAny {
    OF.dstIPAddress = (host, 32),
    OF.ethFrameType = Just OF.ethTypeIP
  }

expNet = do
  reserved "net"
  reservedOp "="
  name <- identifier
  return (Flows.fromMatch OF.matchAny)

ethAddr = do
  b0 <- hexByte
  colon
  b1 <- hexByte
  colon
  b2 <- hexByte
  colon
  b3 <- hexByte
  colon
  b4 <- hexByte
  colon
  b5 <- hexByte
  return (OF.ethernetAddress b0 b1 b2 b3 b4 b5)

srcEth = do
  reserved "srcEth"
  reservedOp "="
  e <- ethAddr
  return $ Flows.fromMatch $ OF.matchAny { OF.srcEthAddress = Just e }

dstEth = do
  reserved "dstEth"
  reservedOp "="
  e <- ethAddr
  return $ Flows.fromMatch $ OF.matchAny { OF.dstEthAddress = Just e }

prin = 
  expUser <|> expSrcPort <|> expDstPort <|> expSrcHost <|> expDstHost <|> expNet
  <|> srcEth <|> dstEth

flowGroupSet = do
  prins <- parens (sepBy prin comma)
  let flow = foldl Flows.intersection Flows.all prins
  case Flows.null flow of
    True -> fail "invalid flow"
    False -> return flow

flowGroupAll = do
  parens (reservedOp "*")
  return Flows.all

flowGroup = (try flowGroupAll) <|> flowGroupSet

-----------------------------
-- Share Permissions
-----------------------------

resvUB = (do
  reserved "reserve"
  reservedOp "<="
  size <- T.integer lex
  return size) <|> (return 0)

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

resvLB = (do
  reserved "reserve"
  reservedOp ">="
  size <- T.integer lex
  return (fromInteger size)) <|> (return NoLimit)

resvCap rub = (do
  reserved "reserveTBCapacity"
  reservedOp "="
  c <- T.integer lex
  return (fromInteger c)) <|> (return (fromInteger rub)) 

resvFill rub = (do
  reserved "reserveTBFill"
  reservedOp "="
  f <- T.integer lex
  return f) <|> (return rub)

permList = do
-- TODO: Convert to be able to specify in any order; need defaults when missing
--  newPerm <- sepBy perm comma
  rub <- resvUB
  ca <- allowPerm
  cd <- denyPerm
  rlb <- resvLB
  rtbc <- resvCap rub
  rtbf <- resvFill rub
  return (rub, ca, cd, rlb, rtbc, rtbf)

sharePerms = do
  p <- brackets permList
  return p

-----------------------------
-- System Management Functions
-----------------------------

tick "root" = do
  reserved "Tick"
  t <- T.integer lex
  return (return (BoolResult True))

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
  (rub, ca, cd, rlb, rtbc, rtbf) <- sharePerms
  reserved "on"
  parent <- identifier
  let tg = TG.new rtbf rlb (DiscreteLimit rub) rtbc
  let s = Share name fg (Set.singleton spk) emptyShareReq
            ca cd tg
  return (newShareM spk parent s)

-----------------------------
-- Query Functions
-----------------------------

getSchedule speaker = do
  reserved "GetSchedule"
  shareName <- identifier
  return (getScheduleM speaker shareName)

listShares speaker = do
  reserved "ListShares"
  return (listShareRefsByUserM speaker)

listSharesByFlowGroup = do
  reserved "ListSharesByFlowGroup"
  fg <- flowGroup
  return (listShareRefsByFlowGroupM fg)

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

verb spk = reserve spk <|> allow spk <|> deny spk
shareManage spk = newShare spk <|> grant spk <|> grantDefault spk
sysManage spk = tick spk <|> addUser spk
query spk = getSchedule spk <|> listShares spk <|> listSharesByFlowGroup

parseStmt spk = do
  stmt <- verb spk <|> shareManage spk <|> sysManage spk <|> query spk
  dot
  return stmt

dnpResult = 
  (do { b <- boolean; return (BoolResult b) })
 
parseTestStmt = do
  res <- dnpResult
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
  return assertions

parseCompleteString spk = do
  s <- (parseStmt spk)
  eof
  return s

parseStmtFromStdin :: String -> IO (DNP DNPResult)
parseStmtFromStdin spk = do
  str <- getLine
  case parse (parseCompleteString spk) "<stdin>" str of
    Left err -> do
      putStrLn ("Parse failed: " ++ show err)
      return (return (BoolResult False))
    Right cmd -> do
      return cmd

parseFromTestFile :: String -> IO (IO ())
parseFromTestFile filename = do
  str <- readFile filename
  case parse parseTestStmts filename str of
    Left err -> fail (show err)
    Right stmts -> return stmts

parseStmtFromString :: String -> String -> IO (DNP DNPResult)
parseStmtFromString spk str = do
  case parse (parseCompleteString spk) "<string>" str of
    Left err -> do
      putStrLn ("Parse failed: " ++ show err)
      return (return (BoolResult False))
    Right cmd -> do
      return cmd

parseInteractive' :: String  -- ^speaker
                 -> String  -- ^input stream
                 -> (DNP DNPResult -> a -> IO a)
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


-- |
paneMan :: Chan (Speaker, String) -- ^commands from speaker
        -> Chan Integer           -- ^current time
        -> IO (Chan MatchTable, Chan (Speaker, String))
paneMan reqChan timeChan = do
  tblChan <- newChan
  respChan <- newChan
  stRef <- newIORef emptyState
  let handleReq = do
        (spk, req) <- readChan reqChan
        dnpM <- parseStmtFromString spk req
        st <- readIORef stRef
        (resp, st') <- runStateT dnpM st
        case resp of
          BoolResult True -> do
            writeIORef stRef st'
            writeChan respChan (spk, show resp)
          otherwise -> do
            writeChan respChan (spk, show resp)
      buildTbl = do
        now <- readChan timeChan
        st <- readIORef stRef
        writeIORef stRef (st { stateNow = now })
        writeChan tblChan (compileShareTree now (getShareTree st))
  forkIO (forever handleReq)
  forkIO (forever buildTbl)
  return (tblChan, respChan)

