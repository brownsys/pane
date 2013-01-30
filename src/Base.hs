{-# LANGUAGE ScopedTypeVariables #-}

module Base 
  ( Flow (..)
  , FlowGroup
  , User
  , Host
  , Port
  , Word16
  , Result (..)
  , Limit (..) 
  , Time (..)
  , timeToLimit
  , timeToInteger
  , Speaker
  , ShareRef
  , Req (..)
  , ReqData (..)
  , module Data.IORef
  , module Control.Monad
  , module Control.Concurrent
  , mergeChan
  , liftChan
  , liftChanIO3
  , module Control.Exception
  , catMaybes
  , mapMaybe
  , sysTime
  , logo
  , retryOnExns
  , ignoreExns
  , killOnExns
  , PaneConfig (..)
  ) where

import Data.Maybe
import Control.Monad
import Control.Concurrent
import System.IO.Unsafe
import Data.IORef
import Set (Set)
import qualified Set as Set
import System.IO (Handle)
import qualified Data.Tree as Tree
import Text.PrettyPrint.HughesPJ
import Data.Word
import Nettle.OpenFlow hiding (Port)
import Flows
import qualified Nettle.OpenFlow as OF
import Control.Exception
import qualified System.Log.Logger as Logger
import System.Log.Logger.TH (deriveLoggers)

$(deriveLoggers "Logger" [Logger.WARNING])

sysTime :: IORef Integer -- ^ system time, updated by timeService
sysTime = unsafePerformIO (newIORef 0)

retryOnExns :: String -> IO a -> IO a
retryOnExns msg action = action `catches`
          [ Handler (\(e :: AsyncException) -> throw e),
            Handler exnHandler ]
  where exnHandler (e :: SomeException) = do
          warningM $ "Exception (retrying): " ++ show e
          warningM $ "Exception log message: " ++ msg
          retryOnExns msg action

ignoreExns :: String -> IO () -> IO ()
ignoreExns msg action = action `catches`
          [ Handler (\(e :: AsyncException) -> throw e),
            Handler exnHandler ]
  where exnHandler (e :: SomeException) = do
          warningM $ "Exception (ignoring): " ++ show e
          warningM $ "Exception log message: " ++ msg

killOnExns :: String -> IO () -> IO ()
killOnExns msg action = action `catches`
          [ Handler (\(e :: AsyncException) -> throw e),
            Handler exnHandler ]
  where exnHandler (e :: SomeException) = do
          warningM $ "Exception (killing thread): " ++ show e
          warningM $ "Exception log message: " ++ msg
          myThreadId >>= killThread

liftChanIO3 :: (a -> b -> c -> IO d) 
             -> (a, Chan a)
             -> (b, Chan b)
             -> (c, Chan c)
             -> IO (Chan d)
liftChanIO3 fn (initA, chanA) (initB, chanB) (initC, chanC) = do
  merged' <- mergeChan chanA chanB
  merged <- mergeChan merged' chanC
  result <- newChan
  let loop a b c = do
        v <- readChan merged
        case v of
          Left (Left a') -> do
            r <- fn a' b c
            writeChan result r
            loop a' b c
          Left (Right b') -> do
            r <- fn a b' c
            writeChan result r
            loop a b' c
          Right c' -> do
            r <- fn a b c'
            writeChan result r
            loop a b c'
  forkIO (loop initA initB initC)
  return result

liftChan :: (a -> b -> IO c) -> (a, Chan a) -> (b, Chan b) -> IO (Chan c)
liftChan fn (initA, chanA) (initB, chanB) = do
  merged <- mergeChan chanA chanB
  result <- newChan
  let loop a b = do
        v <- readChan merged
        case v of
          Left a' -> do
            r <- fn a' b
            writeChan result r
            loop a' b
          Right b' -> do
            r <- fn a b'
            writeChan result r
            loop a b'
  forkIO (loop initA initB)
  return result

mergeChan :: Chan a -> Chan b -> IO (Chan (Either a b))
mergeChan chan1 chan2 = do
  mergedChan <- newChan
  forkIO $ forever $ do
    v <- readChan chan1
    writeChan mergedChan (Left v)
  forkIO $ forever $ do
    v <- readChan chan2
    writeChan mergedChan (Right v)
  return mergedChan

data Result
  = BoolResult Bool
  | ScheduleResult [(Limit, Limit, Limit)]
  | ShareRefsResult [ShareRef]
  deriving Eq

renderResult (BoolResult b)       = text (show b)
renderResult (ScheduleResult lst) = cat $ punctuate (text "; ") $ (len:(map f lst))
  where len = text (show (length lst))
        f (t, bw, toks) = cat $ punctuate (text ",") $ 
                            [renderLimit t, renderLimit bw, renderLimit toks]
renderResult (ShareRefsResult lst) =
  cat $ punctuate (text "; ") $ (len:(map text lst))
  where len = text (show (length lst))

renderLimit lim = text (show lim)

instance Show Result where
  show r = render (renderResult r)

data Limit 
  = NoLimit 
  | DiscreteLimit Integer 
  deriving (Eq)

instance Show Limit where
  show (DiscreteLimit n) = show n
  show NoLimit           = "inf"

instance Ord Limit where
  _ <= NoLimit = True -- TODO(arjun): NoLimit <= NoLimit should be an error
  (DiscreteLimit m) <= (DiscreteLimit n) = m <= n
  NoLimit <= (DiscreteLimit _) = False

instance Num Limit where
  (DiscreteLimit m) + (DiscreteLimit n) = DiscreteLimit (m + n)
  _                 + _                 = NoLimit

  (DiscreteLimit m) - (DiscreteLimit n) = DiscreteLimit (m - n)
  NoLimit           - NoLimit           = error "NoLimit - NoLimit"
  NoLimit           - (DiscreteLimit n) = NoLimit
  (DiscreteLimit n) - NoLimit           = error "DiscreteLimit _ - NoLimit"

  (DiscreteLimit m) * (DiscreteLimit n) = DiscreteLimit (m * n)
  _                 * (DiscreteLimit 0) = DiscreteLimit 0
  (DiscreteLimit 0) * _                 = DiscreteLimit 0
  _                 * _                 = NoLimit

  fromInteger n = DiscreteLimit n

  abs (DiscreteLimit m) = DiscreteLimit (abs m)
  abs NoLimit           = NoLimit

  signum (DiscreteLimit m) = DiscreteLimit (signum m)
  signum NoLimit           = error "signum NoLimit"

instance Enum Limit where
  succ (DiscreteLimit m) = DiscreteLimit (succ m)
  succ NoLimit           = NoLimit
  toEnum n = DiscreteLimit (fromIntegral n)
  enumFrom (DiscreteLimit n) = 
    (DiscreteLimit n):(enumFrom (DiscreteLimit (n+1)))
  enumFrom NoLimit           = [NoLimit]
  fromEnum (DiscreteLimit n) = fromIntegral n
  fromEnum NoLimit           = error "fromEnum NoLimit"

data Time
  = Relative Integer -- ^relative to now
  | Absolute Integer
  | Forever
  deriving (Ord, Eq, Show)

timeToLimit :: Integer -> Time -> Limit
timeToLimit now (Relative delta) = DiscreteLimit (now + delta)
timeToLimit _   (Absolute t)     = DiscreteLimit t
timeToLimit _   Forever          = NoLimit 

timeToInteger :: Integer -> Time -> Integer
timeToInteger now t = case timeToLimit now t of
  DiscreteLimit n -> n
  NoLimit -> error "timeToInteger _ Forever"

type Speaker = String

type ShareRef = String

data Req = Req {
  reqShare :: ShareRef,
  reqFlows :: FlowGroup,
  reqStart :: Integer, -- invariant: start < end
  reqEnd :: Limit,
  reqData :: ReqData,
  reqStrict :: Bool
} deriving (Show, Ord, Eq)

data ReqData 
  = ReqResv Integer
  | ReqAllow
  | ReqDeny
  | ReqOutPort (Maybe OF.SwitchID) OF.PseudoPort
  deriving (Eq, Ord, Show)

logo putter = do
  putter ""
  putter "     _______  ____     ____   ___ _______"
  putter "    /  __  / / __ \\    |   \\  | | | _____|"
  putter "   /  /_/ / / /__\\ \\   | |\\ \\ | | | |___"
  putter "  / _____/ / ______ \\  | | \\ \\| | |  __|"
  putter " / /      / /      \\ \\ | |  \\ \\ | | |____"
  putter "/_/      /_/        \\_\\|_|   \\__| |______|"
  putter ""
  putter "    A PROTOTYPE PARTICIPATORY NETWORK"
  putter ""


data PaneConfig = PaneConfig
  { controllerPort    :: Word16
  , ovsSetQueue       :: String
  , ovsDeleteQueue    :: String
  , logScreenPrio     :: Logger.Priority
  , logFilePath       :: String
  , logFilePrio       :: Logger.Priority
  }
