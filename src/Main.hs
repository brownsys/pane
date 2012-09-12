module Main where

import Prelude hiding (catch)
import Control.Exception (finally)
import System.Console.GetOpt
import System.IO
import Data.IORef
import System.IO.Unsafe
import Control.Monad
import System.Environment
import System.IO
import Data.Word
import Parser
import ShareTreeLang
import ShareTree (emptyStateWithTime, State)
import Test.HUnit hiding (State)
import Base
import Control.Concurrent.MVar
import Control.Concurrent
import System.Time
import qualified NIB
import ControllerService
import OFCompiler (compilerService)
import CombinedPaneMac
import PaneInteractionServer (interactions)
import Data.ConfigFile
import Control.Monad.Error
import Control.Exception

data Argument
  = Test String
  | NewServer Word16
  | Help
  | Config String

argSpec =
  [ Option ['t'] ["test"] (ReqArg Test "FILE") "run test case"
  , Option ['c'] ["config"] (ReqArg Config "FILE") "config file"
  , Option ['p'] ["port"] (ReqArg (NewServer . read) "PORT") "port number for interaction server"
  , Option ['h'] ["help"] (NoArg Help) "print this help message"
  ]

defaultControllerConfig = ControllerConfig
  { controllerPort = 6633
  , ovsSetPortQ    = "./scripts/ovs-set-port-queue-min.sh"
  , ovsDeletePortQ = "./scripts/ovs-delete-port-queue.sh"
  }

runTestFile f = do
  runTests <- parseFromTestFile f
  runTests

timeService = do
  tickChan <- newChan
  forkIO $ forever $ do
    threadDelay (10^6) -- 1 second
    (TOD now _) <- getClockTime
    writeChan tickChan now
    writeIORef sysTime now
  (TOD initNow _) <- getClockTime
  writeIORef sysTime initNow
  return (initNow, tickChan)

--
-- Functions for handling the config file
--

readConfig file = do
  putStrLn $ "Reading config file: " ++ file
  rv <- runErrorT $ do
      cp <- join $ liftIO $ readfile (emptyCP { optionxform = id }) file
      return cp
  case rv of
    Left x ->  do putStrLn $ "Invalid config file; using built-in defaults. " ++ show(x)
                  return emptyCP
    Right x -> return x

parseConfig settings config = do
  config <- case get settings "DEFAULT" "ControllerPort" of
              Left e  -> do putStrLn $ "Invalid controller port setting; using default. " ++ show(e)
                            return config
              Right x -> return (config { controllerPort = x })
  config <- case get settings "Open vSwitch" "setPortScript" of
              Left e  -> do putStrLn $ "Invalid OVS set port script; using default. " ++ show(e)
                            return config
              Right x -> return (config { ovsSetPortQ = x })
  config <- case get settings "Open vSwitch" "deletePortScript" of
              Left e  -> do putStrLn $ "Invalid OVS delete port script; using default. " ++ show(e)
                            return config
              Right x -> return (config { ovsDeletePortQ = x })
  return config

--
-- Where the action is: command-line argument processing
--

action [Test file] = runTestFile file
action [NewServer port] = action [Config "pane.cfg", NewServer port]
action [Config file, NewServer port] = do
  logo putStrLn
  putStrLn "Starting PANE  ..."
  (initTime, tickChan) <- timeService

  settings <- readConfig file `catch`
                              (\e -> do let err = show (e :: IOException)
                                        putStrLn $ ("Warning: Couldn't open " ++ file ++ ": " ++ err)
                                        return emptyCP)

  config <- parseConfig settings defaultControllerConfig

  putStrLn "Creating empty NIB..."
  nibMsg <- newChan
  nib <- NIB.newEmptyNIB nibMsg

  putStrLn "Creating PANE + MAC Learning system..."
  packetIn <- newChan
  switches <- newChan
  paneReq <- newChan
  (tbl, paneResp, pktOut) <- combinedPaneMac switches packetIn paneReq tickChan

  putStrLn $ "Starting PANE console on port " ++ show port ++ " ..."
  interactions port paneResp paneReq

  putStrLn "Starting compiler ..."
  nibUpdates <- newChan -- TODO(arjun): write into this
  nibSnapshot <- compilerService (nib, nibUpdates) tbl

  putStrLn "Starting OpenFlow controller ..."
  controller nibSnapshot nibMsg packetIn switches pktOut config
action [Help] = do
  logo putStrLn
  putStrLn $ usageInfo "Usage Info" argSpec
action [] = do
  logo putStrLn
  putStrLn $ usageInfo "Usage Info" argSpec
  fail "too few args"
action _ = do
  logo putStrLn
  putStrLn $ usageInfo "Usage Info" argSpec
  fail "too many args"

mainBody = do
  rawArgs <- getArgs
  let (args, options, errors) = getOpt RequireOrder argSpec rawArgs
  unless (null errors) $ do { mapM_ putStrLn errors; fail "bad args" }
  action args

shutdown = do
  return ()

main = mainBody `finally` shutdown
