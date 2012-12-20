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
import System.Log.Logger.TH (deriveLoggers)
import qualified System.Log.Logger as Logger
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
import Management
import SetupLogging
import qualified System.Remote.Monitoring as EKG
import qualified Data.ByteString.Char8 as B

$(deriveLoggers "Logger" [Logger.WARNING, Logger.NOTICE, Logger.ERROR])

data Argument
  = Test String
  | NewServer Word16
  | Help
  | Config String

argSpec =
  [ Option ['t'] ["test"] (ReqArg Test "FILE") "run test case"
  , Option ['c'] ["config"] (ReqArg Config "FILE") "config file"
  , Option ['p'] ["port"] (ReqArg (NewServer . read) "PORT")
                          "port number for interaction server"
  , Option ['h'] ["help"] (NoArg Help) "print this help message"
  ]

defaultPaneConfig = PaneConfig
  { controllerPort = 6633
  , ovsSetQueue    = "./scripts/ovs-set-port-queue-min.sh"
  , ovsDeleteQueue = "./scripts/ovs-delete-port-queue.sh"
  , logScreenPrio  = Logger.NOTICE
  , logFilePath    = ""
  , logFilePrio    = Logger.DEBUG
  }

------------------------------------------------------------------------------
--
-- Where the action is...
--
------------------------------------------------------------------------------

startup :: String -> Word16 -> IO ()
startup filename port = do
  logo putStrLn
  (initTime, tickChan) <- timeService

  --
  -- Load & parse config file
  --

  settings <- readConfig filename `catch`
                              (\e -> do let err = show (e :: IOException)
                                        warningM $ "Warning: Couldn't open " ++
                                                   filename ++ ": " ++ err
                                        return emptyCP)
  config <- parseConfig settings defaultPaneConfig

  setupLogging config

  --
  -- Start services
  --

  noticeM $ "Starting PANE's component services ..."

  noticeM $ "Creating empty NIB ..."
  nibMsg <- newChan
  nib <- NIB.newEmptyNIB nibMsg

  noticeM $ "Creating PANE + MAC Learning system..."
  packetIn <- newChan
  switches <- newChan
  paneReq <- newChan
  (tbl, paneResp, pktOut) <- combinedPaneMac switches packetIn paneReq tickChan

  noticeM $ "Starting PANE interaction console on port " ++ show port ++ " ..."
  interactions port paneResp paneReq

  noticeM $ "Launching management server..."
  mgmtReq <- newChan
  mgmtResp <- mgmtServer mgmtReq nibMsg config

  let mgmtPort = port + 1
  noticeM $ "Starting PANE management console on port " ++ show (mgmtPort)
            ++  " ..."
  interactions mgmtPort mgmtResp mgmtReq

  noticeM $ "Starting compiler ..."
  nibUpdates <- newChan -- TODO(arjun): write into this
  nibSnapshot <- compilerService (nib, nibUpdates) tbl

  noticeM $ "Starting OpenFlow controller ..."
  controller nibSnapshot nibMsg packetIn switches pktOut config


------------------------------------------------------------------------------
--
-- Command-line argument processing
--
------------------------------------------------------------------------------

action [Test file] = runTestFile file
action [NewServer port] = startup "pane.cfg" port
action [Config file, NewServer port] = startup file port
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

------------------------------------------------------------------------------
--
-- Functions for handling the config file
--
------------------------------------------------------------------------------

readConfig file = do
  noticeM $ "Reading config file: " ++ file
  rv <- runErrorT $ do
      cp <- join $ liftIO $ readfile (emptyCP { optionxform = id }) file
      return cp
  case rv of
    Left x ->  do errorM $ "Invalid config file; using built-in defaults. "
                           ++ show(x)
                  return emptyCP
    Right x -> return x

parseConfig settings config = do
  config <- case get settings "DEFAULT" "ControllerPort" of
              Left e  -> do putStrLn $ "Invalid controller port setting; " ++
                                       "using default. " ++ show(e)
                            return config
              Right x -> return (config { controllerPort = x })
  config <- case get settings "Open vSwitch" "setQueueScript" of
              Left e  -> do putStrLn $ "Invalid OVS set port script; " ++
                                       "using default. " ++ show(e)
                            return config
              Right x -> return (config { ovsSetQueue = x })
  config <- case get settings "Open vSwitch" "deleteQueueScript" of
              Left e  -> do putStrLn $ "Invalid OVS delete port script; " ++
                                       "using default. " ++ show(e)
                            return config
              Right x -> return (config { ovsDeleteQueue = x })
  config <- case get settings "Logging" "logScreenPriority" of
              Left e  -> do putStrLn $ "Invalid logScreenPriority; " ++
                                       "using default. " ++ show(e)
                            return config
              Right x -> return (config { logScreenPrio = str2logPriority x })
  config <- case get settings "Logging" "logFilePath" of
              Left e  -> do putStrLn $ "Invalid logFilePath; using default. "
                                       ++ show(e)
                            return config
              Right x -> return (config { logFilePath = x })
  config <- case get settings "Logging" "logFilePriority" of
              Left e  -> do putStrLn $ "Invalid logFilePriority; " ++
                                       "using default. " ++ show(e)
                            return config
              Right x -> return (config { logFilePrio = str2logPriority x })
  return config

------------------------------------------------------------------------------
--
-- Helper functions
--
------------------------------------------------------------------------------

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

mainBody = do
  rawArgs <- getArgs
  let (args, options, errors) = getOpt RequireOrder argSpec rawArgs
  unless (null errors) $ do { mapM_ putStrLn errors; fail "bad args" }
  EKG.forkServer (B.pack "localhost") 8000 >> action args

shutdown = do
  return ()

main = mainBody `finally` shutdown
