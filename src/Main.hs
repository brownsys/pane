module Main where

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
import FlowControllerLang
import FlowController (emptyStateWithTime, State)
import Test.HUnit hiding (State)
import Server
import Controller
import Base
import Control.Concurrent.MVar
import Control.Concurrent
import System.Time
import qualified NIB
import ControllerService
import OFCompiler (compilerService)
import CombinedPaneMac
import PaneInteractionServer (interactions)

data Argument
  = Trace String
  | Test String
  | Interactive String
  | Server Word16
  | NewServer Word16

argSpec =
  [ Option ['t'] ["trace"] (ReqArg Trace "FILE") "trace file"
  , Option ['f'] ["test"] (ReqArg Test "FILE") "run test case"
  , Option ['s'] ["server"] (ReqArg (Server . read) "PORT") "server"
  , Option ['n'] [] (ReqArg (NewServer . read) "PORT") "new server"
  ]

runTestFile f = do
  runTests <- parseFromTestFile f
  runTests

doTrace (Trace path:rest) = do
  handle <- openFile path WriteMode
  writeIORef traceFile (Just handle)
  return rest
doTrace lst = return lst

timeService = do
  time <- newChan
  forkIO $ forever $ do
    threadDelay (10^6) -- 1 second
    (TOD now _) <- getClockTime
    writeChan time now
  (TOD initNow _) <- getClockTime
  return (initNow, time)

action [Test file] = runTestFile file
action [Server port] = do
  (TOD now _) <- getClockTime
  let initialState = emptyStateWithTime now
  paneConfigVar <- newEmptyMVar
  terminate <- newEmptyMVar
  let terminator body = do
        body `finally` (putMVar terminate ())
  tid1 <- forkIO (terminator $ serverMain paneConfigVar port initialState)
  tid2 <- forkIO (terminator $ controllerMain paneConfigVar 6633)
  takeMVar terminate
action [NewServer port] = do
  putStrLn "Starting PANE  ..."
  (initTime, time) <- timeService
  nibMsg <- newChan
  putStrLn "Creating empty NIB..."
  nib <- NIB.newEmptyNIB nibMsg
  packetIn <- newChan
  switches <- newChan
  paneReq <- newChan
  putStrLn "Creating PANE + MAC Learning system..."
  (tbl, paneResp, pktOut) <- combinedPaneMac switches packetIn paneReq time
  putStrLn $ "Starting PANE console on port " ++ show port ++ " ..."
  interactions port paneResp paneReq
  nibUpdates <- newChan -- TODO(arjun): write into this
  putStrLn "Starting compiler ..."
  netSnapshot <- compilerService (nib, nibUpdates) tbl
  putStrLn "Starting OpenFlow controller ..."
  controller netSnapshot nibMsg packetIn switches pktOut 6633
action _ = fail "too many args"

mainBody = do
  rawArgs <- getArgs
  let (args, options, errors) = getOpt RequireOrder argSpec rawArgs
  unless (null errors) $ do { mapM_ putStrLn errors; fail "bad args" }
  args <- doTrace args
  action args

shutdown = do
  maybeHandle <- readIORef traceFile
  case maybeHandle of
    Nothing -> return ()
    Just h -> hClose h

main = mainBody `finally` shutdown
