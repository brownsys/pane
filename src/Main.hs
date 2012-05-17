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

data Argument
  = Test String
  | NewServer Word16
  | Help

argSpec =
  [ Option ['t'] ["test"] (ReqArg Test "FILE") "run test case"
  , Option ['p'] ["port"] (ReqArg (NewServer . read) "PORT") "port number for interaction server"
  , Option ['h'] ["help"] (NoArg Help) "print this help message"
  ]

runTestFile f = do
  runTests <- parseFromTestFile f
  runTests

timeService = do
  time <- newChan
  forkIO $ forever $ do
    threadDelay (10^6) -- 1 second
    (TOD now _) <- getClockTime
    writeChan time now
    writeIORef sysTime now
  (TOD initNow _) <- getClockTime
  writeIORef sysTime initNow
  return (initNow, time)

action [Test file] = runTestFile file
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
  nibSnapshot <- compilerService (nib, nibUpdates) tbl

  putStrLn "Starting OpenFlow controller ..."
  controller nibSnapshot nibMsg packetIn switches pktOut 6633
action [Help] = do
  putStrLn $ usageInfo "WELCOME TO PANE" argSpec
action [] = do
  putStrLn $ usageInfo "WELCOME TO PANE" argSpec
  fail "too few args"
action _ = do
  putStrLn $ usageInfo "WELCOME TO PANE" argSpec
  fail "too many args"

mainBody = do
  rawArgs <- getArgs
  let (args, options, errors) = getOpt RequireOrder argSpec rawArgs
  unless (null errors) $ do { mapM_ putStrLn errors; fail "bad args" }
  action args

shutdown = do
  return ()

main = mainBody `finally` shutdown
