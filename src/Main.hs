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

data Argument
  = Trace String
  | Test String
  | Interactive String
  | Server Word16

argSpec =
  [ Option ['t'] ["trace"] (ReqArg Trace "FILE") "trace file"
  , Option ['f'] ["test"] (ReqArg Test "FILE") "run test case"
  , Option ['s'] ["server"] (ReqArg (Server . read) "PORT") "server"
  ]

runTestFile f = do
  runTests <- parseFromTestFile f
  runTests

doTrace (Trace path:rest) = do
  handle <- openFile path WriteMode
  writeIORef traceFile (Just handle)
  return rest
doTrace lst = return lst

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
