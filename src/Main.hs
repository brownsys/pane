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
import FlowController (emptyState, State)
import Test.HUnit hiding (State)
import Server
import Base

data Argument
  = Trace String
  | Test String
  | Interactive String
  | Server Word16

argSpec =
  [ Option ['t'] ["trace"] (ReqArg Trace "FILE") "trace file"
  , Option ['f'] ["test"] (ReqArg Test "FILE") "run test case"
  , Option ['i'] ["interactive"] (ReqArg Interactive "SPEAKER") "???"
  , Option ['s'] ["server"] (ReqArg (Server . read) "PORT") "server"
  ]


loop :: String -> State -> IO a
loop spk st = do
  putStr (spk ++ "$ ")
  hFlush stdout
  cmd <- parseStmtFromStdin spk
  (b, st') <- runDNP cmd st
  case b of
    True -> loop spk st'
    False -> do
      putStrLn "epic fail whale"
      loop spk st'

runTestFile f = do
  runTests <- parseFromTestFile f
  runTests

doTrace (Trace path:rest) = do
  handle <- openFile path WriteMode
  writeIORef traceFile (Just handle)
  return rest
doTrace lst = return lst

action [Test file] = runTestFile file
action [Interactive spk] = loop spk emptyState
action [Server port] = serverMain port emptyState
action _ = fail "too many args"

mainBody = do
  rawArgs <- getArgs
  let (args, options, errors) = getOpt RequireOrder argSpec rawArgs
  unless (null errors) $ do { mapM_ (putStrLn.show) errors; fail "bad args" }
  args <- doTrace args
  action args

shutdown = do
  maybeHandle <- readIORef traceFile
  case maybeHandle of
    Nothing -> return ()
    Just h -> hClose h

main = mainBody `finally` shutdown
