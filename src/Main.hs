module Main where

import System.Console.GetOpt
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
import qualified StateDump
import Base

data Argument
  = Trace
  | Test String
  | Interactive String
  | Server Word16

argSpec =
  [ Option ['t'] ["trace"] (NoArg Trace) "trace state"
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
  test <- parseFromTestFile f
  runTestTT test
  return ()

doTrace (Trace:rest) = do
  writeIORef isTracing True
  return rest
doTrace lst = return lst

action [Test file] = runTestFile file
action [Interactive spk] = loop spk emptyState
action [Server port] = serverMain port emptyState
action _ = fail "too many args"

main = do
  rawArgs <- getArgs
  let (args, options, errors) = getOpt RequireOrder argSpec rawArgs
  unless (null errors) $ do { mapM_ (putStrLn.show) errors; fail "bad args" }
  args <- doTrace args
  action args
