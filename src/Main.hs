module Main where

import System.Environment
import System.IO
import Data.Word
import Parser
import FlowControllerLang
import FlowController (emptyState, State)
import Test.HUnit hiding (State)
import Server

loop :: String -> State -> IO a
loop spk st = do
  putStr (spk ++ "$ ")
  hFlush stdout
  cmd <- parseStmtFromStdin spk
  let (b, st') = runDNP cmd st
  case b of
    True -> loop spk st'
    False -> do
      putStrLn "epic fail whale"
      loop spk st'

runTestFile f = do
  test <- parseFromTestFile f
  runTestTT test
  return ()

main = do
  args <- getArgs

  case args !! 0 of
    "-f" -> runTestFile (args !! 1)
    "-i" -> loop (args !! 1) emptyState
    "-s" -> serverMain (read (args !! 1) :: Word16) emptyState
    _ -> putStrLn "requires two arguments"
