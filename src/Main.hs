module Main where

import System.Environment
import System.IO
import Parser
import FlowControllerLang
import FlowController (emptyState, State)

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
  (res, dnp) <- parseFromTestFile f
  putStrLn (show (evalDNP dnp))
  putStrLn (show res)

main = do
  args <- getArgs

  case args !! 0 of
    "-f" -> runTestFile (args !! 1)
    "-i" -> loop (args !! 1) emptyState
    _ -> putStrLn "requires two arguments"
