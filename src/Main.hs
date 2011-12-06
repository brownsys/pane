module Main where

import System.Environment
import Parser
import FlowControllerLang
import FlowController (emptyState, State)
 
loop :: State -> IO a
loop st = do
  cmd <- parseStmtFromStdin
  let (b, st') = runDNP' cmd st
  case b of
    True -> loop st'
    False -> do
      putStrLn "epic fail whale"
      loop st'

main = loop emptyState

{- 
main = do
  args <- getArgs
  let filename = args !! 0
  dnp <- parseFromFile filename
  putStrLn (show (runDNP dnp))
--  putStrLn (show stmts)
-}
