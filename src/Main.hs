module Main where

import System.Environment
import Parser
import FlowControllerLang

main = do
  args <- getArgs
  let filename = args !! 0
  dnp <- parseFromFile filename
  putStrLn (show (runDNP dnp))
--  putStrLn (show stmts)
