module Main where

import System.Environment
import Parser

main = do
  args <- getArgs
  let filename = args !! 0
  stmts <- parseFromFile filename
  putStrLn (show stmts)
