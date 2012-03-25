module PaneInteractionServer
  ( interactions
  )  where

import Control.Monad (unless)
import Network
import Data.List (span)
import System.IO
import Parser hiding (tick)
import Base
import Nettle.OpenFlow
import System.Time

interactions :: Word16
             -> Chan (Speaker, String)
             -> Chan (Speaker, String)
             -> IO ()
interactions port toClient fromClient = do
  sock <- listenOn (PortNumber $ fromIntegral port)
  forkIO $ forever $ do
    (h, _, _) <- accept sock
    hSetBuffering h LineBuffering
    toClient <- dupChan toClient
    forkIO (authUser h fromClient toClient)
  return ()
    
authUser conn fromClient toClient = do
  msg <- hGetLine conn
  let (spk, _)  = span (/='.') msg
  forkIO $ forever $ do
    (spk', msg) <- readChan toClient
    when (spk == spk') $ do
      hPutStrLn conn msg
      return ()
  forever $ do
    msg <- hGetLine conn
    putStrLn $ spk ++ ": " ++ show msg
    writeChan fromClient (spk, msg)
