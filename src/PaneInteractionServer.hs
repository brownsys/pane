module PaneInteractionServer
  ( interactions
  ) where

import Control.Monad (unless)
import Network
import Data.List (span)
import System.IO
import Parser
import Base
import Nettle.OpenFlow
import System.Time
import System.Log.Logger.TH (deriveLoggers)
import qualified System.Log.Logger as Logger

$(deriveLoggers "Logger" [Logger.INFO])

interactions :: Word16
             -> Chan (Speaker, Integer, String)
             -> Chan (Speaker, Integer, String)
             -> IO ()
interactions port toClient fromClient = do
  sock <- listenOn (PortNumber $ fromIntegral port)
  clientIdCounter <- newMVar 0
  -- no-op reader of the original copy of the toClient channel
  forkIO $ forever $ do
    readChan toClient
  -- accept new clients
  forkIO $ forever $ do
    (h, _, _) <- accept sock
    clientId <- takeMVar clientIdCounter
    putMVar clientIdCounter (clientId + 1)
    hSetBuffering h LineBuffering
    toClient <- dupChan toClient
    forkIO (handleUser h clientId fromClient toClient)
  return ()
    
handleUser conn clientId fromClient toClient = do
--  logo (hPutStrLn conn)
  hPutStr conn "Login: "
  hFlush conn
  msg <- hGetLine conn
  let (tmp1, _) = span (/='.') msg
  let (tmp2, _) = span (/='\r') tmp1
  let (spk, _) = span (/='\n') tmp2
  writeChan toClient (spk, clientId, "logged in")
  -- monitor the toClient bus for messages to this client
  forkIO $ forever $ do
    (spk', id', msg) <- readChan toClient
    when (spk == spk' && clientId == id') $ do
      hPutStrLn conn msg
      hPutStr conn (spk ++ "> ")
      hFlush conn
      return ()
  -- read commands from user and place on fromClient bus
  forever $ do
    msg <- hGetLine conn
    infoM $ spk ++ ": " ++ show msg
    writeChan fromClient (spk, clientId, msg)
