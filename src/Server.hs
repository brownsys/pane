module Server where

import Control.Monad (unless)
import Network.Socket hiding (recv)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (recv, send)
import Data.Word

import System.IO
import Parser
import FlowControllerLang
import FlowController (State)
import EmitFML
import Control.Concurrent
import Data.IORef

-- serverLoop :: Socket -> State -> IO a
serverLoop serverSock state = do
  (clientSock, _) <- accept serverSock
  forkIO (processLoop clientSock state)
  serverLoop serverSock state 

serverMain :: Word16 -> State -> IO ()
serverMain port state = withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    -- bindSocket sock (SockAddrInet (PortNum port) iNADDR_ANY)
    listen sock 2
    stateRef <- newIORef state    
    serverLoop sock stateRef


serverAction cmd stRef = do
  (b, st') <- atomicModifyIORef stRef -- TODO: use a real lock so emitFML is inside as well
         (\st -> let (result, st') = runDNP cmd st in (st', (result, st')))
  case b of
    True -> do
      putStrLn "--> ACCEPTED"
      putStrLn "--> BEGIN NEW FML CONFIGURATION"
      putStrLn (emitFML st')
      putStrLn "--> END NEW FML CONFIGURATION"
    False -> do
      putStrLn "--> REJECTED"
  return stRef


processLoop conn st = do
  msg <- recv conn 1024 -- TODO: what if command longer than 1024? or falls over a boundary?
  case S.null msg of
    True -> do
      processLoop conn st
    False -> do
      let spk = "root" -- TODO: Obviously, something else is needed here
      putStr (spk ++ " : ")
      C.putStrLn msg
      st' <- parseInteractive' spk (C.unpack msg) serverAction st
      processLoop conn st'

