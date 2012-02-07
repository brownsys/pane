{-# LANGUAGE ForeignFunctionInterface #-}

module Server where

import Control.Monad (unless)
import Network.Socket hiding (recv)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (recv, send)
import Data.Word
import Data.List (span)
import System.IO
import Parser hiding (tick)
import FlowControllerLang
import FlowController (State, stateNow, tick, stateSeqn)
import EmitFML
import Control.Concurrent
import Data.IORef
import Base
import Control.Concurrent.MVar
import Nettle.OpenFlow
import System.Time

foreign import ccall unsafe "htons" htons :: Word16 -> Word16

serverLoop serverSock state actionsVar = do
  (clientSock, _) <- accept serverSock
  forkIO (authUser clientSock state actionsVar)
  serverLoop serverSock state actionsVar

serverMain :: MVar Shared -> Word16 -> State -> IO ()
serverMain actionsVar port state = withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet (PortNum (htons port)) iNADDR_ANY)
    listen sock 2
    stateRef <- newMVar state    
    forkIO (tickThread stateRef actionsVar)
    serverLoop sock stateRef actionsVar

tickThread :: MVar State -> MVar Shared -> IO ()
tickThread stVar actionsVar = forever $ do
  st <- takeMVar stVar
  (TOD now _) <- getClockTime
  let delta = now - stateNow st -- seconds
  case delta > 0 of
    True -> do
      let st' = tick delta st
      putMVar stVar st'
      case stateSeqn st /= stateSeqn st' of
        True -> putMVar actionsVar (emitActions st')
        False -> return ()
    False -> do
      putMVar stVar st
  threadDelay (5 * 10^5)
  
  


serverAction actionsVar conn cmd stRef = do
  st <- takeMVar stRef -- block if empty
  (result, st) <- runDNP cmd st
  case result of
    BoolResult True -> do
      putStrLn "--> ACCEPTED"
      (t, _) <- runDNP getTimeM st
      putStrLn ("--> BEGIN NEW FML CONFIGURATION. TIME = " ++ (show t))
      putStrLn (emitFML st)
      putStrLn "--> END NEW FML CONFIGURATION"
      putMVar actionsVar (emitActions st)
    BoolResult False -> do
      putStrLn "--> REJECTED"
    otherwise -> do
      putStrLn "Response returned"
  Network.Socket.send conn (show result)
  putMVar stRef st -- we have it, so will never block
  return stRef

authUser conn st actionsVar = do
   -- TODO: what if command longer than 1024? or falls over a boundary?
  msg <- recv conn 1024
  let msgStr = C.unpack msg
  let (spk, _:restMsg)  = span (/='.') msgStr
  processLoop spk conn st restMsg actionsVar

processLoop spk conn st msg actionsVar = do
  putStr (spk ++ " : ")
  putStrLn msg
  st' <- parseInteractive' spk msg (serverAction actionsVar conn) st
  msg' <- recv conn 1024
  if S.null msg'
    then do putStrLn (spk ++ " disconnected")
    else do processLoop spk conn st' (C.unpack msg') actionsVar
