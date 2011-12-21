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
import Parser
import FlowControllerLang
import FlowController (State)
import EmitFML
import Control.Concurrent
import Data.IORef
import Base

foreign import ccall unsafe "htons" htons :: Word16 -> Word16

serverLoop serverSock state = do
  (clientSock, _) <- accept serverSock
  forkIO (authUser clientSock state)
  serverLoop serverSock state 

serverMain :: Word16 -> State -> IO ()
serverMain port state = withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet (PortNum (htons port)) iNADDR_ANY)
    listen sock 2
    stateRef <- newMVar state    
    serverLoop sock stateRef


serverAction conn cmd stRef = do
  st <- takeMVar stRef -- block if empty
  (result, st) <- runDNP cmd st
  case result of
    BoolResult True -> do
      putStrLn "--> ACCEPTED"
      (t, _) <- runDNP getTimeM st
      putStrLn ("--> BEGIN NEW FML CONFIGURATION. TIME = " ++ (show t))
      putStrLn (emitFML st)
      putStrLn "--> END NEW FML CONFIGURATION"
    BoolResult False -> do
      putStrLn "--> REJECTED"
    otherwise -> do
      putStrLn "Response returned"
  Network.Socket.send conn (show result)
  putMVar stRef st -- we have it, so will never block
  return stRef

authUser conn st = do
   -- TODO: what if command longer than 1024? or falls over a boundary?
  msg <- recv conn 1024
  let msgStr = C.unpack msg
  let (spk, _:restMsg)  = span (/='.') msgStr
  processLoop spk conn st restMsg

processLoop spk conn st msg = do
  putStr (spk ++ " : ")
  putStrLn msg
  st' <- parseInteractive' spk msg (serverAction conn) st
  msg' <- recv conn 1024
  processLoop spk conn st' (C.unpack msg')

