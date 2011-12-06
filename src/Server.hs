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

-- serverLoop :: Socket -> State -> IO a
serverLoop sock st = do
  (conn, _) <- accept sock
  msg <- recv conn 1024 -- TODO: what if command longer than 1024?
  sClose conn
  case S.null msg of
    True -> do
      serverLoop sock st
    False -> do
      let spk = "root" -- TODO: Obviously, something else is needed here
      putStr (spk ++ " : ")
      C.putStrLn msg
      cmd <- parseStmtFromString spk (C.unpack msg)
      let (b, st') = runDNP cmd st
      case b of
        True -> do
          putStrLn "--> ACCEPTED"
          putStrLn "--> BEGIN NEW FML CONFIGURATION"
          putStrLn (emitFML st')
          putStrLn "--> END NEW FML CONFIGURATION"
          serverLoop sock st'
        False -> do
          putStrLn "--> REJECTED"
          serverLoop sock st'

serverMain :: Word16 -> State -> IO ()
serverMain port state = withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    -- bindSocket sock (SockAddrInet (PortNum port) iNADDR_ANY)
    listen sock 2
    serverLoop sock state


{-
       (conn, _) <- accept sock
       talk conn
       sClose conn
       sClose sock

    where
      talk :: Socket -> IO ()
      talk conn =
          do msg <- recv conn 1024
             unless (S.null msg) $ sendAll conn msg >> talk conn
-}

