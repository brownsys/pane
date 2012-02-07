-- |PANE's OpenFlow controller
module Controller where

import Nettle.Servers.Server
import Nettle.OpenFlow
import Data.Word
import Control.Concurrent
import qualified Data.ByteString as B
import System.Environment
import Nettle.OpenFlow.StrictPut -- as Put
import FlowController (State)
import Control.Concurrent.MVar
import Base
import Data.HashTable

controllerMain :: MVar [CSMessage] -> Word16 -> IO ()
controllerMain paneConfigState portNum = do
  ofpServer <- startOpenFlowServer Nothing portNum
  forkIO (installActionsLoop ofpServer paneConfigState) 
  forever (do (switch,sfr) <- acceptSwitch ofpServer
              forkIO (handleSwitch switch))
  closeServer ofpServer

installActionsLoop :: OpenFlowServer -> MVar [CSMessage] -> IO ()
installActionsLoop ofpServer configMsgsVar = forever $ do
  cfgMsgs <- takeMVar configMsgsVar
  let msgs = zip (repeat 0) cfgMsgs
  let send sw = mapM_ (sendToSwitch sw) msgs
  sws <- getSwitches ofpServer
  mapM_ send sws


handleSwitch :: SwitchHandle -> IO ()
handleSwitch switch = do
  let cfg = ExtQueueModify 1 [QueueConfig 6 [MinRateQueue (Enabled 500)]]
  sendToSwitch switch (10, cfg)
  untilNothing (receiveFromSwitch switch) (messageHandler switch)
  closeSwitchHandle switch


messageHandler :: SwitchHandle -> (TransactionID, SCMessage) -> IO ()
messageHandler switch (xid, scmsg) = do
  putStrLn $ "Received: " ++ show scmsg
  case scmsg of
    PacketIn pkt        -> 
      case enclosedFrame pkt of 
        Left s -> putStrLn (s ++ ": " ++ show (B.unpack (packetData pkt)))
        Right frame -> 
          let flowEntry = AddFlow { match             = frameToExactMatch (receivedOnPort pkt) frame
                                  , priority          = 32768
                                  , actions           = [SendOutPort Flood]
                                  , cookie            = 0
                                  , idleTimeOut       = ExpireAfter 5
                                  , hardTimeOut       = ExpireAfter 5
                                  , notifyWhenRemoved = False
                                  , applyToPacket     = bufferID pkt
                                  , overlapAllowed    = True
                                  } 
          in sendToSwitch switch (xid, FlowMod flowEntry)
    _                   -> return ()

