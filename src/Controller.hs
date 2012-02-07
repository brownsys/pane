-- |PANE's OpenFlow controller
module Controller where

import Nettle.Servers.Server
import Nettle.OpenFlow
import Data.Word
import Control.Concurrent
import qualified Data.ByteString as B
import System.Environment
import Nettle.OpenFlow.StrictPut
import FlowController (State)
import Control.Concurrent.MVar
import Base
import Data.HashTable
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.HList
import qualified PriorityQueue as PQ
import PriorityQueue (PQ)

controllerMain :: MVar Shared -> Word16 -> IO ()
controllerMain paneConfigState portNum = do
  ofpServer <- startOpenFlowServer Nothing portNum
  forkIO (reconfThread ofpServer paneConfigState) 
  forever (do (switch,sfr) <- acceptSwitch ofpServer
              forkIO (handleSwitch switch))
  closeServer ofpServer

reconfLoop :: OpenFlowServer 
           -> MVar Shared 
           -> Map SwitchID (PQ (Integer, PortID, QueueID))
           -> IO ()
reconfLoop ofpServer configMsgsVar endingResvs =  do
  sws <- getSwitches ofpServer
  (admMsgs, resvs) <- takeMVar configMsgsVar
  -- Setup new allow/deny rules
  let setAdm sw = mapM_ (sendToSwitch sw) (zip (repeat 0) admMsgs)
  mapM_ setAdm sws
  -- Create new queues
  -- TODO: 
  -- Delete expiring queues
  -- TODO:
  -- 
  reconfLoop ofpServer configMsgsVar endingResvs


reconfThread ofServ sharedVar = reconfLoop ofServ sharedVar Map.empty

handleSwitch :: SwitchHandle -> IO ()
handleSwitch switch = do
  -- map of ports to destinations
  learnedRoutes <- newIORef Map.empty
  
  let cfg = ExtQueueModify 1 [QueueConfig 3 [MinRateQueue (Enabled 700)],
                              QueueConfig 6 [MinRateQueue (Enabled 500)]]
  sendToSwitch switch (10, cfg)
  untilNothing (receiveFromSwitch switch) (messageHandler learnedRoutes switch)
  closeSwitchHandle switch


getPacketRoute pkt = case enclosedFrame pkt of
  Right (HCons _ (HCons (IPInEthernet (HCons ipHdr _)) HNil)) -> 
    Just (srcPort, srcIP, dstIP)
      where srcPort = receivedOnPort pkt
            srcIP = ipSrcAddress ipHdr
            dstIP = ipDstAddress ipHdr
  otherwise -> Nothing

messageHandler :: IORef (Map IPAddress PortID)
               -> SwitchHandle -> (TransactionID, SCMessage) -> IO ()
messageHandler routesRef switch (xid, scmsg) = case scmsg of
  PacketIn pkt -> case getPacketRoute pkt of
    Nothing -> case enclosedFrame pkt of
      Right frm -> do
        let flowEntry = AddFlow { 
                          match = frameToExactMatch (receivedOnPort pkt) frm,
                          priority          = 32767, -- not lowest priority
                          actions           = [SendOutPort Flood],
                          cookie            = 0,
                          idleTimeOut       = ExpireAfter 5,
                          hardTimeOut       = ExpireAfter 5,
                          notifyWhenRemoved = False,
                          applyToPacket     = bufferID pkt,
                          overlapAllowed    = True
                        }
        sendToSwitch switch (xid, FlowMod flowEntry)
      Left _ -> putStrLn "PacketIn with an unrecognized frame."
    Just (srcPort, srcIP, dstIP) -> do
      -- learn
      oldRoutes <- readIORef routesRef
      let routes = Map.insert srcIP srcPort oldRoutes
      writeIORef routesRef routes
      -- lookup route
      let action = case Map.lookup dstIP routes of
                     Just dstPort -> [SendOutPort (PhysicalPort dstPort)]
                     Nothing -> [SendOutPort Flood]
      let flowEntry = AddFlow { 
                        match = matchAny {
                          inPort = Just srcPort, 
                          srcIPAddress = (srcIP, maxPrefixLen),
                          dstIPAddress = (dstIP, maxPrefixLen),
                          ethFrameType = Just ethTypeIP
                        },
                        priority          = 32768, -- lowest priority
                        actions           = action,
                        cookie            = 0,
                        idleTimeOut       = ExpireAfter 10,
                        hardTimeOut       = ExpireAfter 60,
                        notifyWhenRemoved = False,
                        applyToPacket     = bufferID pkt,
                        overlapAllowed    = True
                      }
      sendToSwitch switch (xid, FlowMod flowEntry)
  otherwise -> do
    putStrLn $ "unhandled packet " ++ show scmsg

