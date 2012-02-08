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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IORef

controllerMain :: MVar Shared -> Word16 -> IO ()
controllerMain paneConfigState portNum = do
  ofpServer <- startOpenFlowServer Nothing portNum
  swsVar <- newIORef Set.empty
  forkIO (reconfThread ofpServer swsVar paneConfigState) 
  forever (do (switch,sfr) <- acceptSwitch ofpServer
              sws <- readIORef swsVar
              writeIORef swsVar (Set.insert (handle2SwitchID switch) sws) 
              forkIO (handleSwitch switch))
  closeServer ofpServer

reconfLoop :: OpenFlowServer 
           -> IORef (Set SwitchID)
           -> MVar Shared 
           -> Map SwitchID (PQ (Integer, PortID, QueueID))
           -> IO ()
reconfLoop ofpServer switches configMsgsVar endingResvs =  do
  (admMsgs, resvs) <- takeMVar configMsgsVar
  sws <- readIORef switches
  -- Setup new allow/deny rules
  putStrLn $ "Sending allow/deny messages to " ++ (show sws)
  mapM_ (\f -> putStrLn (show f)) admMsgs
  let setAdm sw = do
        mapM_ (\m -> putStrLn "Sending" >> sendToSwitchWithID ofpServer sw m) (zip (repeat 0) admMsgs)
  mapM_ setAdm (Set.toList sws)
  -- Create new queues
  -- TODO: 
  -- Delete expiring queues
  -- TODO:
  -- 
  reconfLoop ofpServer switches configMsgsVar endingResvs


reconfThread ofServ swsVar sharedVar = reconfLoop ofServ swsVar sharedVar Map.empty

handleSwitch :: SwitchHandle -> IO ()
handleSwitch switch = do
  putStrLn $ "New switch " ++ show (handle2SwitchID switch)
  -- map of ports to destinations
  learnedRoutes <- newIORef Map.empty
  let cfg = ExtQueueModify 1 [QueueConfig 3 [MinRateQueue (Enabled 700)]]
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
  PacketIn pkt -> putStr "PacketIn ... " >> case getPacketRoute pkt of
    Nothing -> case enclosedFrame pkt of
      Right frm -> do
        putStrLn "unrecognized frame type (or ARP) ... flooding."
        let flowEntry = AddFlow { 
                          match = frameToExactMatch (receivedOnPort pkt) frm,
                          priority          = 0, -- lowest priority for short floods
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
      let (action, timeOut) = case Map.lookup dstIP routes of
                     Just dstPort -> ([SendOutPort (PhysicalPort dstPort)], ExpireAfter 60)
                     Nothing -> ([SendOutPort Flood], ExpireAfter 5)
      let flowEntry = AddFlow { 
                        match = matchAny {
                          inPort = Just srcPort, 
                          srcIPAddress = (srcIP, maxPrefixLen),
                          dstIPAddress = (dstIP, maxPrefixLen),
                          ethFrameType = Just ethTypeIP
                        },
                        priority          = 1, -- 2nd lowest priority for learning
                        actions           = action,
                        cookie            = 0,
                        idleTimeOut       = ExpireAfter 5,
                        hardTimeOut       = timeOut,
                        notifyWhenRemoved = False,
                        applyToPacket     = bufferID pkt,
                        overlapAllowed    = True
                      }
      putStrLn $ "IP packet... action: " ++ (show action) ++ " timeout: " ++ show timeOut
      sendToSwitch switch (xid, FlowMod flowEntry)
  otherwise -> do
    putStrLn $ "unhandled packet " ++ show scmsg

