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
import System.IO.Unsafe
  

-- map of ports to destinations
learnedRoutes :: IORef (Map IPAddress PortID)
learnedRoutes = unsafePerformIO (newIORef Map.empty)

nextQueueID :: IORef QueueID
nextQueueID = unsafePerformIO (newIORef 0)

newQueueID :: IO QueueID
newQueueID = do
  n <- readIORef nextQueueID
  writeIORef nextQueueID (n+1)
  return n


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
           -> IO ()
reconfLoop ofpServer switches configMsgsVar = do
  (admMsgs, resvs) <- takeMVar configMsgsVar
  sws <- readIORef switches
  -- Setup new allow/deny rules
  putStrLn $ "Sending allow/deny messages to " ++ (show sws)
  mapM_ (\f -> putStrLn (show f)) admMsgs
  let setAdm sw = do
        mapM_ (\m -> putStrLn "Sending" >> sendToSwitchWithID ofpServer sw m) (zip (repeat 0) admMsgs)
  mapM_ setAdm (Set.toList sws)
  -- Create new queues
  let createQueue sw (match, size, expiry) = 
        case prefixIsExact (dstIPAddress match) of
          False -> putStrLn "inexact dstAddress: cannot create queue"
          True -> do
            routes <- readIORef learnedRoutes
            case Map.lookup (fst (dstIPAddress match)) routes of
              Nothing -> putStrLn "dst IP addr not learned: cannot create queue"
              Just dstPort -> do
                qid <- newQueueID
                let cfg = ExtQueueModify dstPort
                            [QueueConfig qid [MinRateQueue (Enabled size)]]
                sendToSwitchWithID ofpServer sw (11, cfg)
                case expiry of
                  NoLimit -> return ()
                  (DiscreteLimit n) -> do
                    let deleteQueue = do
                           threadDelay ((fromIntegral n) * 10^6)
                           putStrLn "deleting queue..."
                           let cfg = ExtQueueDelete dstPort
                                       [QueueConfig qid []]
                           sendToSwitchWithID ofpServer sw (12, cfg)
                    forkIO deleteQueue
                    return ()
  mapM_ (\sw -> mapM_ (createQueue sw) resvs) (Set.toList sws)
  -- Delete expiring queues
  -- TODO:
  -- 
  reconfLoop ofpServer switches configMsgsVar


reconfThread ofServ swsVar sharedVar = do
  
  reconfLoop ofServ swsVar sharedVar

handleSwitch :: SwitchHandle -> IO ()
handleSwitch switch = do
  putStrLn $ "New switch " ++ show (handle2SwitchID switch)
  let cfg = ExtQueueModify 1 [QueueConfig 3 [MinRateQueue (Enabled 700)]]
  sendToSwitch switch (10, cfg)
  untilNothing (receiveFromSwitch switch) 
               (messageHandler switch)
  closeSwitchHandle switch


getPacketRoute pkt = case enclosedFrame pkt of
  Right (HCons _ (HCons (IPInEthernet (HCons ipHdr _)) HNil)) -> 
    Just (srcPort, srcIP, dstIP)
      where srcPort = receivedOnPort pkt
            srcIP = ipSrcAddress ipHdr
            dstIP = ipDstAddress ipHdr
  otherwise -> Nothing

messageHandler :: SwitchHandle -> (TransactionID, SCMessage) -> IO ()
messageHandler switch (xid, scmsg) = case scmsg of
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
      Left str -> putStrLn ("PacketIn with an unrecognized frame:" ++ str)
    Just (srcPort, srcIP, dstIP) -> do
      -- learn
      oldRoutes <- readIORef learnedRoutes
      let routes = Map.insert srcIP srcPort oldRoutes
      writeIORef learnedRoutes routes
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

