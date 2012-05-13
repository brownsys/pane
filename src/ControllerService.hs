module ControllerService
  ( controller
  , PacketIn
  ) where

import Prelude hiding (catch)
import Base
import System.Time
import Data.Map (Map)
import MacLearning (PacketOutChan)
import qualified NIB
import qualified Nettle.OpenFlow as OF
import qualified Nettle.Servers.Server as OFS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

type PacketIn = (OF.TransactionID, Integer, OF.SwitchID, OF.PacketInfo)

retryOnExns :: IO a -> IO a
retryOnExns action = action `catch` handle
  where handle (e :: SomeException) = do
          putStrLn $ "Exception (retrying): " ++ show e
          retryOnExns action

ignoreExns :: IO () -> IO ()
ignoreExns action = action `catch` handle
  where handle (e :: SomeException) = do
          putStrLn $ "Exception (ignoring): " ++ show e

controller :: Chan NIB.Snapshot
           -> Chan NIB.Msg
           -> Chan PacketIn
           -> Chan (OF.SwitchID, Bool)
           -> PacketOutChan
           -> Word16 
           -> IO ()
controller netSnapshot toNIB packets switches pktOut port = do
  server <- OFS.startOpenFlowServer Nothing port
  forkIO $ forever $ do
    (swID, xid, pktOut) <- readChan pktOut
    -- putStrLn $ "SEND packet-out" ++ show (OF.bufferIDData pktOut)
    OFS.sendToSwitchWithID server swID (xid, OF.PacketOut pktOut)
  forever $ do
    (switch, switchFeatures) <- OFS.acceptSwitch server
    putStrLn $ "OpenFlow controller connected to new switch."
    writeChan toNIB (NIB.NewSwitch switch switchFeatures)
    netSnapshot <- dupChan netSnapshot
    writeChan switches (OFS.handle2SwitchID switch, True)
    forkIO (handleSwitch packets toNIB switches switch)
    forkIO (configureSwitch netSnapshot switch NIB.emptySwitch)
  OFS.closeServer server

handleSwitch :: Chan PacketIn
             -> Chan NIB.Msg
             -> Chan (OF.SwitchID, Bool)
             -> OFS.SwitchHandle
             -> IO ()
handleSwitch packets toNIB switches switch = do
  let swID = OFS.handle2SwitchID switch
  OFS.untilNothing 
    (retryOnExns (OFS.receiveFromSwitch switch))
    (\msg -> ignoreExns (messageHandler packets toNIB switch msg))
  OFS.closeSwitchHandle switch
  writeChan switches (swID, False)
  putStrLn $ "Connection to switch " ++ show swID ++ " closed."

messageHandler :: Chan PacketIn
               -> Chan NIB.Msg
               -> OFS.SwitchHandle
               -> (OF.TransactionID, OF.SCMessage)
               -> IO ()
messageHandler packets toNIB switch (xid, msg) = case msg of
  OF.PacketIn pkt -> do
    (TOD now _) <- getClockTime
    writeChan packets (xid, now, OFS.handle2SwitchID switch, pkt)
    writeChan toNIB (NIB.PacketIn (OFS.handle2SwitchID switch) pkt)
  otherwise -> do
    -- putStrLn $ "unhandled message from switch " ++ 
    --             (show $ OFS.handle2SwitchID switch) -- ++ "\n" ++ show msg
    return ()

configureSwitch :: Chan NIB.Snapshot
                -> OFS.SwitchHandle
                -> NIB.Switch
                -> IO ()
configureSwitch netSnapshot switchHandle oldSw@(NIB.Switch oldPorts oldTbl) = do
  let switchID = OFS.handle2SwitchID switchHandle
  snapshot <- readChan netSnapshot
  case Map.lookup switchID snapshot of
    Nothing -> do
      putStrLn $ "configureSwitch did not find " ++ show switchID ++ 
                 " in the NIB snapshot."
      configureSwitch netSnapshot switchHandle oldSw
    Just sw@(NIB.Switch ports tbl) -> do
      (TOD now _) <- getClockTime
      let (deleteQueueTimers, msgs') = mkPortMods now oldPorts ports 
                                         (OFS.sendToSwitch switchHandle)
      let msgs = mkFlowMods now tbl oldTbl ++ msgs'
      unless (null msgs) $ do
         putStrLn $ "OpenFlow controller modifying tables on " ++ show switchID
         putStrLn $ "sending " ++ show (length msgs) ++ " messages; oldTbl size = " ++ show (Set.size oldTbl) ++ " tbl size = " ++ show (Set.size tbl)
         mapM_ (\x -> putStrLn $ "   " ++ show x) msgs
         putStrLn "-------------------------------------------------"
         return ()
      mapM_ (OFS.sendToSwitch switchHandle) (zip [84 ..] msgs)
      deleteQueueTimers
      configureSwitch netSnapshot switchHandle sw

mkFlowMods :: Integer
           -> NIB.FlowTbl
           -> NIB.FlowTbl
           -> [OF.CSMessage]
mkFlowMods now newTbl oldTbl = map OF.FlowMod (delMsgs ++ addMsgs)
  where delMsgs = mapMaybe mkDelFlow (Set.toList oldRules)
        addMsgs = map mkAddFlow (Set.toList newRules) 
        mkAddFlow (prio, match, acts, expiry) = 
          OF.AddFlow {
            OF.match = match,
            OF.priority = prio,
            OF.actions = acts,
            OF.cookie = 0,
            OF.idleTimeOut = OF.Permanent,
            OF.hardTimeOut = toTimeout now expiry ,
            OF.notifyWhenRemoved = False,
            OF.applyToPacket = Nothing,
            OF.overlapAllowed = True
          }
        mkDelFlow (prio, match, _, expiry) = case expiry <= fromInteger now of
          True -> Nothing
          False -> Just (OF.DeleteExactFlow match Nothing prio)
        newRules = Set.difference newTbl oldTbl
        oldRules = Set.difference oldTbl newTbl

-- |We cannot have queues automatically expire with the slicing extension.
-- So, we return an action that sets up timers to delete queues.
mkPortMods :: Integer
           -> Map OF.PortID NIB.PortCfg
           -> Map OF.PortID NIB.PortCfg
           -> ((OF.TransactionID, OF.CSMessage) -> IO ())
           -> (IO (), [OF.CSMessage])
mkPortMods now portsNow portsNext sendCmd = (delTimers, addMsgs)
  where addMsgs = map newQueueMsg newQueues
        delTimers = sequence_ (map delQueueAction newQueues)
        newQueueMsg ((pid, qid), NIB.Queue resv _) =
          OF.ExtQueueModify pid 
            [OF.QueueConfig qid [OF.MinRateQueue (OF.Enabled resv)]]
        delQueueAction ((_, _), NIB.Queue _ NoLimit) = return ()
        delQueueAction ((pid, qid), NIB.Queue _ (DiscreteLimit end)) = do
          forkIO $ do
            threadDelay (10^6 * (fromIntegral $ end - now))
            sendCmd (85, OF.ExtQueueDelete pid [OF.QueueConfig qid []]) 
          return ()
        newQueues = Map.toList $
          flatten portsNext `Map.difference` flatten portsNow
        flatten portMap = Map.fromList $
          concatMap (\(pid, NIB.PortCfg qMap) ->
                      map (\(qid, q) -> ((pid, qid), q)) (Map.toList qMap))
                    (Map.toList portMap)
            
              
      
-- TODO(arjun): toTimeout will fail if (end - now) does not fit in a Word16
toTimeout :: Integer -> Limit -> OF.TimeOut
toTimeout _   NoLimit = 
  OF.Permanent
toTimeout now (DiscreteLimit end) = 
  OF.ExpireAfter (fromInteger (end - fromInteger now))
