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

controller :: Chan NIB.Snapshot  -- ^input channel (from Compiler)
           -> Chan NIB.Msg       -- ^output channel (headed to NIB module)
           -> Chan PacketIn      -- ^output channel (headed to MAC Learning)
           -> Chan (OF.SwitchID, Bool) -- ^output channel (for MAC Learning;
                                       -- switches connecting & disconnecting)
           -> PacketOutChan      -- ^input channel (from MAC Learning)
           -> Word16 
           -> IO ()
controller nibSnapshot toNIB packets switches pktOut port = do
  server <- OFS.startOpenFlowServer Nothing port
  -- actually send packets sent by MAC learning module
  forkIO $ forever $ do
    (swID, xid, pktOut) <- readChan pktOut
    -- putStrLn $ "SEND packet-out" ++ show (OF.bufferIDData pktOut)
    OFS.sendToSwitchWithID server swID (xid, OF.PacketOut pktOut)
  -- process new switches
  forever $ do
    (switch, switchFeatures) <- OFS.acceptSwitch server
    putStrLn $ "OpenFlow controller connected to new switch."
    writeChan toNIB (NIB.NewSwitch switch switchFeatures)
    nibSnapshot <- dupChan nibSnapshot
    writeChan switches (OFS.handle2SwitchID switch, True)
    forkIO (handleSwitch packets toNIB switches switch)
    forkIO (configureSwitch nibSnapshot switch NIB.emptySwitch)
  OFS.closeServer server

--
-- Functions to handle messages from switches
-- 

handleSwitch :: Chan PacketIn  -- ^output channel (headed to MAC Learning)
             -> Chan NIB.Msg   -- ^output channel (headed to NIB module)
             -> Chan (OF.SwitchID, Bool) -- ^output channel (for MAC Learning;
                                         -- switches connecting & disconnecting)
             -> OFS.SwitchHandle
             -> IO ()
handleSwitch packets toNIB switches switch = do
  let swID = OFS.handle2SwitchID switch
  OFS.sendToSwitch switch (0, OF.FlowMod $ OF.DeleteFlows OF.matchAny Nothing)
  OFS.untilNothing 
    (retryOnExns (OFS.receiveFromSwitch switch))
    (\msg -> ignoreExns (messageHandler packets toNIB switch msg))
  OFS.closeSwitchHandle switch
  writeChan switches (swID, False)
  putStrLn $ "Connection to switch " ++ show swID ++ " closed."

messageHandler :: Chan PacketIn -- ^output channel (headed to MAC Learning)
               -> Chan NIB.Msg  -- ^output channel (headed to NIB module)
               -> OFS.SwitchHandle
               -> (OF.TransactionID, OF.SCMessage) -- ^coming from Nettle
               -> IO ()
messageHandler packets toNIB switch (xid, msg) = case msg of
  OF.PacketIn pkt -> do
    (TOD now _) <- getClockTime
    writeChan packets (xid, now, OFS.handle2SwitchID switch, pkt)
    writeChan toNIB (NIB.PacketIn (OFS.handle2SwitchID switch) pkt)
  otherwise -> do
    putStrLn $ "unhandled message from switch " ++ 
                (show $ OFS.handle2SwitchID switch) ++ "\n" ++ show msg
    return ()

--
-- Functions to reconfigure switches
--

-- |Block until new snapshot appears, then reconfigure switch based
-- on updated NIB.
configureSwitch :: Chan NIB.Snapshot -- ^input channel (from the Compiler)
                -> OFS.SwitchHandle
                -> NIB.Switch
                -> IO ()
configureSwitch nibSnapshot switchHandle oldSw@(NIB.Switch oldPorts oldTbl) = do
  let switchID = OFS.handle2SwitchID switchHandle
  snapshot <- readChan nibSnapshot
  case Map.lookup switchID snapshot of
    Nothing -> do
      putStrLn $ "configureSwitch did not find " ++ show switchID ++ 
                 " in the NIB snapshot."
      configureSwitch nibSnapshot switchHandle oldSw
    Just sw@(NIB.Switch newPorts newTbl) -> do
      (TOD now _) <- getClockTime
      let (deleteQueueTimers, msgs') = mkPortMods now oldPorts newPorts 
                                         (OFS.sendToSwitch switchHandle)
      let msgs = msgs' ++ mkFlowMods now newTbl oldTbl
      unless (null msgs) $ do
         putStrLn $ "OpenFlow controller modifying tables on " ++ show switchID
         putStrLn $ "sending " ++ show (length msgs) ++ " messages; oldTbl size = " ++ show (Set.size oldTbl) ++ " newTbl size = " ++ show (Set.size newTbl)
         mapM_ (\x -> putStrLn $ "   " ++ show x) msgs
         putStrLn "-------------------------------------------------"
         return ()
      mapM_ (OFS.sendToSwitch switchHandle) (zip [0 ..] msgs)
      deleteQueueTimers
      configureSwitch nibSnapshot switchHandle sw

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
          True -> Nothing -- rule would've been automatically deleted by switch
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
            putStrLn $ "Deleting queue " ++ show qid ++ " on port " ++ show pid
            sendCmd (0, OF.ExtQueueDelete pid [OF.QueueConfig qid []]) 
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
