module ControllerService
  ( controller
  , PacketIn
  ) where

import Prelude hiding (catch)
import Base
import Data.Map (Map)
import MacLearning (PacketOutChan)
import qualified NIB
import qualified NIB2
import qualified Nettle.OpenFlow as OF
import Nettle.OpenFlow.Switch (showSwID)
import qualified Nettle.Servers.Server as OFS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import System.Process
import System.Exit
import Network.Socket as Skt
import qualified System.Log.Logger as Logger
import System.Log.Logger.TH (deriveLoggers)

$(deriveLoggers "Logger" [Logger.DEBUG, Logger.NOTICE, Logger.WARNING,
                          Logger.ERROR])

type PacketIn = (OF.TransactionID, Integer, OF.SwitchID, OF.PacketInfo)

controller :: Chan NIB.Snapshot  -- ^input channel (from Compiler)
           -> Chan NIB.Msg       -- ^output channel (headed to NIB module)
           -> Chan NIB2.Msg       -- ^output channel (headed to NIB2 module)
           -> Chan PacketIn      -- ^output channel (headed to MAC Learning)
           -> Chan (OF.SwitchID, Bool) -- ^output channel (for MAC Learning;
                                       -- switches connecting & disconnecting)
           -> PacketOutChan      -- ^input channel (from MAC Learning)
           -> PaneConfig
           -> IO ()
controller nibSnapshot toNIB toNIB2 packets switches pktOut config = do
  server <- OFS.startOpenFlowServer Nothing (controllerPort config)
  -- actually send packets sent by MAC learning module
  forkIO $ forever $ do
    (swID, xid, pktOut) <- readChan pktOut
    debugM $ "SEND packet-out" ++ show (OF.bufferIDData pktOut)
    killOnExns "send pkt from controller"
               (OFS.sendToSwitchWithID server swID (xid, OF.PacketOut pktOut))
  -- no-op reader of the original copy of the nibSnapshot channel
  forkIO $ forever $ do
    readChan nibSnapshot
  -- process new switches
  forever $ do
    (switch, switchFeatures) <- retryOnExns "accept switch"
                                            (OFS.acceptSwitch server)
    noticeM $ "OpenFlow controller connected to new switch."
    writeChan toNIB (NIB.NewSwitch switch switchFeatures)
    writeChan toNIB2 (NIB2.NewSwitch switch switchFeatures)
    writeChan switches (OFS.handle2SwitchID switch, True)

    -- Disable Nagle's algorithm on this socket since we are sometimes seeing
    -- junk at the end of Controller -> Switch messages. Since it's not clear
    -- where this is coming from, let's eliminate the kernel's buffers as a
    -- source of confusion. It's most likely that Nettle is not well-behaved
    -- when we have multiple hardware threads; still, setting NO_DELAY will
    -- hopefully cut-down on the problems we were seeing. If we still see them,
    -- we should change sendToSwitch & Strict.runPtr in Nettle to check that
    -- they send the same number of bytes as in the OpenFlow header's len field
    Skt.setSocketOption (OFS.switchSocket switch) Skt.NoDelay 1 -- Disable Nagle

    nibSnapshot <- dupChan nibSnapshot
    configThreadId <- forkIO (configureSwitch nibSnapshot switch NIB.emptySwitch config)
    forkIO (handleSwitch packets toNIB toNIB2 switches switch configThreadId)
    ignoreExns "stats request" $
        OFS.sendToSwitch switch (0, OF.StatsRequest OF.DescriptionRequest)
  OFS.closeServer server

--
-- Functions to handle messages from switches
-- 

handleSwitch :: Chan PacketIn  -- ^output channel (headed to MAC Learning)
             -> Chan NIB.Msg   -- ^output channel (headed to NIB module)
             -> Chan NIB2.Msg   -- ^output channel (headed to NIB module)
             -> Chan (OF.SwitchID, Bool) -- ^output channel (for MAC Learning;
                                         -- switches connecting & disconnecting)
             -> OFS.SwitchHandle
             -> ThreadId -- ^ ThreadId of the configuration thread
             -> IO ()
handleSwitch packets toNIB toNIB2 switches switch configThreadId = do
  let swID = OFS.handle2SwitchID switch
  killOnExns ("clear flowtable on switch with ID: " ++ showSwID swID)
             (OFS.sendToSwitch switch
                  (0, OF.FlowMod $ OF.DeleteFlows OF.matchAny Nothing))
  OFS.untilNothing 
    (retryOnExns ("receive from switch with ID: " ++ showSwID swID)
                 (OFS.receiveFromSwitch switch))
    (\msg -> ignoreExns "msgHandler" (messageHandler packets toNIB toNIB2 switch msg))
  ignoreExns ("close handle for switch with ID: " ++ showSwID swID)
             (OFS.closeSwitchHandle switch)
  writeChan switches (swID, False)
  -- TODO(adf): also inform NIB that switch is gone? could be transient...
  noticeM $ "Connection to switch " ++ showSwID swID ++ " closed. Killing config thread."
  killThread configThreadId

messageHandler :: Chan PacketIn -- ^output channel (headed to MAC Learning)
               -> Chan NIB.Msg  -- ^output channel (headed to NIB module)
               -> Chan NIB2.Msg  -- ^output channel (headed to NIB module)
               -> OFS.SwitchHandle
               -> (OF.TransactionID, OF.SCMessage) -- ^coming from Nettle
               -> IO ()
messageHandler packets toNIB toNIB2 switch (xid, msg) = case msg of
  OF.PacketIn pkt -> do
    now <- readIORef sysTime
    writeChan packets (xid, now, OFS.handle2SwitchID switch, pkt)
    writeChan toNIB (NIB.PacketIn (OFS.handle2SwitchID switch) pkt)
    writeChan toNIB2 (NIB2.PacketIn (OFS.handle2SwitchID switch) pkt)
  OF.StatsReply pkt -> do
    writeChan toNIB (NIB.StatsReply (OFS.handle2SwitchID switch) pkt)
    writeChan toNIB2 (NIB2.StatsReply (OFS.handle2SwitchID switch) pkt)
  OF.PortStatus pkt -> do
    writeChan toNIB2 (NIB2.PortStatus (OFS.handle2SwitchID switch) pkt)
  otherwise -> do
    warningM $ "unhandled message from switch " ++
               (showSwID $ OFS.handle2SwitchID switch) ++ "\n" ++ show msg
    return ()

--
-- Functions to reconfigure switches
--

-- |Block until new snapshot appears, then reconfigure switch based
-- on updated NIB.
configureSwitch :: Chan NIB.Snapshot -- ^input channel (from the Compiler)
                -> OFS.SwitchHandle
                -> NIB.Switch
                -> PaneConfig
                -> IO ()
configureSwitch nibSnapshot switchHandle oldSw@(NIB.Switch oldPorts oldTbl _)
                config = do
  let switchID = OFS.handle2SwitchID switchHandle
  snapshot <- readChan nibSnapshot
  case Map.lookup switchID snapshot of
    Nothing -> do
      errorM $ "configureSwitch did not find " ++ showSwID switchID ++
               " in the NIB snapshot."
      configureSwitch nibSnapshot switchHandle oldSw config
    Just sw@(NIB.Switch newPorts newTbl swType) -> do
      now <- readIORef sysTime
      let (portActions, deleteQueueTimers, msgs') =
           case swType of
             NIB.ReferenceSwitch -> mkPortModsExt now oldPorts newPorts
                                      (OFS.sendToSwitch switchHandle)
             NIB.OpenVSwitch     -> mkPortModsOVS now oldPorts newPorts
                                      switchID config
             NIB.ProntoSwitch    -> mkPortModsExt now oldPorts newPorts
                                      (OFS.sendToSwitch switchHandle)
             otherwise           -> (errorM $ "Don't know how to create "
                                              ++ "queues for " ++ show swType,
                                     return(), [])
      let msgs = msgs' ++ mkFlowMods now newTbl oldTbl
      unless (null msgs) $ do
         debugM $ "Controller modifying tables on " ++ showSwID switchID
         debugM $ "sending " ++ show (length msgs) ++ " messages; "
                  ++ "oldTbl size = " ++ show (Set.size oldTbl) ++
                  " newTbl size = " ++ show (Set.size newTbl)
         mapM_ (\x -> debugM $ "   " ++ show x) msgs
         debugM "-------------------------------------------------"
         return ()
      -- TODO(adf): should do something smarter here than silently ignoring
      -- exceptions while writing config to switch...
      portActions
      killOnExns ("configuring switch with ID: " ++ showSwID switchID)
                 (mapM_ (OFS.sendToSwitch switchHandle) (zip [0 ..] msgs))
      deleteQueueTimers
      configureSwitch nibSnapshot switchHandle sw config

mkFlowMods :: Integer
           -> NIB.FlowTbl
           -> NIB.FlowTbl
           -> [OF.CSMessage]
mkFlowMods now newTbl oldTbl = map OF.FlowMod (delMsgs ++ addMsgs)
  where delMsgs = mapMaybe mkDelFlow (Set.toList oldRules)
        addMsgs = mapMaybe mkAddFlow (Set.toList newRules)
        mkAddFlow (prio, match, acts, expiry) = case expiry <= fromInteger now of
          True -> Nothing -- rule is expiring
          False ->
            Just (OF.AddFlow {
              OF.match = match,
              OF.priority = prio,
              OF.actions = acts,
              OF.cookie = 0,
              OF.idleTimeOut = OF.Permanent,
              OF.hardTimeOut = toTimeout now expiry ,
              OF.notifyWhenRemoved = False,
              OF.applyToPacket = Nothing,
              OF.overlapAllowed = True
            })
        mkDelFlow (prio, match, _, expiry) = case expiry <= fromInteger now of
          True -> Nothing -- rule would've been automatically deleted by switch
          False -> Just (OF.DeleteExactFlow match Nothing prio)
        newRules = Set.difference newTbl oldTbl
        oldRules = Set.difference oldTbl newTbl

-- |We cannot have queues automatically expire with the slicing extension.
-- So, we return an action that sets up timers to delete queues.
mkPortModsExt :: Integer
              -> Map OF.PortID NIB.PortCfg
              -> Map OF.PortID NIB.PortCfg
              -> ((OF.TransactionID, OF.CSMessage) -> IO ())
              -> (IO (), IO (), [OF.CSMessage])
mkPortModsExt now portsNow portsNext sendCmd = (addActions, delTimers, addMsgs)
  where addActions = return ()
        addMsgs = map newQueueMsg newQueues
        delTimers = sequence_ (map delQueueAction newQueues)

        newQueueMsg ((pid, qid), NIB.Queue (OF.Enabled resv) OF.Disabled _) =
          OF.ExtQueueModify pid
            [OF.QueueConfig qid [OF.MinRateQueue (OF.Enabled (
                                                  translateRate resv))]]
        newQueueMsg ((pid, qid), NIB.Queue OF.Disabled (OF.Enabled rlimit) _) =
          OF.ExtQueueModify pid
            [OF.QueueConfig qid [OF.MaxRateQueue (OF.Enabled (
                                                  translateRate rlimit))]]

        delQueueAction ((_, _), NIB.Queue _ _ NoLimit) = return ()
        delQueueAction ((pid, qid), NIB.Queue _ _ (DiscreteLimit end)) = do
          forkIO $ do
            threadDelay (10^6 * (fromIntegral $ end - now))
            debugM $ "Deleting queue " ++ show qid ++ " on port " ++ show pid
            ignoreExns ("deleting queue " ++ show qid)
                    (sendCmd (0, OF.ExtQueueDelete pid [OF.QueueConfig qid []]))
          return ()

        qCmpLeft ql qr = if ql == qr then Nothing else (Just ql)
        newQueues = Map.toList $
          Map.differenceWith qCmpLeft (flatten portsNext) (flatten portsNow)
        flatten portMap = Map.fromList $
          concatMap (\(pid, NIB.PortCfg qMap) ->
                      map (\(qid, q) -> ((pid, qid), q)) (Map.toList qMap))
                    (Map.toList portMap)

-- |We cannot have queues automatically expire with Open vSwitch, either.
-- So, we return an action that sets up timers to delete queues.
mkPortModsOVS :: Integer
              -> Map OF.PortID NIB.PortCfg
              -> Map OF.PortID NIB.PortCfg
              -> OF.SwitchID
              -> PaneConfig
              -> (IO (), IO (), [OF.CSMessage])
mkPortModsOVS now portsNow portsNext swid config =
  (addActions, delTimers, addMsgs)
  where addMsgs = [] -- No OpenFlow messages needed
        addActions = sequence_ (map newQueueAction newQueues)
        delTimers = sequence_ (map delQueueAction newQueues)

        newQueueAction ((pid, qid), NIB.Queue (OF.Enabled resv) OF.Disabled _) =
          runOVSscript "create" (ovsSetQueue config) swid pid qid resv 0

        newQueueAction ((pid, qid), NIB.Queue OF.Disabled (OF.Enabled rlimit) _) =
          runOVSscript "create" (ovsSetQueue config) swid pid qid 0 rlimit

        delQueueAction ((_, _), NIB.Queue _ _ NoLimit) = return ()
        delQueueAction ((pid, qid), NIB.Queue _ _ (DiscreteLimit end)) = do
          forkIO $ do
            threadDelay (10^6 * (fromIntegral $ end - now))
            runOVSscript "delete" (ovsDeleteQueue config) swid pid qid 0 0
            return()
          return ()

        qCmpLeft ql qr = if ql == qr then Nothing else (Just ql)
        newQueues = Map.toList $
          Map.differenceWith qCmpLeft (flatten portsNext) (flatten portsNow)
        flatten portMap = Map.fromList $
          concatMap (\(pid, NIB.PortCfg qMap) ->
                      map (\(qid, q) -> ((pid, qid), q)) (Map.toList qMap))
                    (Map.toList portMap)



-- |Helper to handle fork'ing out to run the scripts which know how
-- to configure Open vSwitch-based switches.
runOVSscript desc script swid pid qid resv rlimit = do
  debugM $ desc ++ " queue " ++ show qid ++ " on port " ++ show pid
           ++ " switch " ++ show swid
  exitcode <- rawSystem script [show swid, show pid, show qid,
                                show resv, show rlimit]
  case exitcode of
    ExitSuccess   -> return ()
    ExitFailure n -> noticeM $ "Exception (ignoring): failed to " ++ desc
                             ++ " OVS queue: " ++ show swid ++ " " ++
                             show pid ++ " " ++ show qid ++
                             "; ExitFailure: " ++ show n

            
-- TODO(arjun): toTimeout will fail if (end - now) does not fit in a Word16
toTimeout :: Integer -> Limit -> OF.TimeOut
toTimeout _   NoLimit = 
  OF.Permanent
toTimeout now (DiscreteLimit end) = 
  OF.ExpireAfter (fromInteger (end - fromInteger now))

-- assuming the total bandwidth is 1000Mbps, r is the rate in Mbps
-- returns the rate in tenths of a percent, which the Reference switch
-- uses as the guaranteed minimum bandwidth for a queue.
--
-- TODO(adf): should pull link speed from NIB, rather than assume 1000Mbps
translateRate :: Word16 -> Word16
translateRate r =
  let linkSpeed = 1000
  in  truncate $ ((toRational r) / linkSpeed) * 1000
