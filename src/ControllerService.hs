module ControllerService
  ( controller
  , PacketIn
  ) where

import Prelude hiding (catch)
import Base
import System.Time
import qualified NIB
import qualified Nettle.OpenFlow as OF
import qualified Nettle.Servers.Server as OFS
import qualified Data.Map as Map

type PacketIn = (Integer, OF.SwitchID, OF.PacketInfo)

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
           -> Word16 
           -> IO ()
controller netSnapshot toNIB packets switches port = do
  server <- OFS.startOpenFlowServer Nothing port
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
  putStrLn $ "Switch " ++ show swID ++ " entered."
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
    writeChan packets (now, OFS.handle2SwitchID switch, pkt)
    writeChan toNIB (NIB.PacketIn (OFS.handle2SwitchID switch) pkt)
  otherwise -> putStrLn $ "unhandled message" ++ show msg

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
      let msgs = mkFlowMods now tbl oldTbl
      unless (null msgs) $ do
        putStrLn $ "OpenFlow controller modifying tables on " ++ show switchID
      mapM_ (OFS.sendToSwitch switchHandle) (zip [84 ..] msgs)
      configureSwitch netSnapshot switchHandle sw

mkFlowMods :: Integer
           -> NIB.FlowTbl
           -> NIB.FlowTbl
           -> [OF.CSMessage]
mkFlowMods now newTbl oldTbl = map OF.FlowMod msgs
  where newTblPrio = zip newTbl [65536, 65534 .. ]
        delMsg = OF.DeleteFlows OF.matchAny Nothing
        addMsgs =  map mkAddFlow newTblPrio
        mkAddFlow ((match, acts, expiry), prio) = 
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
        msgs = case newTbl == oldTbl of
          True -> []
          False -> delMsg:addMsgs

      
-- TODO(arjun): toTimeout will fail if (end - now) does not fit in a Word16
toTimeout :: Integer -> Limit -> OF.TimeOut
toTimeout _   NoLimit = 
  OF.Permanent
toTimeout now (DiscreteLimit end) = 
  OF.ExpireAfter (fromInteger (end - fromInteger now))
