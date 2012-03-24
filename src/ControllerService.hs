module ControllerService
  ( controller
  , PacketIn
  ) where

import Prelude hiding (catch)
import Base

import qualified NIB
import qualified Nettle.OpenFlow as OF
import qualified Nettle.Servers.Server as OFS
import qualified Data.Map as Map

type PacketIn = (OF.SwitchID, OF.PacketInfo)

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
    forkIO (handleSwitch packets switches switch)
    forkIO (configureSwitch netSnapshot switch NIB.emptySwitch)
  OFS.closeServer server

handleSwitch :: Chan PacketIn
             -> Chan (OF.SwitchID, Bool)
             -> OFS.SwitchHandle
             -> IO ()
handleSwitch packets switches switch = do
  let swID = OFS.handle2SwitchID switch
  putStrLn $ "Switch " ++ show swID ++ " entered."
  OFS.untilNothing (retryOnExns (OFS.receiveFromSwitch switch))
                   (\msg -> ignoreExns (messageHandler packets switch msg))
  OFS.closeSwitchHandle switch
  writeChan switches (swID, False)
  putStrLn $ "Connection to switch " ++ show swID ++ " closed."

messageHandler :: Chan PacketIn
               -> OFS.SwitchHandle
               -> (OF.TransactionID, OF.SCMessage)
               -> IO ()
messageHandler packets switch (xid, msg) = case msg of
  OF.PacketIn pkt -> do
    putStrLn $ "OpenFlow controller received packet from switch " ++
               (show (OFS.handle2SwitchID switch))
    writeChan packets (OFS.handle2SwitchID switch, pkt)
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
      let msgs = mkFlowMods tbl oldTbl
      mapM_ (OFS.sendToSwitch switchHandle) (zip [84 ..] msgs)
      configureSwitch netSnapshot switchHandle sw

mkFlowMods :: NIB.FlowTbl
           -> NIB.FlowTbl
           -> [OF.CSMessage]
mkFlowMods newTbl oldTbl = map OF.FlowMod (delMsg:msgs)
  where newTblPrio = zip newTbl [65536, 65534 .. ]
        delMsg = OF.DeleteFlows OF.matchAny Nothing
        msgs =  map mkAddFlow newTblPrio
        mkAddFlow ((match, acts, timeOut), prio) = 
          OF.AddFlow {
            OF.match = match,
            OF.priority = prio,
            OF.actions = acts,
            OF.cookie = 0,
            OF.idleTimeOut = OF.Permanent,
            OF.hardTimeOut = timeOut,
            OF.notifyWhenRemoved = False,
            OF.applyToPacket = Nothing,
            OF.overlapAllowed = True
         }

      
