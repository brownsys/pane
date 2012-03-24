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
           -> Word16 
           -> IO (Chan PacketIn, Chan (OF.SwitchID, Bool))
controller netSnapshot toNIB port = do
  packets <- newChan
  switches <- newChan
  server <- OFS.startOpenFlowServer Nothing port
  forkIO $ do
    forever $ do
      (switch, switchFeatures) <- OFS.acceptSwitch server
      writeChan toNIB (NIB.NewSwitch switch switchFeatures)
      netSnapshot <- dupChan netSnapshot
      writeChan switches (OFS.handle2SwitchID switch, True)
      forkIO (handleSwitch packets switches switch)
      forkIO (configureSwitch netSnapshot switch NIB.emptySwitch)
    OFS.closeServer server
  return (packets, switches)

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
  OF.PacketIn pkt -> writeChan packets (OFS.handle2SwitchID switch, pkt)
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
      configureSwitch netSnapshot switchHandle sw
      