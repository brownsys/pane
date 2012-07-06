module NettleShim where

import Nettle.OpenFlow
import Nettle.Servers.Server
import Data.Maybe (catMaybes)
import Control.Concurrent
import SimplePatPkt
  ( Coq_pat (..)
  , Coq_pat' (..)
  )
import NetCore
  ( compile
  , Coq_pol (..)
  , Coq_pred
  , A
  )

fromNat Nothing = Nothing
fromNat (Just n) = Just (fromInteger n)

natToEth :: Maybe Integer -> Maybe EthernetAddress
natToEth Nothing = Nothing
natToEth (Just n) = Just (ethernetAddress64 (fromInteger n))

transPat PatEmpty = Nothing
transPat (PatMatch (Coq_mkPat' port srcHost dstHost srcPort dstPort)) =
  Just $ matchAny {
            inPort = fromNat port,
            srcEthAddress = natToEth srcHost,
            dstEthAddress = natToEth dstHost,
            srcTransportPort = fromNat srcPort,
            dstTransportPort = fromNat dstPort
          }

transAct n = SendOutPort (PhysicalPort (fromInteger n))

coqToNettle :: [(Coq_pat, A)] -> [(Match, ActionSequence)]
coqToNettle lst = catMaybes (map f lst)
  where f (pat, act) = case transPat pat of
          Nothing -> Nothing
          Just nettlePat -> Just (nettlePat, map transAct act)

sendToSwitch' sw msg = do
  sendToSwitch sw msg

mkFlowMod :: (Match, ActionSequence) 
          -> Priority
          -> CSMessage
mkFlowMod (pat, acts) pri = FlowMod AddFlow {
  match=pat, 
  priority=pri, 
  actions=acts, 
  cookie=0, 
  notifyWhenRemoved=False, 
  idleTimeOut=Permanent,
  hardTimeOut=Permanent,
  applyToPacket=Nothing,
  overlapAllowed=True 
}

handleOFMsg :: SwitchHandle 
            -> Coq_pol
            -> (TransactionID, SCMessage)  
            -> IO ()
handleOFMsg switch policy (xid, msg) = case msg of
  otherwise -> putStrLn $ "Unhandled message " ++ show msg
      
-- |Installs the static portion of the policy
handleSwitch :: SwitchHandle -> Coq_pol -> IO ()
handleSwitch switch policy = do
  -- Nettle handles keep alive
  let nettleClassifier = coqToNettle (compile policy)
  -- Priority 65535 is for microflow rules from reactive-specialization
  let flowMods = zipWith mkFlowMod nettleClassifier [65534, 65533 ..]
  mapM_ (sendToSwitch' switch) (zip [0..] flowMods)
  untilNothing (receiveFromSwitch switch) (handleOFMsg switch policy)

nettleServer :: Coq_pol -> IO ()
nettleServer policy = do 
  server <- startOpenFlowServer Nothing -- bind to this address
                                6633    -- port to listen on
  forever $ do
    (switch, switchFeatures) <- acceptSwitch server
    forkIO (handleSwitch switch policy)
  closeServer server
