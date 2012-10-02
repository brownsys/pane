module NIB2 where

import qualified Nettle.OpenFlow as OF
import qualified Nettle.Servers.Server as OFS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word16)
import System.Log.Logger
import Control.Monad.State

import Base hiding (Port)


data NIB = NIB {
  nibSwitches   :: Map.Map OF.SwitchID Switch,
  nibEndpoints  :: Map.Map OF.EthernetAddress Endpoint
}

data NodeLink
  = SwitchLink OF.SwitchID OF.PortID
  | EndpointLink OF.EthernetAddress
    deriving (Eq,Ord)

data Node 
  = SwitchNode Switch
  | EndpointNode Endpoint

data Switch = Switch {
    switchID        :: OF.SwitchID,
    switchType      :: SwitchType,
    switchPorts     :: [ Port ]
} deriving (Eq,Ord)

data Port = Port {
    portID      :: OF.PortID, -- not globally unique
    portLinks   :: [ NodeLink ],
    portQueues  :: [ Queue ]
    -- portNoFlood ... controller disables with OFPPC_NO_FLOOD
    -- portDown? ... controller can disable with OFPPC_PORT_DOWN
    -- portSpeed
    -- portMedium? (copper, fiber)
    -- portPause?
} deriving (Eq,Ord)

data Queue = Queue {
    queueID         :: OF.QueueID, -- not globally unique
    queueMinRate    :: Word16,
    queueExpiry     :: Limit
} deriving (Eq,Ord)

data Endpoint = Endpoint {
    endpointEthAddr     :: OF.EthernetAddress,
    endpointEtherIPs    :: [ OF.IPAddress ],
    endpointLink        :: NodeLink -- will always be a SwitchLink
} deriving (Eq,Ord)


data SwitchType
  = ReferenceSwitch
  | OpenVSwitch
  | ProntoSwitch
  | OtherSwitch String
  | UnknownSwitch
  deriving (Eq,Ord)

instance Show SwitchType where
  show ReferenceSwitch = "Reference Switch"
  show OpenVSwitch = "Open vSwitch"
  show ProntoSwitch = "Pronto Switch"
  show (OtherSwitch t) = show t
  show UnknownSwitch = "(Unknown)"

decodeSwitchType :: Map String SwitchType
decodeSwitchType = Map.fromList $ [
    ("Reference Userspace Switch", ReferenceSwitch),
    ("Open vSwitch", OpenVSwitch),
    ("Pronto 3290", ProntoSwitch)]

-- TODO(adf): instance Show for types above


data Msg
  = NewSwitch OFS.SwitchHandle OF.SwitchFeatures
  | PacketIn OF.SwitchID OF.PacketInfo
  | StatsReply OF.SwitchID OF.StatsReply
  | DisplayNIB (String -> IO ())
-- | PortStatus TODO(adf): enable, disable, modify .. which includes OFPPS_LINK_DOWN

data OutgoingMsg
  = DiscoveryMsg OFS.SwitchHandle
  | LogMsg Priority String

runNIB :: Chan Msg -> Chan OutgoingMsg -> IO ()
runNIB chan outgoingChan = do
  let newNIB = NIB Map.empty Map.empty

      nibProcessor nib = do
        msg <- readChan chan
        let (nib, outgoing) = execState (processMsg msg) (nib, [])
        writeList2Chan outgoingChan outgoing
        nibProcessor nib

  forkIO $ nibProcessor newNIB
  return ()


processMsg :: Msg -> NIBState ()
processMsg (StatsReply swid reply) = case reply of
  OF.DescriptionReply desc ->
        let hardwareDesc = OF.hardwareDesc desc in
        case Map.lookup hardwareDesc decodeSwitchType of
            Just st -> setSwitchType swid st
            Nothing -> setSwitchType swid (OtherSwitch hardwareDesc)
  otherwise -> logMsg NOTICE $ "unhandled statistics reply from switch " ++
                 (OF.showSwID swid) ++ "\n" ++ show reply


setSwitchType :: OF.SwitchID -> SwitchType -> NIBState ()
setSwitchType swid stype = do
  nib <- getNIB
  case Map.lookup swid (nibSwitches nib) of
    Just sw -> let sw'  = sw { switchType = stype }
                   sws' = Map.insert swid sw' (nibSwitches nib) in
               putNIB (nib { nibSwitches = sws' })
    Nothing -> logMsg ERROR $ "switch " ++ OF.showSwID swid ++ " not yet in NIB."
                          ++ " cannot add its type."


type NIBState a = State (NIB, [OutgoingMsg]) a

getNIB :: NIBState NIB
getNIB = get >>= return . fst

putNIB :: NIB -> NIBState ()
putNIB nib = do
    (_, msgs) <- get
    put (nib, msgs)

modifyNIB :: (NIB -> NIB) -> NIBState ()
modifyNIB f = getNIB >>= return . f >>= putNIB

addMsg :: OutgoingMsg -> NIBState ()
addMsg msg = do
    (nib, msgs) <- get
    put (nib, msgs ++ [msg])

logMsg :: Priority -> String -> NIBState ()
logMsg pri str = addMsg (LogMsg pri str)
