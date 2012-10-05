module NIB2 where

import qualified Nettle.OpenFlow as OF
import qualified Nettle.Servers.Server as OFS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word16)
import System.Log.Logger
import Control.Monad.State

import Base hiding (Port)

------------------------------------------------------------------------------
--
-- | The "Network Information Base" (NIB) is a model of the current network.
-- Nodes in the network can be either OpenFlow switches, or endpoints. Switches
-- identify themselves to the controller, while endpoints are discovered by
-- processing packets from them.
--
-- The topology between switches are found by a separate discovery module.
--
--------------------------------------------------------------------------------

data NIB = NIB {
  nibSwitches   :: Map.Map OF.SwitchID Switch,
  nibEndpoints  :: Map.Map OF.EthernetAddress Endpoint,

  -- | IP address of the current gateway router, if one exists
  nibGatewayIP  :: Maybe OF.IPAddress
}

data Node 
  = SwitchNode Switch
  | EndpointNode Endpoint

------------------------------------------------------------------------------
--
-- | Switches contain ports, which can be linked to zero or more
-- nodes and have zero or more queues.
--
------------------------------------------------------------------------------

data Switch = Switch {
    switchID        :: OF.SwitchID, -- ^ the datapath ID of the switch
    switchType      :: SwitchType,
    switchPorts     :: [ Port ]
} deriving (Eq,Ord)

instance Show Switch where
  show (Switch sid stype ports) =
    "SwitchID: " ++ show(sid) ++
    "\n    Type: " ++ show(stype) ++
    "\n    Num ports: " ++ show(length ports)

data Port = Port {
    portID      :: OF.PortID, -- not globally unique; only unique per switch
    portLinks   :: [ NodeLink ],
    portQueues  :: [ Queue ],
    portSpeed   :: PortSpeed -- highest speed port supports TODO(adf): should become property of a link instead?
    -- portNoFlood ... controller disables with OFPPC_NO_FLOOD
    -- portDown? ... controller can disable with OFPPC_PORT_DOWN
    -- portSpeed
    -- portMedium? (copper, fiber)
    -- portPause?
} deriving (Eq,Ord)

instance Show Port where
  show (Port pid links queues speed) =
    "PortID: " ++ show(pid) ++
    "\n    Speed: " ++ show(speed) ++
    "\n    Num links: " ++ show(length links) ++
--    "\n    Links:\n" ++ map show links ++
    "\n    Num queues: " ++ show(length queues)
--    "\n    Queues:\n" ++ map show queues

data Queue = Queue {
    queueID         :: OF.QueueID, -- not globally unique; only unique per port
    queueMinRate    :: Word16,
    queueExpiry     :: Limit
} deriving (Eq,Ord)

instance Show Queue where
  show (Queue qid rate expiry) =
    "QueueID: " ++ show(qid) ++
    "\n    Min-rate: " ++ show(rate) ++
    "\n    Expiry: " ++ show(expiry)

--
-- Switch attributes
--

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

--
-- Port attributes
--

data PortSpeed
    = Speed10Mb  -- ^10 Mb rate support
    | Speed100Mb -- ^100 Mb rate support
    | Speed1Gb   -- ^1 Gb rate support
    | Speed10Gb  -- ^10 Gb rate support
    deriving (Eq,Ord)

instance Show PortSpeed where
  show Speed10Mb = "10 Mbps"
  show Speed100Mb = "100 Mbps"
  show Speed1Gb = "1 Gbps"
  show Speed10Gb = "10 Gbps"

------------------------------------------------------------------------------
--
-- | Endpoints are network elements not managed by the OpenFlow controller.
-- From our perspective, endpoints do not forward traffic, and are simply
-- attached to some OpenFlow switch.
--
------------------------------------------------------------------------------

data Endpoint = Endpoint {
    endpointEthAddr     :: OF.EthernetAddress,
    endpointEtherIPs    :: [ OF.IPAddress ],
    endpointLink        :: NodeLink -- will always be a SwitchLink
} deriving (Eq,Ord)

instance Show Endpoint where
  show (Endpoint ea ips link) =
    "Endpoint Eth Addr: " ++ show(ea) ++
    "\n    IP Addresses: " ++ show(ips) ++
    "\n    Linked to: " ++ show(link)

------------------------------------------------------------------------------
--
-- | Links in the network always terminate at a switch port or an endpoint.
--
------------------------------------------------------------------------------

data NodeLink
  = SwitchLink OF.SwitchID OF.PortID
  | EndpointLink OF.EthernetAddress
    deriving (Eq,Ord)

instance Show NodeLink where
  show (SwitchLink s p) = "SwitchLink(" ++ show s ++ "," ++ show p ++ ")"
  show (EndpointLink e) = "EndpointLink(" ++ show e ++ ")"

------------------------------------------------------------------------------
--
-- | NIB management is event-based. Incoming messages are events which update
-- the NIB state. Outgoing messages are produced for modules which wish
-- to react to changes in the NIB (such as the addition of a new switch, the
-- loss a link, etc.)
--
------------------------------------------------------------------------------

data Msg
  = NewSwitch OFS.SwitchHandle OF.SwitchFeatures
  | PacketIn OF.SwitchID OF.PacketInfo
  | StatsReply OF.SwitchID OF.StatsReply
  | DisplayNIB (String -> IO ())
-- | PortStatus TODO(adf): enable, disable, modify .. which includes OFPPS_LINK_DOWN .. also, adding new ports

data OutgoingMsg
  = DiscoveryMsg OFS.SwitchHandle
  | LogMsg Priority String

------------------------------------------------------------------------------
--
-- The heart of NIB management. These functions process the incoming update
-- messages, and distribute snapshots and outgoing events to the NIB's clients.
--
------------------------------------------------------------------------------

runNIB :: Chan Msg -> Chan OutgoingMsg -> IO ()
runNIB chan outgoingChan = do
  let newNIB = NIB Map.empty Map.empty Nothing

      nibProcessor nib = do
        msg <- readChan chan
        let (nib, outgoing) = execState (processMsg msg) (nib, [])
        -- TODO(adf): handle logging messages ourselves, send the rest out
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


------------------------------------------------------------------------------
--
-- External helper functions.
--
------------------------------------------------------------------------------

-- | Returns the list of Endpoint's with the given IPAddress
--
-- one day, there will be a caching layer for this
--
lookupIP :: NIB -> OF.IPAddress -> [ Endpoint ]
lookupIP nib ip =
    let hasIP = elem ip . endpointEtherIPs in
    filter hasIP (Map.elems (nibEndpoints nib))

------------------------------------------------------------------------------
--
-- Internal helper functions.
--
------------------------------------------------------------------------------

--
-- Updating the NIB
--

setSwitchType :: OF.SwitchID -> SwitchType -> NIBState ()
setSwitchType swid stype = do
  nib <- getNIB
  case Map.lookup swid (nibSwitches nib) of
    Just sw -> let sw'  = sw { switchType = stype }
                   sws' = Map.insert swid sw' (nibSwitches nib) in
               putNIB (nib { nibSwitches = sws' })
    Nothing -> logMsg ERROR $ "switch " ++ OF.showSwID swid ++ " not in NIB."
                          ++ " cannot add its type."

--
-- Tracking and updating the state of the NIB
--

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
