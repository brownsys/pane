module NIB2
 ( Msg (..)
 , NIB (..)
 , Switch (..)
 , Port (..)
 , Queue (..)
 , Endpoint (..)
 , NodeLink  (..)
 , runNIB
 , lookupIP
 ) where

import qualified Nettle.OpenFlow as OF
import qualified Nettle.Servers.Server as OFS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word16)
import System.Log.Logger
import Control.Monad.State
import Data.Either (partitionEithers)

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
    switchPorts     :: Map.Map OF.PortID Port,
    switchBufSize   :: Integer,
    switchNumTables :: Integer
} deriving (Eq,Ord)

instance Show Switch where
  show (Switch sid stype ports bufsize ntables) =
    "SwitchID: " ++ show(sid) ++
    "\n    Type: " ++ show(stype) ++
    "\n    Num ports: " ++ show(Map.size ports) ++
    "\n    Buffer size: " ++ show(bufsize) ++
    "\n    Num tables: " ++ show(ntables)

data Port = Port {
    portID      :: OF.PortID, -- not globally unique; only unique per switch
    portName    :: String,
    portLinks   :: [ NodeLink ],
    portQueues  :: Map.Map OF.QueueID Queue,
    portSpeed   :: PortSpeed,  -- highest speed port supports / is operating on
    portDown    :: Bool, -- whether port is disabled
    portNoFlood :: Bool, -- whether flooding is disabled
    portNoLink  :: Bool  -- whether a link exists or not
    -- portMedium? (copper, fiber)
    -- portPause?
} deriving (Eq,Ord)

instance Show Port where
  show (Port pid name links queues speed down noflood nolink) =
    "PortID: " ++ show(pid) ++
    "\n    Name: " ++ show(name) ++
    "\n    Speed: " ++ show(speed) ++
    "\n    isDown?: " ++ show(down) ++
    "\n    noFlood?: " ++ show(noflood) ++
    "\n    noLink?: " ++ show(nolink) ++
    "\n    Num links: " ++ show(length links) ++
--    "\n    Links:\n" ++ map show links ++
    "\n    Num queues: " ++ show(Map.size queues)
--    "\n    Queues:\n" ++ show queues

data Queue = Queue {
    queueID         :: OF.QueueID, -- not globally unique; only unique per port
    queueMinRate    :: OF.QueueRate,
    queueMaxRate    :: OF.QueueRate,
    queueExpiry     :: Limit
} deriving (Eq,Ord)

instance Show Queue where
  show (Queue qid minr maxr expiry) =
    "QueueID: " ++ show(qid) ++
    "\n    Min-rate: " ++ show(minr) ++
    "\n    Max-rate: " ++ show(maxr) ++
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
    | SpeedUnknown
    deriving (Eq,Ord)

instance Show PortSpeed where
  show Speed10Mb = "10 Mbps"
  show Speed100Mb = "100 Mbps"
  show Speed1Gb = "1 Gbps"
  show Speed10Gb = "10 Gbps"
  show SpeedUnknown = "(unknown speed)"

highestPortSpeed :: [OF.PortFeature] -> PortSpeed
highestPortSpeed pf | OF.Rate10GbFD  `elem` pf = Speed10Gb
                    | OF.Rate1GbFD   `elem` pf = Speed1Gb
                    | OF.Rate1GbHD   `elem` pf = Speed1Gb
                    | OF.Rate100MbFD `elem` pf = Speed100Mb
                    | OF.Rate100MbHD `elem` pf = Speed100Mb
                    | OF.Rate10MbFD  `elem` pf = Speed10Mb
                    | OF.Rate10MbHD  `elem` pf = Speed10Mb
                    | otherwise                = SpeedUnknown


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
  | PortStatus OF.SwitchID OF.PortStatus

data OutgoingMsg
  = DiscoveryMsg OFS.SwitchHandle
 -- maybe also a "DiscoveryPortMsg" for when a port is added to existing switch?
 
data LogMsg = LogMsg Priority String

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
        let (nib', msgs) = execState (processMsg msg) (nib, [])
            (logs, outgoing) = partitionEithers msgs
        mapM_ (\(LogMsg p s) -> logM "NIB.processor" p s) logs
        writeList2Chan outgoingChan outgoing
        nibProcessor nib'

  forkIO $ nibProcessor newNIB
  return ()


processMsg :: Msg -> NIBState ()

processMsg (NewSwitch handle features) =
  let swid = OF.switchID features in do
      newSwitch <- addSwitch swid handle features
      if newSwitch
        then mapM_ (addPort swid) (OF.ports features)
        else return()

processMsg (StatsReply swid reply) = case reply of
  OF.DescriptionReply desc ->
        let hardwareDesc = OF.hardwareDesc desc in
        case Map.lookup hardwareDesc decodeSwitchType of
            Just st -> setSwitchType swid st
            Nothing -> setSwitchType swid (OtherSwitch hardwareDesc)
  otherwise -> logMsg NOTICE $ "unhandled statistics reply from switch " ++
                 (OF.showSwID swid) ++ "\n" ++ show reply

processMsg (PortStatus swid (reason, port)) = case reason of
  OF.PortAdded -> addPort swid port
  OF.PortModified -> modifyPort swid port
  OF.PortDeleted -> deletePort swid port

processMsg (PacketIn swid pkt) = logMsg DEBUG $ "PacketIn unimplemented" -- TODO(adf):

processMsg (DisplayNIB putter) = do
  logMsg NOTICE $ "trying to log (this won't work)..."
  displayNIB putter -- TODO(adf): this is not at all the right approach; should send NIB snapshots out a channel
  return()


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

addSwitch :: OF.SwitchID -> OFS.SwitchHandle -> OF.SwitchFeatures -> NIBState Bool
addSwitch swid handle features = do
  nib <- getNIB
  case Map.lookup swid (nibSwitches nib) of
    Nothing -> let sw = Switch swid UnknownSwitch Map.empty
                               (OF.packetBufferSize features)
                               (OF.numberFlowTables features)
                   sws = Map.insert swid sw (nibSwitches nib) in do
               logMsg NOTICE $ "NIB added switch: " ++ OF.showSwID swid
               addMsg (DiscoveryMsg handle)
               putNIB (nib { nibSwitches = sws })
               return True

    -- TODO(adf): will need to adjust this if we give switches a grace period
    Just sw -> do logMsg WARNING $ "switch " ++ OF.showSwID swid ++
                                " already in NIB"
                  return False

setSwitchType :: OF.SwitchID -> SwitchType -> NIBState ()
setSwitchType swid stype = do
  nib <- getNIB
  case Map.lookup swid (nibSwitches nib) of
    Just sw -> let sw'  = sw { switchType = stype }
                   sws' = Map.insert swid sw' (nibSwitches nib) in
               putNIB (nib { nibSwitches = sws' })
    Nothing -> logMsg ERROR $ "switch " ++ OF.showSwID swid ++ " not in NIB."
                          ++ " cannot add its type."


-- Determines highest speed port supports / is operating on
-- TODO(adf): double-check against Chen's experience!!
determineSpeed :: OF.Port -> PortSpeed
determineSpeed ofport =
  case OF.portCurrentFeatures ofport of
    Just pf -> highestPortSpeed pf
    Nothing -> case OF.portAdvertisedFeatures ofport of
                 Just pf -> highestPortSpeed pf
                 Nothing -> SpeedUnknown

-- Helper function for addPort and modifyPort
-- since both are very similar, except with modifyPort, we want
-- to keep the known links and queues
insertPort :: OF.SwitchID -> Switch -> OF.Port -> [NodeLink] ->
              Map.Map OF.QueueID Queue -> NIBState()
insertPort swid sw ofport nls queues = do
  nib <- getNIB
  let pid     = OF.portID ofport
      name    = OF.portName ofport
      speed   = determineSpeed ofport
      down    = OF.PortDown `elem` (OF.portConfig ofport)
      noflood = OF.NoFlooding `elem` (OF.portConfig ofport)
      nolink  = OF.portLinkDown ofport

      port    = Port pid name nls queues speed down noflood nolink

      ports   = Map.insert pid port (switchPorts sw)
      sw'     = sw { switchPorts = ports }
      sws     = Map.insert swid sw' (nibSwitches nib) in do
      logMsg DEBUG $ "NIB inserted port: " ++ show pid ++
                     " to switch: " ++ OF.showSwID swid
--    addMsg (PortUpdateMsg swid pid) -- TODO(adf): maybe? depends on events NIB should send...
      putNIB (nib { nibSwitches = sws })

addPort :: OF.SwitchID -> OF.Port -> NIBState ()
addPort swid ofport = do
  nib <- getNIB
  case Map.lookup swid (nibSwitches nib) of
    Just sw -> insertPort swid sw ofport [] Map.empty
    Nothing -> logMsg ERROR $ "switch " ++ OF.showSwID swid ++ " not in NIB."
                        ++ " cannot add OF port: " ++ show ofport


modifyPort :: OF.SwitchID -> OF.Port -> NIBState()
modifyPort swid ofport = do
  nib <- getNIB
  case Map.lookup swid (nibSwitches nib) of
    Just sw -> case Map.lookup (OF.portID ofport) (switchPorts sw) of
                 Just port -> insertPort swid sw ofport (portLinks port)
                                         (portQueues port)
                 Nothing   -> logMsg ERROR $ "switch " ++ OF.showSwID swid ++
                                " does not have a port to modify with: " ++
                                show ofport
    Nothing -> logMsg ERROR $ "switch " ++ OF.showSwID swid ++ " not in NIB."
                        ++ " cannot modify OF port: " ++ show ofport

deletePort :: OF.SwitchID -> OF.Port -> NIBState()
deletePort swid ofport = do
  nib <- getNIB
  case Map.lookup swid (nibSwitches nib) of
    Just sw -> let ports = Map.delete (OF.portID ofport) (switchPorts sw)
                   sw' = sw { switchPorts = ports }
                   sws = Map.insert swid sw' (nibSwitches nib) in do
                   logMsg DEBUG $ "NIB deleted port: " ++ show (OF.portID ofport) ++
                                  " from switch: " ++ OF.showSwID swid
--                   addMsg (PortDeletedMsg swid pid) -- TODO(adf): maybe? depends on events NIB should send...
                   putNIB (nib { nibSwitches = sws })
    Nothing -> logMsg ERROR $ "switch " ++ OF.showSwID swid ++ " not in NIB."
                        ++ " cannot delete OF port: " ++ show ofport


-- TODO(adf): this needs to move away from here...
displayNIB :: (String -> IO ()) -> NIBState (IO ())
displayNIB putter = do
  nib <- getNIB
  let sw  = nibSwitches nib
      e   = nibEndpoints nib
      gw  = nibGatewayIP nib
      str  = "Displaying the NIB...\n" ++
             "Switches:\n" ++ show sw ++
             "\n-------------------------------------\n" ++
             "Endpoints:\n" ++ show e ++
             "\n-------------------------------------\n" ++
             "Gateway IP:\n" ++ show gw ++
             "\n-------------------------------------\n" in do
      return (putter str)

--
-- Tracking and updating the state of the NIB
--

type NIBState a = State (NIB, [Either LogMsg OutgoingMsg]) a

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
    put (nib, msgs ++ [Right msg])

logMsg :: Priority -> String -> NIBState ()
logMsg pri str = do
    (nib, msgs) <- get
    put (nib, msgs ++ [Left (LogMsg pri str)])
