module NIB
  ( Switch (..)
  , Endpoint (..)
  , FlowTbl (..)
  , FlowTblEntry
  , PortCfg (..)
  , Msg (..)
  , Queue (..)
  , SwitchType (..)
  , newQueue
  , switchWithNPorts
  , newEmptyNIB
  , addSwitch
  , addPort
  , addEndpoint
  , linkPorts
  , getPath
  , endpointPort
  , getEthFromIP
  , snapshot
  , NIB
  , Snapshot
  , emptySwitch
  ) where

import Debug.Trace
import qualified Nettle.OpenFlow as OF
import qualified Nettle.Ethernet.AddressResolutionProtocol as OFARP
import qualified Nettle.Servers.Server as OFS
import HFT (MatchTable (..))
import Base
import qualified Nettle.OpenFlow as OF
import qualified Nettle.OpenFlow.StrictPut as OFBS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word16)
import Data.Int (Int32)
import Data.IORef
import Data.HashTable (HashTable)
import qualified Data.HashTable as Ht
import Data.Maybe (isJust, fromJust, catMaybes)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.HList as HL
import System.Log.Logger.TH (deriveLoggers)
import qualified System.Log.Logger as Logger

$(deriveLoggers "Logger" [Logger.DEBUG, Logger.INFO, Logger.WARNING,
                          Logger.ERROR])

type FlowTblEntry = (Word16, OF.Match, [OF.Action], Limit)

type FlowTbl = Set FlowTblEntry

type Snapshot = Map OF.SwitchID Switch

data Queue = Queue Word16 Limit deriving (Show, Eq)


data NIB = NIB {
  nibSwitches  :: HashTable OF.SwitchID SwitchData,
  nibEndpoints :: HashTable OF.EthernetAddress EndpointData,
  nibEndpointsByIP :: HashTable OF.IPAddress EndpointData
}

-- TODO(adf): There is no guarantee that nibEndpoints and nibEndpointsByIP be
-- consistent. It might be better to have a single nibEndpoints HashTable, and
-- then a separate HashTable which does IP -> EthernetAddress. In the "Real
-- World" we can expect to see multiple EthernetAddresses attached to the same
-- switch port, and multiple IP addresses assocated with the same EthernetAddress.
-- Multiple ethernet address for a single IP is also possible (load-balancing),
-- but those will ultimately have separate endpoints.

data EndpointData = EndpointData {
  endpointEthAddr     :: OF.EthernetAddress,
  endpointIP          :: OF.IPAddress,
  endpointPort        :: PortData
}

instance Show EndpointData where
  show (EndpointData ea ip port) =
    "\nEthernet Addr: " ++ show(ea) ++
    "\n    IP: " ++ show(ip) ++
    "\n    Port: " ++ show(port)

data SwitchData = SwitchData {
  switchSwitchID                 :: OF.SwitchID,
  switchType                     :: SwitchType,
  switchFlowTable                :: IORef FlowTbl,
  switchPorts                    :: HashTable OF.PortID PortData,
  switchFlowTableUpdateListener  :: IORef (FlowTbl -> IO ())
}

instance Show SwitchData where
  show (SwitchData sid stype ft ports listener) =
    "\nSwitchID: " ++ show(sid) ++
    "\n    Type: " ++ show(stype)

data SwitchType
  = ReferenceSwitch
  | OpenVSwitch
  | ProntoSwitch
  | OtherSwitch String
  | UnknownSwitch
  deriving Eq

instance Show SwitchType where
  show ReferenceSwitch = "Reference Switch"
  show OpenVSwitch = "Open vSwitch"
  show ProntoSwitch = "Pronto Switch"
  show (OtherSwitch t) = show t
  show UnknownSwitch = "(Unknown)"

data PortData = PortData {
  portPortID              :: OF.PortID,
  portQueues              :: HashTable OF.QueueID Queue,
  portDevice              :: Element, -- ^ local device (always a switch)
  portConnectedTo         :: IORef Element, -- ^ other end of the wire
  portQueueUpdateListener :: IORef ([(OF.QueueID, Queue)] -> IO ())
}

data Element
  = ToNone
  | ToEndpoint EndpointData
  | ToSwitch SwitchData PortData

instance Show Element where
  show ToNone = "<nothing>"
  show (ToEndpoint ep) = show (endpointIP ep)
  show (ToSwitch sw pd) = show (switchSwitchID sw) ++
                          ":" ++ show(portPortID pd)

class ShowIO a where
  showIO :: a -> IO String

instance Show a => ShowIO (IORef a) where
  showIO a = readIORef a >>= return . show

instance Show PortData where
  show p = show (portDevice p) ++ ":" ++ show (portPortID p) ++
             " -> " ++ (unsafePerformIO $ showIO (portConnectedTo p))

data Msg
  = NewSwitch OFS.SwitchHandle OF.SwitchFeatures
  | PacketIn OF.SwitchID OF.PacketInfo
  | StatsReply OF.SwitchID OF.StatsReply
  | DisplayNIB (String -> IO ())

newEmptyNIB :: Chan Msg -> IO NIB
newEmptyNIB msg = do
  s <- Ht.new (==) ((Ht.hashInt).fromIntegral)
  e <- Ht.new (==) ((Ht.hashInt).fromIntegral.(OF.unpack64))
  i <- Ht.new (==) ((Ht.hashInt).fromIntegral.(OF.ipAddressToWord32))
  let nib = NIB s e i
  forkIO (forever (readChan msg >>= nibMutator nib))
  return nib

nibMutator :: NIB -> Msg -> IO ()
nibMutator nib (NewSwitch handle features) = do
  let swID = OF.switchID features
  maybe <- addSwitch swID nib
  case maybe of
    Nothing -> warningM $ "nibMutator: switch already exists " ++ show swID
    Just sw -> do
      infoM$ "NIB added switch " ++ show swID ++ "."
      let addPort' p = do
            maybe <- addPort (OF.portID p) sw
            case maybe of
              Nothing -> warningM $ "nibMutator: port already exists"
              Just _ -> do
                debugM $ "NIB added port " ++ show (OF.portID p) ++
                         " on switch " ++ show swID
                sendDP handle (OF.portID p)
                return ()
      ignoreExns ("sending PaneDP on switch " ++ show swID) $
                 mapM_ addPort' (OF.ports features)
nibMutator nib (StatsReply swid reply) = case reply of
  OF.DescriptionReply desc -> case OF.hardwareDesc desc of
      "Reference Userspace Switch" -> setSwitchType swid ReferenceSwitch nib
      "Open vSwitch" -> setSwitchType swid OpenVSwitch nib
      "Pronto 3290" -> setSwitchType swid ProntoSwitch nib
      otherwise -> setSwitchType swid (OtherSwitch (OF.hardwareDesc desc)) nib
  otherwise -> infoM $ "unhandled statistics reply from switch " ++
                 (OF.showSwID swid) ++ "\n" ++ show reply
nibMutator nib (DisplayNIB putter) = do
  sw  <- Ht.toList (nibSwitches nib)
  e   <- Ht.toList (nibEndpoints nib)
  eip <- Ht.toList (nibEndpointsByIP nib)
  let sw'  = map (\(k,v) -> v) sw
      e'   = map (\(k,v) -> v) e
      eip' = map (\(k,v) -> v) eip
      str  = "Displaying the NIB...\n" ++
             "Switches:\n" ++ show sw' ++
             "\n-------------------------------------\n" ++
             "Endpoints:\n" ++ show e' ++
             "\n-------------------------------------\n" ++
             "EndpointsByIP:\n" ++ show eip' ++
             "\n-------------------------------------\n"
  putter $ str
-- TODO: the code below should be broken-up somehow
nibMutator nib (PacketIn tS pkt) = case OF.enclosedFrame pkt of
  Right (HL.HCons _ (HL.HCons (OF.PaneDPInEthernet fS fP) HL.HNil)) -> do
    let tP = OF.receivedOnPort pkt
    yFromSwitch <- Ht.lookup (nibSwitches nib) fS
    yToSwitch <- Ht.lookup (nibSwitches nib) tS
    case (yFromSwitch, yToSwitch) of
      (Just fromSwitch, Just toSwitch) -> do
        yFromPort <- Ht.lookup (switchPorts fromSwitch) fP
        yToPort <- Ht.lookup (switchPorts toSwitch) tP
        case (yFromPort, yToPort) of
          (Just fromPort, Just toPort) -> do
            toDevice <- readIORef (portConnectedTo fromPort)
            case toDevice of
              ToNone -> do
                linkPorts fromPort toPort
                return ()
              ToEndpoint ep -> do
                Ht.delete (nibEndpoints nib) (endpointEthAddr ep)
                Ht.delete (nibEndpointsByIP nib) (endpointIP ep)
                writeIORef (portConnectedTo fromPort) ToNone
                writeIORef (portConnectedTo toPort) ToNone
                linkPorts fromPort toPort
                return ()
              ToSwitch _ _ -> do
                errorM $ "NIB already linked to a switch"
          otherwise -> errorM $ "NIB failed to find port(s)"
      otherwise -> errorM $ "NIB failed to find switch(s)"
  Right (HL.HCons hdr (HL.HCons (OF.ARPInEthernet arp) HL.HNil)) -> do
    let srcEth = OF.sourceMACAddress hdr
    let srcIP = case arp of
          OFARP.ARPQuery qp -> OFARP.querySenderIPAddress qp
          OFARP.ARPReply rp -> OFARP.replySenderIPAddress rp
    let srcPort = OF.receivedOnPort pkt
    ySwitch <- Ht.lookup (nibSwitches nib) tS
    let hostStr = show (srcEth, srcIP)
    case ySwitch of
      Nothing -> do
        errorM $ "NIB cannot find switch for " ++ hostStr
        return ()
      Just switch -> do
        maybe <- Ht.lookup (switchPorts switch) srcPort
        case maybe of
          Nothing -> do
            errorM $ "NIB cannot find port for " ++ hostStr
            return ()
          Just port -> do
            connectedTo <- readIORef (portConnectedTo port)
            case connectedTo of
              ToNone -> do
                maybe <- addEndpoint srcEth srcIP nib
                case maybe of
                  Nothing -> do
                    infoM $ "NIB already knows " ++ hostStr
                    return ()
                  Just endpoint -> do
                    b <- linkPorts port (endpointPort endpoint)
                    infoM $ "NIB discovered host " ++ (show (srcEth, srcIP)) ++ " " ++ show b
                    return ()
              conn -> do
                warningM $ "NIB already connects " ++ hostStr ++ " to " ++
                           show conn
                return ()
  Right (HL.HCons hdr (HL.HCons (OF.IPInEthernet (HL.HCons ipHdr (HL.HCons _ HL.HNil))) HL.HNil)) -> do
    let srcEth = OF.sourceMACAddress hdr
    let srcIP = OF.ipSrcAddress ipHdr
    let srcPort = OF.receivedOnPort pkt
    ySwitch <- Ht.lookup (nibSwitches nib) tS
    let hostStr = show (srcEth, srcIP)
    case ySwitch of
      Nothing -> do
        errorM $ "NIB cannot find switch for " ++ hostStr
        return ()
      Just switch -> do
        maybe <- Ht.lookup (switchPorts switch) srcPort
        case maybe of
          Nothing -> do
            errorM $ "NIB cannot find port for " ++ hostStr
            return ()
          Just port -> do
            connectedTo <- readIORef (portConnectedTo port)
            case connectedTo of
              ToNone -> do
                maybe <- addEndpoint srcEth srcIP nib
                case maybe of
                  Nothing -> do
                    return ()
                  Just endpoint -> do
                    b <- linkPorts port (endpointPort endpoint)
                    infoM $ "NIB discovered host " ++ (show (srcEth, srcIP)) ++ " " ++ show b
                    return ()
              conn -> do
                return ()

  otherwise -> return ()
 
  

sendDP :: OFS.SwitchHandle -> OF.PortID -> IO ()
sendDP handle portID = do
  threadDelay 1000
  let ethAddr = OF.ethernetAddress64 0
  let hdr = OF.EthernetHeader ethAddr ethAddr OF.ethTypePaneDP
  let body = OF.PaneDPInEthernet (OFS.handle2SwitchID handle) portID
  let frm = HL.HCons hdr (HL.HCons body HL.HNil)
  let bs = OFBS.runPutToByteString 200 (OF.putEthFrame frm)
  let out = OF.PacketOutRecord (Right bs) Nothing (OF.sendOnPort portID)
  OFS.sendToSwitch handle (0xbe, OF.PacketOut out)

addSwitch :: OF.SwitchID -> NIB -> IO (Maybe SwitchData)
addSwitch newSwitchID nib = do
  -- TODO(adf): why don't we do anything with maybe?
  maybe <- Ht.lookup (nibSwitches nib) newSwitchID
  flowTbl <- newIORef Set.empty
  ports <- Ht.new (==) ((Ht.hashInt).fromIntegral)
  updListener <- newIORef (\_ -> return ()) 
  let sw = SwitchData newSwitchID UnknownSwitch flowTbl ports updListener
  Ht.insert (nibSwitches nib) newSwitchID sw
  return (Just sw)

setSwitchType :: OF.SwitchID -> SwitchType -> NIB -> IO ()
setSwitchType swid stype nib = do
  maybe <- Ht.lookup (nibSwitches nib) swid
  case maybe of
   Nothing -> do
     errorM $ "switch " ++ OF.showSwID swid ++ " not yet in NIB."
                        ++ " cannot add its type."
     return()
   Just sd ->
     let sd' = sd { switchType = stype }
     in do Ht.update (nibSwitches nib) swid sd'
           debugM $ "set switch " ++ OF.showSwID swid ++ " to have type: "
                    ++ show stype
           return()

addPort :: OF.PortID -> SwitchData -> IO (Maybe PortData)
addPort newPortID switch = do
  maybe <- Ht.lookup (switchPorts switch) newPortID
  case maybe of
    Just _  -> return Nothing
    Nothing -> do
      queues <- Ht.new (==) ((Ht.hashInt).fromIntegral)
      connectedTo <- newIORef ToNone
      updListener <- newIORef (\_ -> return ())
      let port = PortData newPortID queues (ToSwitch switch port) 
                          connectedTo updListener
      Ht.insert (switchPorts switch) newPortID port
      return (Just port)

addEndpoint :: OF.EthernetAddress -> OF.IPAddress -> NIB 
            -> IO (Maybe EndpointData)
addEndpoint newEthAddr ipAddr nib = do
  maybe <- Ht.lookup (nibEndpoints nib) newEthAddr
  case maybe of
    Just _  -> return Nothing
    Nothing -> do
      connectedTo <- newIORef ToNone     
      queues <- Ht.new (==) ((Ht.hashInt).fromIntegral)
      updListener <- newIORef (\_ -> return ())
      let ep = EndpointData newEthAddr ipAddr 
                            (PortData 0 queues (ToEndpoint ep)
                                      connectedTo updListener)
      Ht.insert (nibEndpoints nib) newEthAddr ep
      Ht.insert (nibEndpointsByIP nib) ipAddr ep
      return (Just ep)

getEndpoint :: OF.EthernetAddress -> NIB -> IO (Maybe EndpointData)
getEndpoint ethAddr nib = Ht.lookup (nibEndpoints nib) ethAddr

getPorts :: SwitchData -> IO [PortData]
getPorts switch = do
  links <- Ht.toList (switchPorts switch)
  return (map snd links)


followLink :: PortData -> IO (Maybe PortData)
followLink port = do
  elem <- readIORef (portConnectedTo port)
  case elem of
    ToNone        -> return Nothing
    ToEndpoint ep -> return (Just (endpointPort ep))
    ToSwitch sw p -> return (Just p)


linkPorts :: PortData -> PortData -> IO Bool
linkPorts port1 port2 = do
  conn1 <- readIORef (portConnectedTo port1)
  conn2 <- readIORef (portConnectedTo port2)
  case (conn1, conn2) of
    (ToNone, ToNone) -> do
      writeIORef (portConnectedTo port1) (portDevice port2)
      writeIORef (portConnectedTo port2) (portDevice port1)
      return True
    otherwise -> return False


-- "Neighborhood"
-- (ingress port, switch, egress port)
type NbhWalk = [(OF.PortID,OF.SwitchID, OF.PortID)]

data Nbh 
  = EpNbh NbhWalk OF.EthernetAddress (Maybe Nbh)
  | SwNbh NbhWalk OF.SwitchID [Nbh]

getEndpointNbh :: NbhWalk -> EndpointData -> IO Nbh
getEndpointNbh walk endpoint = do
  otherEnd <- readIORef (portConnectedTo (endpointPort endpoint))
  case otherEnd of
    ToNone -> do
      return (EpNbh walk (endpointEthAddr endpoint) Nothing)
    ToEndpoint otherEndpoint -> do
      let nbh = unsafePerformIO $ getEndpointNbh walk otherEndpoint
      return (EpNbh walk (endpointEthAddr endpoint) (Just nbh))
    ToSwitch switch otherPort -> do
      let nbh = unsafePerformIO $ getSwitchNbh (portPortID otherPort) [] switch
      return (EpNbh walk (endpointEthAddr endpoint) (Just nbh))

getSwitchNbh :: OF.PortID -> NbhWalk -> SwitchData -> IO Nbh
getSwitchNbh inPort walk switch = do
  let continueWalk outPort = do
        otherEnd <- readIORef (portConnectedTo outPort)
        let walk' = (inPort, switchSwitchID switch, portPortID outPort):walk
        case otherEnd of
          ToNone -> return Nothing
          ToEndpoint ep -> do
            let nbh = unsafePerformIO $ getEndpointNbh walk' ep
            return (Just nbh)
          ToSwitch switch' inPort' -> do
            let nbh = unsafePerformIO $
                        getSwitchNbh (portPortID inPort') walk' switch'
            return (Just nbh)
  outPorts <- getPorts switch
  nbhs <- mapM continueWalk outPorts
  return (SwNbh walk (switchSwitchID switch) (catMaybes nbhs))


getEthFromIP :: OF.IPAddress -> NIB -> IO (Maybe OF.EthernetAddress)
getEthFromIP ip nib = do
  maybe <- Ht.lookup (nibEndpointsByIP nib) ip
  case maybe of
    Nothing -> return Nothing
    Just ep -> return (Just (endpointEthAddr ep))
  
getPath :: OF.EthernetAddress -> OF.EthernetAddress -> NIB 
        -> IO NbhWalk
getPath srcEth dstEth nib = do
  let loop fringe visited = case fringe of
        [] -> []
        ((EpNbh walk eth nbh):rest) -> case eth == dstEth of
          True  -> reverse walk
          False -> loop rest visited
        ((SwNbh walk swID neighbors):fringe') -> 
          let isVisited (EpNbh _ _ _) = False
              isVisited (SwNbh _ swID' _) = swID' `Set.member` visited
              visited' = Set.insert swID visited
            in loop (fringe' ++ (filter (not.isVisited) neighbors)) visited'
  maybe <- getEndpoint srcEth nib
  case maybe of
    Nothing -> return []
    Just srcEp -> do
      nbh <- getEndpointNbh [] srcEp
      case nbh of
        EpNbh _ _ (Just nbh) -> return (loop [nbh] Set.empty)
        otherwise -> return []

snapshotPortData :: (OF.PortID, PortData) -> IO (OF.PortID, PortCfg)
snapshotPortData (portID, port) = do
  queues <- Ht.toList (portQueues port)
  return (portID, PortCfg (Map.fromList queues))

snapshotSwitchData :: (OF.SwitchID, SwitchData) -> IO (OF.SwitchID, Switch)
snapshotSwitchData (sid, switch) = do
  ft <- readIORef (switchFlowTable switch)
  ports <- Ht.toList (switchPorts switch)
  ports <- mapM snapshotPortData ports
  return (sid, Switch (Map.fromList ports) ft (switchType switch))

snapshot :: NIB -> IO Snapshot
snapshot nib = do
  lst <- Ht.toList (nibSwitches nib)
  lst <- mapM snapshotSwitchData lst 
  return (Map.fromList lst)
  

data PortCfg = PortCfg (Map OF.QueueID Queue) deriving (Show, Eq)

data Switch = Switch {
  switchPortMap  :: Map OF.PortID PortCfg,
  switchTbl      :: FlowTbl,
  switchTypeSnap :: SwitchType
} deriving (Show, Eq)

data Endpoint = Endpoint OF.IPAddress OF.EthernetAddress deriving (Show, Eq)

data Edge
  = Inner OF.SwitchID OF.PortID OF.SwitchID OF.PortID
  | Leaf OF.IPAddress OF.SwitchID OF.PortID
  deriving (Show, Eq)

type Network = (Map OF.SwitchID Switch, [Endpoint], [Edge])

emptySwitch = Switch Map.empty Set.empty UnknownSwitch

-- |'unusedNumWithFloor flr lst' returns the smallest positive number greater
-- than 'flr' which is not in 'lst'. Assumes that 'lst' is in ascending order.
unusedNumWithFloor ::  (Num a, Ord a) => a -> [a] -> a
unusedNumWithFloor flr lst = loop flr lst
  where loop m [] = m
        loop m (n:ns) | m < n     = m
                      | m == n    = loop (m+1) ns
                      | otherwise = error "unusedNum : lst not ascending"

newQueue :: Map OF.PortID PortCfg -- ^ports
         -> OF.PortID             -- ^port to adjust
         -> Word16                -- ^queue GMB
         -> Limit                 -- ^queue ending time
         -> (OF.QueueID, Map OF.PortID PortCfg) -- ^new configuration
newQueue ports portID gmb end = (queueID, ports')
  -- Queue IDs start with 1 for Open vSwitch and go up, so let's follow that
  where queueID = unusedNumWithFloor 1 (Map.keys queues)
        queues  = case Map.lookup portID ports of
                    Just (PortCfg q)  -> q
                    Nothing -> error "newQueue: bad portID"
        queues' = Map.insert queueID (Queue gmb end) queues
        ports'  = Map.adjust (\(PortCfg queues) -> PortCfg queues') portID ports



switchWithNPorts :: Word16 -> Switch
switchWithNPorts n = 
  Switch (Map.fromList [(k, PortCfg Map.empty) | k <- [0 .. n-1]]) Set.empty UnknownSwitch
