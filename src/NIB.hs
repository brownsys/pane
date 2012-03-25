module NIB
  ( Switch (..)
  , Endpoint (..)
  , FlowTbl (..)
  , PortCfg (..)
  , Msg (..)
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
import qualified Nettle.Servers.Server as OFS
import ShareSemantics (MatchTable (..))
import Base
import qualified Nettle.OpenFlow as OF
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

type FlowTblEntry = (OF.Match, [OF.Action], Limit)

type FlowTbl = [FlowTblEntry]

type Snapshot = Map OF.SwitchID Switch

data Queue = Queue Word16 Limit deriving (Show, Eq)


data NIB = NIB {
  nibSwitches  :: HashTable OF.SwitchID SwitchData,
  nibEndpoints :: HashTable OF.EthernetAddress EndpointData,
  nibEndpointsByIP :: HashTable OF.IPAddress EndpointData
}

data EndpointData = EndpointData {
  endpointEthAddr     :: OF.EthernetAddress,
  endpointIP          :: OF.IPAddress,
  endpointPort        :: PortData
}

data SwitchData = SwitchData {
  switchSwitchID                 :: OF.SwitchID,
  switchFlowTable                :: IORef FlowTbl,
  switchPorts                    :: HashTable OF.PortID PortData,
  switchFlowTableUpdateListener  :: IORef (FlowTbl -> IO ())
}


data PortData = PortData {
  portPortID              :: OF.PortID,
  portQueues              :: HashTable OF.QueueID Queue,
  portAttachedTo          :: Element,
  portConnectedTo         :: IORef Element,
  portQueueUpdateListener :: IORef ([(OF.QueueID, Queue)] -> IO ())
}

data Element
  = ToNone
  | ToEndpoint EndpointData
  | ToSwitch SwitchData PortData

data Msg
  = NewSwitch OFS.SwitchHandle OF.SwitchFeatures

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
    Nothing -> putStrLn $ "nibMutator: switch already exists " ++ show swID
    Just sw -> do
      putStrLn $ "NIB added switch " ++ show swID
      let addPort' p = do
            maybe <- addPort (OF.portID p) sw
            case maybe of
              Nothing -> putStrLn $ "nibMutator: port already exists"
              Just _ -> do
                putStrLn $ "NIB added port " ++ show (OF.portID p) ++ 
                           " on switch " ++ show swID
                return ()
      mapM_ addPort' (OF.ports features)

addSwitch :: OF.SwitchID -> NIB -> IO (Maybe SwitchData)
addSwitch newSwitchID nib = do
  maybe <- Ht.lookup (nibSwitches nib) newSwitchID
  case maybe of
    Just _  -> return Nothing
    Nothing -> do
      flowTbl <- newIORef []
      ports <- Ht.new (==) ((Ht.hashInt).fromIntegral)
      updListener <- newIORef (\_ -> return ()) 
      let sw = SwitchData newSwitchID flowTbl ports updListener
      Ht.insert (nibSwitches nib) newSwitchID sw
      return (Just sw)

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
      writeIORef (portConnectedTo port1) (portAttachedTo port2)
      writeIORef (portConnectedTo port2) (portAttachedTo port1)
      return True
    otherwise -> return False


-- "Neighborhood"
type NbhWalk = [(OF.PortID,OF.SwitchID, OF.PortID)]

data Nbh 
  = EpNbh NbhWalk OF.EthernetAddress (Maybe Nbh)
  | SwNbh NbhWalk OF.SwitchID [Nbh]

getEndpointNbh :: NbhWalk -> EndpointData -> IO Nbh
getEndpointNbh walk endpoint = do
  putStrLn $ "in getEndpointNbh " ++ (show $ endpointEthAddr endpoint)
  otherEnd <- readIORef (portConnectedTo (endpointPort endpoint))
  case otherEnd of
    ToNone -> do
      putStrLn "orphan endpoint"
      return (EpNbh walk (endpointEthAddr endpoint) Nothing)
    ToEndpoint otherEndpoint -> do
      let nbh = unsafePerformIO $ getEndpointNbh walk otherEndpoint
      return (EpNbh walk (endpointEthAddr endpoint) (Just nbh))
    ToSwitch switch otherPort -> do
      putStrLn "connected EP"
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
  return (sid, Switch (Map.fromList ports) ft)

snapshot :: NIB -> IO Snapshot
snapshot nib = do
  lst <- Ht.toList (nibSwitches nib)
  lst <- mapM snapshotSwitchData lst 
  return (Map.fromList lst)
  

data PortCfg = PortCfg (Map OF.QueueID Queue) deriving (Show, Eq)

data Switch = Switch {
  switchPortMap :: Map OF.PortID PortCfg,
  switchTbl :: FlowTbl 
} deriving (Show, Eq)

data Endpoint = Endpoint OF.IPAddress OF.EthernetAddress deriving (Show, Eq)

data Edge
  = Inner OF.SwitchID OF.PortID OF.SwitchID OF.PortID
  | Leaf OF.IPAddress OF.SwitchID OF.PortID
  deriving (Show, Eq)

type Network = (Map OF.SwitchID Switch, [Endpoint], [Edge])

emptySwitch = Switch Map.empty []

-- |'unusedNum lst' returns the smallest positive number that is not in 'lst'.
-- Assumes that 'lst' is in ascending order.
unusedNum :: (Num a, Ord a) => [a] -> a
unusedNum lst = loop 0 lst
  where loop m [] = m
        loop m (n:ns) | m < n     = m
                      | m == n    = loop (m+1) ns
                      | otherwise = error "unusedNum : lst not ascending"

newQueue :: Map OF.PortID PortCfg -- ^ports
         -> OF.PortID             -- ^port to adjust
         -> Word16                -- ^queue GMB
         -> Limit                 -- ^queue lifetime
         -> (OF.QueueID, Map OF.PortID PortCfg) -- ^new configuration
newQueue ports portID gmb timeOut = (queueID, ports')
  where queueID = unusedNum (Map.keys queues)
        queues  = case Map.lookup portID ports of
                    Just (PortCfg q)  -> q
                    Nothing -> error "newQueue: bad portID"
        queues' = Map.insert queueID (Queue gmb timeOut) queues
        ports'  = Map.adjust (\(PortCfg queues) -> PortCfg queues') portID ports



switchWithNPorts :: Word16 -> Switch
switchWithNPorts n = 
  Switch (Map.fromList [(k, PortCfg Map.empty) | k <- [0 .. n-1]]) []
