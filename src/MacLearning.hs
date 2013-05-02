module MacLearning 
  ( rule
  , macLearning
  , PacketOutChan
  ) where

import Base
import qualified Flows
import qualified Nettle.OpenFlow as OF
import HFT
import qualified Data.HashTable as Ht
import qualified Data.HList as HList
import qualified Data.Set as Set
import System.Log.Logger.TH (deriveLoggers)
import qualified System.Log.Logger as Logger

$(deriveLoggers "Logger" [Logger.DEBUG, Logger.ERROR])

mtDiff mt1@(MatchTable tbl1) mt2@(MatchTable tbl2) =
  Set.difference (Set.fromList tbl1) (Set.fromList tbl2)

getPacketMac pkt = case OF.enclosedFrame pkt of
  Right (HList.HCons ethHdr _ ) -> 
    Just (srcPort, srcMac, dstMac)
      where srcPort = OF.receivedOnPort pkt
            srcMac = OF.sourceMACAddress ethHdr
            dstMac = OF.destMACAddress ethHdr
  otherwise -> Nothing

rule :: Integer                       -- ^current time
     -> OF.SwitchID                   -- ^switch
     -> OF.PortID
     -> OF.EthernetAddress            -- ^destination Eth addr
     -> Maybe (OF.PortID, Integer)    -- ^output port, timeout
     -> (Flows.FlowGroup, Action)     -- ^(switch, match rule), action
rule now sw srcPort eth dstPort = (match, action) where
  match = Flows.fromSwitchMatch sw (OF.matchAny { OF.inPort = Just srcPort, OF.dstEthAddress = Just eth })
  action = case dstPort of
     Nothing -> Just (ReqOutPort (Just sw) OF.Flood, fromInteger $ now + 5)
     Just (portID, t) -> Just (ReqOutPort (Just sw) (OF.PhysicalPort portID), timeout)
                           -- XXX(adf): the first 24 ports of the euc cluster are fixed
                           -- we'll give this an idle timeout in ControllerService
                           where timeout = if (srcPort <= 24 && portID <= 24)
                                             then NoLimit
                                             else fromInteger $ t + 60

type PacketOutChan = Chan (OF.SwitchID, OF.TransactionID, OF.PacketOut)

macLearning :: Chan (OF.SwitchID, Bool)          -- ^switches (created/deleted)
            -> Chan (OF.TransactionID, Integer, OF.SwitchID, OF.PacketInfo)
                 -- ^packets seen by controller
            -> IO (Chan MatchTable, PacketOutChan)
macLearning switchChan packetInChan = do
  tableChan <- newChan
  packetOut <- newChan
  -- learned :: HashTable SwitchID (HashTable EthAddr (PortID, absolute time))
  learned <- Ht.new (==) ((Ht.hashInt).fromIntegral)
  hft <- newIORef emptyTable
  let -- Switch connects
      loop (Left (newSwitchID, True)) = do
        macs <- Ht.new (==) ((Ht.hashInt).fromIntegral.(OF.unpack64))
        Ht.insert learned newSwitchID macs
      -- Switch disconnects
      loop (Left (oldSwitchID, False)) = do
        Ht.delete learned oldSwitchID
      -- Learning a new flow
      loop (Right (xid, now, switchID, packet)) = case getPacketMac packet of
        Just (srcPort, srcMac, dstMac) -> do
          maybeFwdTbl <- Ht.lookup learned switchID
          case maybeFwdTbl of
            Nothing -> errorM $ "MAC learning error: no table"
            Just fwdTbl -> do
              debugM $ "RECV t=" ++ show now ++ " srcEth=" ++ 
                              show srcMac ++ " dstEth=" ++ show dstMac ++
                             " switch=" ++ show switchID ++ " port=" ++
                             show srcPort

              maybePortTime <- Ht.lookup fwdTbl srcMac
              -- now' avoid refreshing MAC learned rules on switches when
              -- lots of PacketIn messags appear in a short (2sec) interval.
              -- TODO(adf): This feels like a hack. what should we really be
              -- doing here?
              let now' = case maybePortTime of
                   Just (srcPort', now') | srcPort' == srcPort && now - now' <= 2 ->
                     now'
                   otherwise -> now
              -- (re)fresh the MAC learning table on the switch
              Ht.insert fwdTbl srcMac (srcPort, now')

              -- establish forwarding rule
              maybeDstPortTime <- Ht.lookup fwdTbl dstMac
              let singleTbl = MatchTable 
                    [ rule now switchID srcPort dstMac maybeDstPortTime ]
--
-- Send the packet along
--
              case OF.bufferID packet of
                Nothing -> return ()
                Just bufID -> do
                  let action = case maybeDstPortTime of
                        Nothing -> OF.flood
                        Just (port, _) -> OF.sendOnPort port
-- TODO(adf): aw, crap. the packet we send out, thinking that we're doing
-- the right thing, could be a packet which would be dropped by a Deny rule,
-- which would only be detected once the paneTbl and macLearnedTbl were unioned
-- this is probably very rare, but could happen if a host moved in the network,
-- and the NIB has not been updated with the moved location, and therefore
-- the compiler has not moved the deny rule to the switch where the host is
-- now attached.
                  writeChan packetOut
                    (switchID, xid, 
                     OF.PacketOutRecord (Left bufID) (Just srcPort) action)
--
-- Write a rule for Mac Learning
--
              oldTbl <- readIORef hft
              -- NB: "now" is the time at which the PacketIn was received
              -- it's ok for us to condense up to that point as its in the past
              let hft' = condense now $
                                  unionTable (\_ new -> new) oldTbl singleTbl
              unless (Set.null (mtDiff hft' oldTbl)) $ do
                 writeIORef hft hft'
                 writeChan tableChan hft'
        Nothing -> return ()
  msgChan <- mergeChan switchChan packetInChan
  forkIO $ forever (readChan msgChan >>= loop)
  return (tableChan, packetOut)

        
