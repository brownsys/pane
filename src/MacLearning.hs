module MacLearning 
  ( rule
  , macLearning
  , PacketOutChan
  ) where

import Base
import qualified Flows
import qualified Nettle.OpenFlow as OF
import ShareSemantics
import qualified Data.HashTable as Ht
import qualified Data.HList as HList

getPacketMac pkt = case OF.enclosedFrame pkt of
  Right (HList.HCons ethHdr _ ) -> 
    Just (srcPort, srcMac, dstMac)
      where srcPort = OF.receivedOnPort pkt
            srcMac = OF.sourceMACAddress ethHdr
            dstMac = OF.destMACAddress ethHdr
  otherwise -> Nothing

rule :: Integer
     -> OF.SwitchID -> OF.EthernetAddress -> Maybe (OF.PortID, Integer)
     -> (Flows.FlowGroup, Action)
rule now sw eth port =  
  (Flows.fromSwitchMatch sw (OF.matchAny { OF.dstEthAddress = Just eth }),
   case port of
     Nothing -> Just (ReqOutPort sw OF.Flood, fromInteger $ now + 5)
     Just (portID, t) -> Just (ReqOutPort sw (OF.PhysicalPort portID), 
                               fromInteger t))

type PacketOutChan = Chan (OF.SwitchID, OF.TransactionID, OF.PacketOut)

macLearning :: Chan (OF.SwitchID, Bool)          -- ^switches (created/deleted)
            -> Chan (OF.TransactionID, Integer, OF.SwitchID, OF.PacketInfo)
                 -- ^packets seen by controller
            -> IO (Chan MatchTable, PacketOutChan)
macLearning switchChan packetChan = do
  msgChan <- mergeChan switchChan packetChan
  outChan <- newChan
  packetOut <- newChan
  -- learned :: HashTable SwitchID (HashTable EthernetAddress PortID)
  learned <- Ht.new (==) ((Ht.hashInt).fromIntegral)
  tbl <- newIORef emptyTable
  let loop (Left (newSwitchID, True)) = do
        macs <- Ht.new (==) ((Ht.hashInt).fromIntegral.(OF.unpack64))
        Ht.insert learned newSwitchID macs
      loop (Left (oldSwitchID, False)) = do
        Ht.delete learned oldSwitchID
      loop (Right (xid, now, switchID, packet)) = case getPacketMac packet of
        Just (srcPort, srcMac, dstMac) -> do
          maybeFwdTbl <- Ht.lookup learned switchID
          case maybeFwdTbl of
            Nothing -> putStrLn "MAC learning error: no table"
            Just fwdTbl -> do
              maybe <- Ht.lookup fwdTbl srcMac
              -- now' avoid refreshing MAC learned rules on switches when
              -- lots of PacketIn messags appear in a short (2sec) interval.
              let now' = case maybe of
                   Just (srcPort', now') | srcPort' == srcPort && now' + 2 < now ->
                     now'
                   otherwise -> now
              Ht.insert fwdTbl srcMac (srcPort, now')
              maybeDstPortTime <- Ht.lookup fwdTbl dstMac
              let singleTbl = MatchTable 
                    [rule now switchID srcMac (Just (srcPort, now')),
                     rule now switchID dstMac maybeDstPortTime ]
              case OF.bufferID packet of
                Nothing -> return ()
                Just bufID -> do
                  let action = case maybeDstPortTime of
                        Nothing -> OF.flood
                        Just (port, _) -> OF.sendOnPort port
                  putStrLn $ "RECV t=" ++ show now ++ " srcEth=" ++ 
                              show srcMac ++
                             " switch= " ++ show switchID ++ " port=" ++ 
                             show srcPort
                  writeChan packetOut
                    (switchID, xid, 
                     OF.PacketOutRecord (Left bufID) (Just srcPort) action)
              oldTbl <- readIORef tbl
              let tbl' = condense $ unionTable (\_ new -> new) oldTbl singleTbl
              writeIORef tbl tbl'
              writeChan outChan tbl'
        Nothing -> return ()
  forkIO $ forever (readChan msgChan >>= loop)
  return (outChan, packetOut)

        
