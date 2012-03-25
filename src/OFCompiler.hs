module OFCompiler
  ( compile
  , compilerService
  ) where

import ShareSemantics (MatchTable (..), emptyTable)
import Base
import qualified Nettle.OpenFlow as OF
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified NIB
import Flows (toMatch)
import Control.Monad
import qualified Flows


compilerService :: (NIB.NIB, Chan NIB.NIB)
                -> Chan MatchTable
                -> IO (Chan NIB.Snapshot)
compilerService (initNIB, nib) tbl =
  unionChan compile (initNIB, nib) (emptyTable, tbl)

compile :: NIB.NIB -> MatchTable -> IO (Map OF.SwitchID NIB.Switch)
compile nib (MatchTable tbl) = do
  let -- TODO(arjun): foldr loop and rule:flows instead of flows ++ [rule]
      withEth flow default_ k = do
        let match = toMatch flow
        case (OF.srcIPAddress match, OF.dstIPAddress match) of
          ((srcIP, 32), (dstIP, 32)) -> do
            srcEth <- NIB.getEthFromIP srcIP nib
            dstEth <- NIB.getEthFromIP dstIP nib
            case (srcEth, dstEth) of
              (Just s, Just d) -> k s d
              otherwise -> default_
          otherwise -> default_
      loop :: Map OF.SwitchID NIB.Switch 
           -> (FlowGroup, Maybe (ReqData, Limit))
           -> IO (Map OF.SwitchID NIB.Switch)
      loop switches (_, Nothing) = do
        return switches -- TODO(arjun): Maybe is silly here
      loop switches (fl, Just (ReqOutPort switchID pseudoPort, end)) =
        case Flows.flowSwitchMatch fl of
          Nothing -> do
            putStrLn "compile error: ReqOutPort does not match a switch"
            return switches
          Just (_, match) -> return (Map.adjust upd switchID switches)
            where upd (NIB.Switch ports flows) =
                    NIB.Switch ports (flows ++ [rule])
                  rule = (match, [OF.SendOutPort pseudoPort], end)
      loop switches (fl, Just (ReqDeny, end)) = 
        --TODO(arjun):error?
        withEth fl (return switches) $ \srcEth dstEth -> do
          path <- NIB.getPath srcEth dstEth nib
          case path of
            [] -> return switches -- TODO(arjun): error?
            ((inp, sid,  _):_) -> return (Map.adjust upd sid switches)
              where upd (NIB.Switch ports flows) =  
                      NIB.Switch ports (flows ++ [rule])
                    rule = (Flows.toMatch fl, [],  end)
      loop switches (fl, Just (ReqAllow, end)) = 
        -- TODO(arjun): error?
        withEth fl (return switches) $ \srcEth dstEth -> do
          path <- NIB.getPath srcEth dstEth nib
          case path of
            [] -> return switches -- TODO(arjun): error?
            ((inp, sid, outp):_) -> return (Map.adjust upd sid switches)
              where upd (NIB.Switch ports flows) = 
                      NIB.Switch ports (flows ++ [rule])
                    port = OF.SendOutPort (OF.PhysicalPort outp)
                    rule = (Flows.toMatch fl, [port], end)
      loop switches (fl, Just (ReqResv bw, end)) =
        --TODO(arjun):error?
        withEth fl (return switches) $ \srcEth dstEth -> do
          path <- NIB.getPath srcEth dstEth nib -- TODO(arjun): error on empty?
          let queue switches (inp, swid, outp) = 
                  Map.adjust (updSwitch inp outp) swid switches
              updSwitch inp outp (NIB.Switch ports flows) = 
                NIB.Switch ports' (flows ++ [rule])
                  where (queueID, ports') = 
                          NIB.newQueue ports outp bw' end
                        bw' = fromInteger bw -- TODO(arjun): fit in Word16
                        rule = (Flows.toMatch fl, 
                                [OF.Enqueue outp queueID],
                                end)
          return (foldl queue switches path)
  snap <- NIB.snapshot nib
  putStrLn $ "Compiler recalculating network configuration."
  foldM loop snap tbl
