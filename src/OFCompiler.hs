module OFCompiler
  ( compile
  ) where

import ShareSemantics (MatchTable (..))
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

-- TODO(arjun): toTimeout will fail if (end - now) does not fit in a Word16
toTimeout :: Integer -> Limit -> OF.TimeOut
toTimeout _   NoLimit = 
  OF.Permanent
toTimeout now (DiscreteLimit end) = 
  OF.ExpireAfter (fromInteger (end - fromInteger now))

-- |
compile :: Integer -> NIB.NIB -> MatchTable -> IO (Map OF.SwitchID NIB.Switch)
compile now nib (MatchTable tbl) = do
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
      loop :: Map OF.SwitchID NIB.Switch -> (FlowGroup, Maybe (ReqData, Limit))
           -> IO (Map OF.SwitchID NIB.Switch)
      loop switches (_, Nothing) = do
        return switches -- TODO(arjun): Maybe is silly here
      loop switches (fl, Just (ReqOutPort switchID portID, end)) =
        case Flows.flowSwitchMatch fl of
          Nothing -> do
            putStrLn "compile error: ReqOutPort does not match a switch"
            return switches
          Just (_, match) -> return (Map.adjust upd switchID switches)
            where upd (NIB.Switch ports flows) =
                    NIB.Switch ports (flows ++ [rule])
                  rule = (match, [OF.SendOutPort (OF.PhysicalPort portID)],
                          toTimeout now end)
      loop switches (fl, Just (ReqDeny, end)) = 
        --TODO(arjun):error?
        withEth fl (return switches) $ \srcEth dstEth -> do
          path <- NIB.getPath srcEth dstEth nib
          case path of
            [] -> return switches -- TODO(arjun): error?
            ((inp, sid,  _):_) -> return (Map.adjust upd sid switches)
              where upd (NIB.Switch ports flows) =  
                      NIB.Switch ports (flows ++ [rule])
                    rule = (Flows.toMatch fl, [],  toTimeout now end)
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
                    rule = (Flows.toMatch fl, [port], toTimeout now end)
      loop switches (fl, Just (ReqResv bw, end)) =
        --TODO(arjun):error?
        withEth fl (return switches) $ \srcEth dstEth -> do
          path <- NIB.getPath srcEth dstEth nib -- TODO(arjun): error on empty?
          let queue switches (inp, swid, outp) = 
                  Map.adjust (updSwitch inp outp) swid switches
              updSwitch inp outp (NIB.Switch ports flows) = 
                NIB.Switch ports' (flows ++ [rule])
                  where (queueID, ports') = 
                          NIB.newQueue ports outp bw' (end - fromInteger now)
                        bw' = fromInteger bw -- TODO(arjun): fit in Word16
                        rule = (Flows.toMatch fl, 
                                [OF.Enqueue outp queueID],
                                toTimeout now end)
          return (foldl queue switches path)
  snap <- NIB.snapshot nib
  foldM loop snap tbl
