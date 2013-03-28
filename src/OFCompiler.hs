module OFCompiler
  ( compile
  , compilerService
  ) where

import HFT (MatchTable (..), emptyTable)
import Base
import qualified Nettle.OpenFlow as OF
import qualified Nettle.OpenFlow.Match as OFMatch
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified NIB
import Flows (toMatch)
import Control.Monad
import qualified Flows
import System.Log.Logger.TH (deriveLoggers)
import qualified System.Log.Logger as Logger

$(deriveLoggers "Logger" [Logger.DEBUG, Logger.INFO, Logger.ERROR])

-- | Calls compile when either NIB or network flow table changes
-- TODO(adf): actually, the NIB changes do not casue a re-compile as the
-- NIB variable just silently changes and nothing written to NIB channel, right?
compilerService :: (NIB.NIB, Chan NIB.NIB)
                -> Chan MatchTable    -- ^ Network Flow Table
                -> IO (Chan NIB.Snapshot)
compilerService (initNIB, nib) tbl =
  liftChan compile (initNIB, nib) (emptyTable, tbl)

-- | Converts a switch's flow table to an OpenFlow table by groupping
-- consecutive, non-overlapping rules into the same OpenFlow priority level
toFlowTbl :: [(OF.Match, [OF.Action], Limit)]
          -> NIB.FlowTbl
toFlowTbl lst = Set.fromList prioritizedRules
  where isOverlapped (m, _, _) (m', _, _) = case OFMatch.intersect m m' of
          Just _  -> True
          Nothing -> False
        mergePriority group [] = (group, [])
        mergePriority group (hd:tl) = case List.find (isOverlapped hd) group of
          Just _  -> (group, hd:tl)
          Nothing -> mergePriority (hd:group) tl
        groupByPriority lst = case mergePriority [] lst of
          ([], [])      -> []
          (group, [])   -> [group]
          (group, lst') -> group:(groupByPriority lst')
        groups = groupByPriority lst
        prioritizedGroups = zip [ 65535, 65534 .. ] groups
        ruleWithPriority p (m, f, t) = (p, m, f, t)
        prioritizedRules = 
          List.concatMap (\(p, rules) -> map (ruleWithPriority p) rules)
                         prioritizedGroups

-- | Scan through the Network Flow Table and separate out the rules which
-- apply to each switch.
compile :: NIB.NIB          -- ^ actual NIB
        -> MatchTable       -- ^ Network Flow Table
        -> IO NIB.Snapshot  -- ^ NIB snapshot produced by compilation
compile nib (MatchTable tbl) = do
  let -- TODO(arjun): foldr loop and rule:flows instead of flows ++ [rule]
      withEth flow default_ k = case toMatch flow of
        Just match -> case (OF.srcIPAddress match, OF.dstIPAddress match) of
          ((srcIP, 32), (dstIP, 32)) -> do
            srcEth <- NIB.getEthFromIP srcIP nib
            dstEth <- NIB.getEthFromIP dstIP nib
            case (srcEth, dstEth) of
              (Just s, Just d) -> 
                  k flow s d
              otherwise -> do
                infoM $ "compiler could not find flow in NIB " ++ show flow
                default_
          otherwise -> do
            errorM $ "compiler needs IP for src and dst " ++ show flow
            default_
        Nothing -> do
          infoM $ "compiler cannot realize " ++ show flow ++ " (default)"
          default_
      loop switches (_, Nothing) = do
        return switches -- TODO(arjun): Maybe is silly here
      loop switches (fl, Just (ReqOutPort Nothing pseudoPort, end)) =
        case Flows.flowSwitchMatch fl of
          Nothing -> do
            errorM $ "compile error: ReqOutPort does not match a switch"
            return switches
          Just (_, match) -> return (Map.map upd switches)
            where upd (ports, flows, swtype) = (ports, flows ++ [rule], swtype)
                  rule = (match, [OF.SendOutPort pseudoPort], end)
      loop switches (fl, Just (ReqOutPort (Just switchID) pseudoPort, end)) =
        case Flows.flowSwitchMatch fl of
          Nothing -> do
            errorM $ "compile error: ReqOutPort does not match a switch"
            return switches
          Just (_, match) -> return (Map.adjust upd switchID switches)
            where upd (ports, flows, swtype) = (ports, flows ++ [rule], swtype)
                  rule = (match, [OF.SendOutPort pseudoPort], end)
      loop switches (flow, Just (ReqDeny, end)) = case Flows.toMatch flow of
        -- TODO(adf): if the flow has an IP src or dst, we should try looking
        -- for that IP in the NIB and instead of updating all of the switches,
        -- as the code below does, install the deny only on the closest switch.
        -- This corresponds to the unit test: testDeny2Switch
        Just match -> 
          return (Map.map upd switches)
            where upd (ports, flows, swtype) = (ports, flows ++ [rule], swtype)
                  rule = (match, [], end)
        Nothing -> do
          errorM $ "compile deny cannot realize " ++ show flow
          return switches -- TODO(arjun): error?
      loop switches (fl, Just (ReqAllow, end)) = 
        -- TODO(arjun): error?
        withEth fl (return switches) $ \fl srcEth dstEth -> do
          path <- NIB.getPath srcEth dstEth nib
          case path of
            [] -> return switches -- TODO(arjun): error?
            ((inp, sid, outp):_) -> return (Map.adjust upd sid switches)
              where upd (ports, flows, swtype) = (ports, flows ++ [rule], swtype)
                    port = OF.SendOutPort (OF.PhysicalPort outp)
                    rule = (Flows.toMatch' fl, [port], end)
      loop switches (fl, Just (ReqResv bw, end)) =
        --TODO(arjun):error?
        withEth fl (return switches) $ \fl srcEth dstEth -> do
          path <- NIB.getPath srcEth dstEth nib -- TODO(arjun): error on empty?
          let queue switches (inp, swid, outp) =
                  Map.adjust (updSwitch inp outp) swid switches
              updSwitch inp outp (ports, flows, swtype) = (ports', flows ++ [rule], swtype)
                where (queueID, ports') = NIB.newQueue ports outp bw' OF.Disabled end
                      bw' = OF.Enabled (fromInteger bw) -- TODO(arjun): fit in Word16
                      m   = Flows.toMatch' fl
                      rule = (m, [OF.Enqueue outp queueID], end)
          return (foldl queue switches path)
      loop switches (fl, Just (ReqRlimit rl, end)) =
        --TODO(arjun):error?
        withEth fl (return switches) $ \fl srcEth dstEth -> do
          path <- NIB.getPath srcEth dstEth nib -- TODO(arjun): error on empty?
          let queue switches (inp, swid, outp) =
                  Map.adjust (updSwitch inp outp) swid switches
              updSwitch inp outp (ports, flows, swtype) = (ports', flows ++ [rule], swtype)
                where (queueID, ports') = NIB.newQueue ports outp OF.Disabled rl' end
                      rl' = OF.Enabled (fromInteger rl) -- TODO(arjun): fit in Word16
                      m   = Flows.toMatch' fl
                      rule = (m, [OF.Enqueue outp queueID], end)
          return (foldl queue switches path)
  snap <- NIB.snapshot nib
  -- Recompile switches' flow tables from scratch, but remember port cfgs and types
  let cfgs = Map.map (\(NIB.Switch p _ t) -> (p, [], t)) snap
  cfgs' <- foldM loop cfgs tbl

  debugM $ "Policy:"
  mapM_ (\x -> debugM $ "    " ++ (show x)) tbl
  debugM "Compiled to :"
  mapM_ (\(_, (_, v, _)) -> debugM $ "  " ++ show v) (Map.toList cfgs')

  -- Produce output snapshot
  let f (ports, flows, swtype) = NIB.Switch ports (toFlowTbl flows) swtype
  return (Map.map f cfgs')
