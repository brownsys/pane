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

-- TODO(arjun): toTimeout will fail if (end - now) does not fit in a Word16
toTimeout :: Integer -> Limit -> OF.TimeOut
toTimeout _   NoLimit = 
  OF.Permanent
toTimeout now (DiscreteLimit end) = 
  OF.ExpireAfter (fromInteger (end - fromInteger now))

-- |
compile :: Integer -> NIB.Network -> MatchTable -> Map OF.SwitchID NIB.Switch
compile now net (MatchTable tbl) = foldl loop (NIB.switches net) tbl where
  -- TODO(arjun): foldr loop and rule:flows instead of flows ++ [rule]
  loop switches (_, Nothing) = switches -- TODO(arjun): Maybe is silly here
  loop switches (fl, Just (ReqDeny, end)) = case NIB.path net (toMatch fl) of
    Nothing -> error "cannot calculate path"
    Just [] -> error "empty path for Deny"
    Just ((sid, inp, _):_) -> Map.adjust upd sid switches
      where upd (NIB.Switch ports flows) =  NIB.Switch ports (flows ++ [rule])
            rule = (Flows.toMatch fl, 
                    [], 
                    toTimeout now end)
  loop switches (fl, Just (ReqAllow, end)) = case NIB.path net (toMatch fl) of
    Nothing -> error "cannot calc path"
    Just [] -> error "empty path for Allow"
    Just ((sid, inp, outp):_) -> Map.adjust upd sid switches
      where upd (NIB.Switch ports flows) = NIB.Switch ports (flows ++ [rule])
            rule = (Flows.toMatch fl, 
                    [OF.SendOutPort (OF.PhysicalPort outp)], 
                    toTimeout now end)
  loop switches (fl, Just (ReqResv bw, end)) = case NIB.path net (toMatch fl) of
    Nothing -> error "cannot find path"
    Just path -> foldl queue switches path
      where queue switches (swid, inp, outp) = 
              Map.adjust (updSwitch inp outp) swid switches
            updSwitch inp outp (NIB.Switch ports flows) = 
              NIB.Switch ports' (flows ++ [rule])
                where (queueID, ports') = 
                        NIB.newQueue ports outp bw' (end - fromInteger now)
                      bw' = fromInteger bw -- TODO(arjun): requres fit in Word16
                      rule = (Flows.toMatch fl, 
                              [OF.Enqueue outp queueID],
                              toTimeout now end)

     



