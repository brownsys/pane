module EmitFML 
  ( emitFML
  , emitActions
  ) where

import FlowController hiding (request)
import Text.PrettyPrint.HughesPJ
import qualified Data.Maybe as Maybe
import qualified Set as Set
import Data.List hiding (drop)
import Prelude hiding (drop)
import Set (Set)
import Base hiding (Port)
import qualified Base as Base
import Nettle.OpenFlow
import Data.Word
import qualified Flows as Flows

emitActions :: State -> Shared
emitActions st = 
  (admissionControlActions (currentRequests st) st, 
   resvActions  (eventsNow st) st)


resvActions' :: Integer
             -> Req
             -> [(Match, Word16, Limit)]
resvActions' now (Req {reqFlows=flows,reqData=ReqResv n, reqEnd=end}) = 
  [ (flowToMatch f, fromIntegral n, end - fromInteger now) | f <- Flows.expand flows ]
resvActions' _ _ = error "resvActions' expected ReqResv"

resvActions allReqs st = concatMap (resvActions' (stateNow st)) reqs
    where reqs = filter (not.isAdmControl) allReqs

admissionControlActions reqs st =
  let admReqs = filter isAdmControl reqs
      admReqs' = sortBy (\x y -> compare (reqDepth y st) (reqDepth x st)) admReqs
      now = stateNow st
  -- TODO: composition of allow/deny for sibliings is broken here.
  --       well, one hack could be to put the deny rules at the odd priorities (1,3,5,...)
  --       and the allow rules at the even priorities (2,4,6,...) but that would not help us
  --       with the situation below.
  -- TODO: also, will OpenFlow automatically use the most specific match rule at the same priority?
  -- or do we have to implement that ourselves? for example:
  -- root: allow(user=adf) on rootShare.
  -- root: deny(user=adf, dstHost=10.200.0.1) on rootShare.
  -- root: allow(user=adf, dstHost=10.200.0.1, srcHost=10.200.0.2) on rootShare.
  -- root: deny(user=adf, dstHost=10.200.0.1, srcHost=10.200.0.2, dstPort=80) on rootShare.
  -- (this is allowed by our rules at the moment; does the order affect what we think
  -- of this interaction? is this were partial/strict should affect us, say, if we
  -- re-arranged the order? oh! I think I should have used a subshare to implement this.
  -- still, PANE should probably have told me that it wouldn't achieve what I thought
  -- it was going to achieve because the 1st deny on line 2 will override the 2nd
  -- allow on line 3!)

  in concatMap (\(x, prio) -> requestAction now x prio) (zip admReqs' [65535,65534 ..])

requestAction now req@(Req {reqFlows=flowGroup, reqData=rd, reqEnd=end}) prio = 
  let flows = Flows.expand flowGroup
      timeOut = case end of
        NoLimit -> Permanent
        DiscreteLimit n -> ExpireAfter (fromIntegral $ n - now) -- TODO: blows up if > 65K
    in case rd of
      ReqAllow -> 
        let flowEntry flow = AddFlow {
              match = flowToMatch flow,
              priority = prio,
              actions = flood, -- TODO: HACK! what we mean is that it should be handled by the forwarding algorithm
              cookie = 0,
              idleTimeOut = Permanent,
              hardTimeOut = timeOut,
              notifyWhenRemoved = False,
              applyToPacket = Nothing,
              overlapAllowed = True
            } in map (\f -> FlowMod (flowEntry f)) flows
      ReqDeny -> 
        let flowEntry flow = AddFlow {
              match = flowToMatch flow,
              priority = prio,
              actions = drop,
              cookie = 0,
              idleTimeOut = Permanent,
              hardTimeOut = timeOut,
              notifyWhenRemoved = False,
              applyToPacket = Nothing,
              overlapAllowed = True
            } in map (\f -> FlowMod (flowEntry f)) flows

emitFML :: State
        -> String
emitFML st = render (requests (currentRequests st) st)

requests reqs st = admControl reqs st $$ nonAdmControl reqs st

admControl reqs st =
  let admReqs = filter isAdmControl reqs
      admReqs' = sortBy (\x y -> compare (reqDepth y st) (reqDepth x st)) admReqs
  in vcat $ map (\x -> request x st) admReqs'

nonAdmControl reqs st = vcat $ map (\x -> request x st) (filter (not.isAdmControl) reqs)

flowToMatch :: Flow -> Match
flowToMatch (Flow srcUser destUser srcPort dstPort srcIP dstIP) =
  matchAny {
    srcTransportPort = srcPort,
    dstTransportPort = dstPort,
    srcIPAddress = toIPPrefix srcIP,
    dstIPAddress = toIPPrefix dstIP,
    ethFrameType = Just ethTypeIP -- TODO: Will eventually need to match on MAC addrs as well
  }

toIPPrefix Nothing = (IPAddress 0, 0)
toIPPrefix (Just ip) = (ip, maxPrefixLen)


var _ Nothing = Nothing
var v (Just s) = Just (text v <+> text "=" <+> text (show s))



flow (Flow su ru sp rp sh rh) =
  parens $ cat $ punctuate (text ", ") $ 
    Maybe.catMaybes [ var "U_s" su, var "U_r" ru, 
                      var "P_s" sp, var "P_r" rp,
                      var "H_s" sh, var "H_r" rh ]




request req@(Req {reqFlows=flowGroup, reqData=rd}) st = 
  case rd of
    (ReqResv n) -> vcat [ text "bandwidth(" <> text (show n) <> text ") <=" <+> flow  f
                      | f <- Flows.expand flowGroup ]
    (ReqAllow) -> vcat [ text "P" <> text (show (reqDepth req st)) <> text ": allow <=" <+> flow f
                      | f <- Flows.expand flowGroup ]
    (ReqDeny) -> vcat [ text "P" <> text (show (reqDepth req st)) <> text ": deny <=" <+> flow f
                      | f <- Flows.expand flowGroup ]

--  text "bandwidth(" $$ text (show n) $$ text ") <=" $+$ flow f
