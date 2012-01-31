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

emitActions :: State -> [CSMessage]
emitActions st = admissionControlActions (currentRequests st) st

admissionControlActions reqs st =
  let admReqs = filter isAdmControl reqs
      admReqs' = sortBy (\x y -> compare (reqDepth y st) (reqDepth x st)) admReqs
      now = stateNow st
  -- TODO: composition of allow/deny for sibliings is broken here.
  in concatMap (\(x, prio) -> requestAction now x prio) (zip admReqs' [1 ..])

requestAction now req@(Req {reqFlows=flowGroup, reqData=rd, reqEnd=end}) prio = 
  let flows = expandFlowGroup flowGroup
      timeOut = case end of
        NoLimit -> Permanent
        DiscreteLimit n -> ExpireAfter (fromIntegral $ n - now) -- TODO: blows up if > 65K
    in case rd of
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

data Flow = Flow (Maybe User) (Maybe User) (Maybe Base.Port) (Maybe Base.Port) (Maybe Host) (Maybe Host)

flowToMatch :: Flow -> Match
flowToMatch (Flow srcUser destUser srcPort dstPort srcIP dstIP) =
  matchAny {
    srcTransportPort = srcPort,
    dstTransportPort = dstPort,
    srcIPAddress = toIPPrefix srcIP,
    dstIPAddress = toIPPrefix dstIP,
    ethFrameType = Just ethTypeIP
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



expandFlowGroup :: FlowGroup -> [Flow]
expandFlowGroup (FlowGroup sendUser recvUser sendPort recvPort sendHost recvHost) = 
  [ Flow su ru sp rp sh rh | su <- toList' sendUser, ru <- toList' recvUser,
                             sp <- toList' sendPort, rp <- toList' recvPort,
                             sh <- toList' sendHost, rh <- toList' recvHost ]
    where toList' s = case Set.toList s of
            Just lst -> map Just lst
            Nothing -> [Nothing]

request req@(Req {reqFlows=flowGroup, reqData=rd}) st = 
  case rd of
    (ReqResv n) -> vcat [ text "bandwidth(" <> text (show n) <> text ") <=" <+> flow  f
                      | f <- expandFlowGroup flowGroup ]
    (ReqAllow) -> vcat [ text "P" <> text (show (reqDepth req st)) <> text ": allow <=" <+> flow f
                      | f <- expandFlowGroup flowGroup ]
    (ReqDeny) -> vcat [ text "P" <> text (show (reqDepth req st)) <> text ": deny <=" <+> flow f
                      | f <- expandFlowGroup flowGroup ]

--  text "bandwidth(" $$ text (show n) $$ text ") <=" $+$ flow f
