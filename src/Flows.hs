module Flows
  ( Flow (..)
  , FlowGroup
  , flowInGroup
  , User
  , Host
  , Port
  , union
  , intersection
  , null
  , all
  , isSubFlow
  , isOverlapped
  , expand
  , make
  ) where

import Prelude hiding (null, all)
import Data.Word (Word16)
import Set (Set)
import qualified Set as Set
import Nettle.IPv4.IPAddress

type User = String

type Port = Word16

type Host = IPAddress

data Flow = Flow (Maybe User) (Maybe User) 
                 (Maybe Port) (Maybe Port) 
                 (Maybe Host) (Maybe Host)

isSubFlow :: FlowGroup -> FlowGroup -> Bool
isSubFlow (FlowGroup fs1 fr1 fsp1 fdp1 fsh1 fdh1)
          (FlowGroup fs2 fr2 fsp2 fdp2 fsh2 fdh2) =
  Set.isSubsetOf fs1 fs2 &&
  Set.isSubsetOf fr1 fr2 &&
  Set.isSubsetOf fsp1 fsp2 &&
  Set.isSubsetOf fdp1 fdp2 &&
  Set.isSubsetOf fsh1 fsh2 &&
  Set.isSubsetOf fdh1 fdh2

isOverlapped :: FlowGroup -> FlowGroup -> Bool
isOverlapped (FlowGroup fs1 fr1 fsp1 fdp1 fsh1 fdh1)
             (FlowGroup fs2 fr2 fsp2 fdp2 fsh2 fdh2) =
  not (Set.null (Set.intersection fs1 fs2)) &&
  not (Set.null (Set.intersection fr1 fr2)) &&
  not (Set.null (Set.intersection fsp1 fsp2)) &&
  not (Set.null (Set.intersection fdp1 fdp2)) &&
  not (Set.null (Set.intersection fsh1 fsh2)) &&
  not (Set.null (Set.intersection fdh1 fdh2))


data FlowGroup = FlowGroup {
  flowSend :: Set User,
  flowRecv :: Set User,
  flowSrcPort :: Set Port,
  flowDstPort :: Set Port,
  flowSrcHost :: Set Host,
  flowDstHost :: Set Host 
} deriving (Ord, Eq, Show)

make = FlowGroup

all = FlowGroup Set.all Set.all Set.all Set.all Set.all Set.all

flowInGroup :: Flow -> FlowGroup -> Bool
flowInGroup (Flow srcUser dstUser srcPort dstPort srcHost dstHost)
            (FlowGroup srcUserG dstUserG srcPortG dstPortG srcHostG dstHostG) =
  srcUser `mem` srcUserG && dstUser `mem` dstUserG &&
  srcPort `mem` srcPortG && dstPort `mem` dstPortG &&
  srcHost `mem` srcHostG && dstHost `mem` dstHostG
    where mem Nothing  set = set == Set.all
          mem (Just e) set = e `Set.member` set

union :: FlowGroup -> FlowGroup -> FlowGroup
union (FlowGroup su1 du1 sp1 dp1 sh1 dh1) (FlowGroup su2 du2 sp2 dp2 sh2 dh2) =
  FlowGroup (Set.union su1 su2) (Set.union du1 du2) 
            (Set.union sp1 sp2) (Set.union dp1 dp2)
             (Set.union sh1 sh2) (Set.union dh1 dh2)

intersection :: FlowGroup -> FlowGroup -> FlowGroup
intersection (FlowGroup su1 du1 sp1 dp1 sh1 dh1) 
             (FlowGroup su2 du2 sp2 dp2 sh2 dh2) =
  FlowGroup (Set.intersection su1 su2) (Set.intersection du1 du2)
            (Set.intersection sp1 sp2) (Set.intersection dp1 dp2)
             (Set.intersection sh1 sh2) (Set.intersection dh1 dh2)

-- |'null fg' is true if 'fg' does not match any 'Flow'.
null :: FlowGroup -> Bool
null (FlowGroup su du sp dp sh dh) = 
  Set.null su || Set.null du || Set.null sp || Set.null dp || 
  Set.null sh || Set.null dh

expand :: FlowGroup -> [Flow]
expand (FlowGroup sendUser recvUser sendPort recvPort sendHost recvHost) = 
  [ Flow su ru sp rp sh rh | su <- toList' sendUser, ru <- toList' recvUser,
                             sp <- toList' sendPort, rp <- toList' recvPort,
                             sh <- toList' sendHost, rh <- toList' recvHost ]
    where toList' s = case Set.toList s of
            Just lst -> map Just lst
            Nothing -> [Nothing]
