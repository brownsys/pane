module Flows
  ( Flow (..)
  , FlowGroup (..)
  , flowInGroup
  , User
  , Host
  , Port
  , union
  , intersection
  , null
  ) where

import Prelude hiding (null)
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

data FlowGroup = FlowGroup {
  flowSend :: Set User,
  flowRecv :: Set User,
  flowSrcPort :: Set Port,
  flowDstPort :: Set Port,
  flowSrcHost :: Set Host,
  flowDstHost :: Set Host 
} deriving (Ord, Eq, Show)

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
