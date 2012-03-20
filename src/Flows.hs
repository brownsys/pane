module Flows
  ( Flow (..)
  , FlowGroup (..)
  , flowInGroup
  , User
  , Host
  , Port
  ) where

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

