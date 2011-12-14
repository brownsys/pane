module StateDump
  (
  ) where

import qualified PriorityQueue as PQ
import PriorityQueue (PQ)
import Base
import FlowController
import Data.Aeson
import qualified Tree as T
import qualified Data.Tree as Tree

instance ToJSON Limit where
  toJSON NoLimit           = Null
  toJSON (DiscreteLimit n) = toJSON n

instance ToJSON FlowGroup where
  toJSON (FlowGroup srcUser destUser srcPort destPort) =
    object [ ("srcUser", toJSON srcUser)
           , ("destUser", toJSON destUser)
           , ("srcPort", toJSON srcPort)
           , ("destPort", toJSON destPort)
           ]

instance ToJSON Req where
  toJSON (Req share flows start end typ strict) =
    object [ ("share", toJSON share)
           , ("flows", toJSON flows)
           , ("start", toJSON start)
           , ("end", toJSON end)
           , ("data", toJSON typ)
           , ("strict", toJSON strict)
           ]

instance ToJSON ReqData where
  toJSON (ReqResv n) = object [ ("reserve", toJSON n) ]
  toJSON ReqAllow    = object [ ("allow", Bool True) ]
  toJSON ReqDeny     = object [ ("deny", Bool False) ]

instance ToJSON a => ToJSON (Tree.Tree a) where
  toJSON (Tree.Node val children) = 
    object [ ("item", toJSON val), ("children", toJSON children) ]

instance ToJSON Share where
  toJSON share = object
    [ ("name", toJSON (shareName share))
    , ("flows", toJSON (shareFlows share))
    , ("holders", toJSON (shareHolders share))
    , ("req", toJSON (shareReq share))
    , ("resvLimit", toJSON (shareResvLimit share))
    , ("canAllow", toJSON (shareCanAllowFlows share))
    , ("canDeny", toJSON (shareCanDenyFlows share))
    ]
instance ToJSON State where
  toJSON (State shares speakers accepted active now) = object
    [ ("shares", toJSON (T.expose shares))
    , ("speakers", toJSON speakers)
    , ("accepted", toJSON accepted)
    , ("now", toJSON now)
    ]
