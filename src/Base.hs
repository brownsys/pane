module Base where

import Set (Set)

data Limit 
  = NoLimit 
  | DiscreteLimit Integer 
  deriving (Eq, Show)

instance Ord Limit where
  _ <= NoLimit = True
  (DiscreteLimit m) <= (DiscreteLimit n) = m <= n
  NoLimit <= (DiscreteLimit _) = False

addLimits (DiscreteLimit m) (DiscreteLimit n) = DiscreteLimit (m + n)
addLimits _                 _                 = NoLimit

data Time
  = Relative Integer -- ^relative to now
  | Absolute Integer
  | Forever
  deriving (Ord, Eq, Show)

timeToLimit :: Integer -> Time -> Limit
timeToLimit now (Relative delta) = DiscreteLimit (now + delta)
timeToLimit _   (Absolute t)     = DiscreteLimit t
timeToLimit _   Forever          = NoLimit 

timeToInteger :: Integer -> Time -> Integer
timeToInteger now t = case timeToLimit now t of
  DiscreteLimit n -> n
  NoLimit -> error "timeToInteger _ Forever"

type Speaker = String

type User = String
type Port = Integer

data FlowGroup = FlowGroup {
  flowSend :: Set User,
  flowRecv :: Set User,
  flowSrcPort ::  Set Port,
  flowDestPort :: Set Port
} deriving (Ord, Eq, Show)


type ShareRef = String

data Req = Req {
  reqShare :: ShareRef,
  reqFlows :: FlowGroup,
  reqStart :: Integer, -- invariant: start < end
  reqEnd :: Limit,
  reqData :: ReqData
} deriving (Show, Ord, Eq)

data ReqData = ReqResv Integer
             | ReqAllow
             | ReqDeny
             deriving (Eq, Ord, Show)
