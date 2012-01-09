module Base where

import System.IO.Unsafe
import Data.IORef
import Set (Set)
import System.IO (Handle)
import Data.Aeson
import qualified Data.Tree as Tree
import Text.PrettyPrint.HughesPJ


traceFile :: IORef (Maybe Handle)
traceFile = unsafePerformIO (newIORef Nothing)

data DNPResult
  = BoolResult Bool
  | ScheduleResult [(Limit, Limit, Limit)]
  | ShareRefsResult [ShareRef]
  deriving Eq

renderResult (BoolResult b)       = text (show b)
renderResult (ScheduleResult lst) = cat $ punctuate (text "; ") $ (len:(map f lst))
  where len = text (show (length lst))
        f (t, bw, toks) = cat $ punctuate (text ",") $ 
                            [renderLimit t,  renderLimit bw, renderLimit toks]
renderResult (ShareRefsResult lst) =
  cat $ punctuate (text "; ") $ (len:(map text lst))
  where len = text (show (length lst))

renderLimit (DiscreteLimit n) = text (show n)
renderLimit NoLimit           = text "inf"

instance Show DNPResult where
  show r = render (renderResult r)

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

subLimits (DiscreteLimit m) (DiscreteLimit n) = DiscreteLimit (m - n)
subLimits NoLimit NoLimit = error "uh oh. what should we do?"
subLimits NoLimit (DiscreteLimit n) = NoLimit
subLimits (DiscreteLimit n) NoLimit = error "uh oh. another strange case."

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
type Host = String -- TODO: use strings as a crutch for now

data FlowGroup = FlowGroup {
  flowSend :: Set User,
  flowRecv :: Set User,
  flowSrcPort :: Set Port,
  flowDstPort :: Set Port,
  flowSrcHost :: Set Host,
  flowDstHost :: Set Host 
} deriving (Ord, Eq, Show)


type ShareRef = String

data Req = Req {
  reqShare :: ShareRef,
  reqFlows :: FlowGroup,
  reqStart :: Integer, -- invariant: start < end
  reqEnd :: Limit,
  reqData :: ReqData,
  reqStrict :: Bool
} deriving (Show, Ord, Eq)

data ReqData = ReqResv Integer
             | ReqAllow
             | ReqDeny
             deriving (Eq, Ord, Show)

instance ToJSON Limit where
  toJSON NoLimit           = Null
  toJSON (DiscreteLimit n) = toJSON n

instance ToJSON FlowGroup where
  toJSON (FlowGroup srcUser dstUser srcPort dstPort srcHost dstHost) =
    object [ ("srcUser", toJSON srcUser)
           , ("dstUser", toJSON dstUser)
           , ("srcPort", toJSON srcPort)
           , ("dstPort", toJSON dstPort)
           , ("srcHost", toJSON srcHost)
           , ("dstHost", toJSON dstHost)
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
