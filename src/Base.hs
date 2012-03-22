module Base 
  ( Flow (..)
  , FlowGroup
  , User
  , Host
  , Port
  , flowInGroup
  , traceFile
  , Shared (..)
  , DNPResult (..)
  , Limit (..) 
  , Time (..)
  , timeToLimit
  , timeToInteger
  , Speaker
  , ShareRef
  , Req (..)
  , ReqData (..)
  , parseIPAddress
  ) where

import System.IO.Unsafe
import Data.IORef
import Set (Set)
import qualified Set as Set
import System.IO (Handle)
import Data.Aeson
import qualified Data.Tree as Tree
import Text.PrettyPrint.HughesPJ
import Data.Word
import Nettle.IPv4.IPAddress
import Nettle.OpenFlow hiding (Port)
import Flows

traceFile :: IORef (Maybe Handle)
traceFile = unsafePerformIO (newIORef Nothing)

-- |Data shared between the OpenFlow Controller and the PANE Server.
type Shared = ([CSMessage], [(Match, Word16, Limit)])

data DNPResult
  = BoolResult Bool
  | ScheduleResult [(Limit, Limit, Limit)]
  | ShareRefsResult [ShareRef]
  deriving Eq

renderResult (BoolResult b)       = text (show b)
renderResult (ScheduleResult lst) = cat $ punctuate (text "; ") $ (len:(map f lst))
  where len = text (show (length lst))
        f (t, bw, toks) = cat $ punctuate (text ",") $ 
                            [renderLimit t, renderLimit bw, renderLimit toks]
renderResult (ShareRefsResult lst) =
  cat $ punctuate (text "; ") $ (len:(map text lst))
  where len = text (show (length lst))

renderLimit lim = text (show lim)

instance Show DNPResult where
  show r = render (renderResult r)

data Limit 
  = NoLimit 
  | DiscreteLimit Integer 
  deriving (Eq)

instance Show Limit where
  show (DiscreteLimit n) = show n
  show NoLimit           = "inf"

instance Ord Limit where
  _ <= NoLimit = True -- TODO(arjun): NoLimit <= NoLimit should be an error
  (DiscreteLimit m) <= (DiscreteLimit n) = m <= n
  NoLimit <= (DiscreteLimit _) = False

instance Num Limit where
  (DiscreteLimit m) + (DiscreteLimit n) = DiscreteLimit (m + n)
  _                 + _                 = NoLimit

  (DiscreteLimit m) - (DiscreteLimit n) = DiscreteLimit (m - n)
  NoLimit           - NoLimit           = error "NoLimit - NoLimit"
  NoLimit           - (DiscreteLimit n) = NoLimit
  (DiscreteLimit n) - NoLimit           = error "DiscreteLimit _ - NoLimit"

  (DiscreteLimit m) * (DiscreteLimit n) = DiscreteLimit (m * n)
  _                 * (DiscreteLimit 0) = DiscreteLimit 0
  (DiscreteLimit 0) * _                 = DiscreteLimit 0
  _                 * _                 = NoLimit

  fromInteger n = DiscreteLimit n

  abs (DiscreteLimit m) = DiscreteLimit (abs m)
  abs NoLimit           = NoLimit

  signum (DiscreteLimit m) = DiscreteLimit (signum m)
  signum NoLimit           = error "signum NoLimit"

instance Enum Limit where
  succ (DiscreteLimit m) = DiscreteLimit (succ m)
  succ NoLimit           = NoLimit
  toEnum n = DiscreteLimit (fromIntegral n)
  enumFrom (DiscreteLimit n) = 
    (DiscreteLimit n):(enumFrom (DiscreteLimit (n+1)))
  enumFrom NoLimit           = [NoLimit]
  fromEnum (DiscreteLimit n) = fromIntegral n
  fromEnum NoLimit           = error "fromEnum NoLimit"

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
  toJSON _ = "TODO flow flow"

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
