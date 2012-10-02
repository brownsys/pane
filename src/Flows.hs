module Flows
  ( Flow (..)
  , FlowGroup
  , User
  , Host
  , Port
  , intersection
  , null
  , all
  , isSubFlow
  , isOverlapped
  , make
  , simple
  , toMatch
  , toMatch'
  , flowSwitchMatch
  , fromSwitchMatch
  , fromMatch
  ) where

import Prelude hiding (null, all)
import Data.Word (Word16)
import Set (Set)
import qualified Set as Set
import Nettle.IPv4.IPAddress
import qualified Nettle.IPv4.IPAddress as IPAddress
import Nettle.OpenFlow.Match
import qualified Nettle.OpenFlow.Match as Match
import qualified Nettle.OpenFlow as OF

-- TODO(adf): We have lost the richness of the original FlowGroups (from
-- Hot-ICE paper)... which supports sets of users, sets of srcIP, etc. We
-- should return to supporting those by implementing Flows as *sets* of
-- Match rules with duplicated actions.
-- TODO(adf): Might also be able to return "union" operation to Flows at
-- that time.
-- TODO(adf): At one time, we used to consider parsing "Applications" which
-- expanded into sets of parts, or "Network" which expanded into a set of
-- IP addresses (maybe should be CIDR blocks?), or "Group" which was a set
-- of users.

type User = String

type Port = Word16 -- Transport port (UDP,TCP)

type Host = IPAddress

data Flow = Flow (Maybe User) (Maybe User) 
                 (Maybe Port) (Maybe Port) 
                 (Maybe Host) (Maybe Host)
  deriving (Eq, Ord, Show)

data FlowGroup 
  = FlowMatch (Maybe OF.SwitchID) Match
  | Empty
  deriving (Ord, Eq, Show)

toMatch :: FlowGroup -> Maybe Match
toMatch (FlowMatch _ m) = Just m
toMatch _               = Nothing

-- | Helper function for unit testing
toMatch' :: FlowGroup -> Match
toMatch' f = case toMatch f of
  Just m -> m
  Nothing -> error "Flows.toMatch invalid argument"

flowSwitchMatch :: FlowGroup -> Maybe (Maybe OF.SwitchID, OF.Match)
flowSwitchMatch (FlowMatch sw match) = Just (sw, match)
flowSwitchMatch _                    = Nothing

fromSwitchMatch :: OF.SwitchID -> OF.Match -> FlowGroup
fromSwitchMatch sid m = FlowMatch (Just sid) m

fromMatch :: OF.Match -> FlowGroup
fromMatch m = FlowMatch Nothing m

-- TODO(adf): UNUSED -- For future, generalized version of flow groups
isSubFlow' :: Flow -> Flow -> Bool
isSubFlow' (Flow su du sp dp sh dh) (Flow su' du' sp' dp' sh' dh') =
  let (⊆) s1 s2 = case (s1, s2) of
        (_, Nothing)      -> True -- Remember: "Nothing" represents wildcard
        (Just x, Just y)  -> x == y
        (Nothing, Just _) -> False
    in su ⊆ su' && du ⊆ du' && sp ⊆ sp' && dp ⊆ dp' && sh ⊆ sh' && dh ⊆ dh'

all = FlowMatch Nothing matchAny

-- | Helper function for easy unit testing
simple :: Maybe Host -> Maybe Port -> Maybe Host -> Maybe Port -> FlowGroup
simple sh sp dh dp = 
  let mHost h = case h of
                  Nothing -> (IPAddress 0, 0)
                  Just ip -> (ip, maxPrefixLen)
    in FlowMatch Nothing (matchAny { srcIPAddress = mHost sh,
                                     dstIPAddress = mHost dh,
                                     srcTransportPort = sp, 
                                     dstTransportPort = dp })

-- TODO(adf): UNUSED -- For future, generalized version which we don't know
-- how to actually use/create yet....
make :: Set User -> Set User -> Set Port -> Set Port -> Set Host -> Set Host
     -> FlowGroup
make su du sp dp sh dh = 
  let port set = case Set.toList set of
        Just [x]  -> Just (Just x)
        Nothing   -> Just Nothing
        otherwise -> Nothing
      ip set = case Set.toList set of
        Just [x]  -> Just (x, maxPrefixLen)
        Nothing   -> Just (IPAddress 0,0)
        otherwise -> Nothing
      -- TODO(arjun): srcUser and dstUser are ignored
      makeMatch _ _ sp dp sh dh = do
       sp' <- port sp
       dp' <- port dp
       sh' <- ip dh
       dh' <- ip dh
       return $ matchAny { srcIPAddress = sh',  dstIPAddress = dh',
                           srcTransportPort = sp', dstTransportPort = dp' }
    in case makeMatch su du sp dp sh dh of
         Just m -> FlowMatch Nothing m
         Nothing -> error "flow group unsupported"

intersection :: FlowGroup -> FlowGroup -> FlowGroup
intersection _              Empty          = Empty
intersection Empty          _              = Empty
intersection (FlowMatch sw1 m1) (FlowMatch sw2 m2) = 
  let sw' = case (sw1, sw2) of
              (Just s, Just s') -> if s == s' then Just (Just s) else Nothing
              (Just s, Nothing) -> Just (Just s)
              (Nothing, Just s') -> Just (Just s')
              (Nothing, Nothing) -> Just Nothing
      m' = Match.intersect m1 m2
    in case (sw', m') of
         (Just s, Just m) -> FlowMatch s m
         otherwise -> Empty

null :: FlowGroup -> Bool
null Empty = True
null _     = False

isOverlapped :: FlowGroup -> FlowGroup -> Bool
isOverlapped f1 f2 = not (null (intersection f1 f2))

isSubFlow :: FlowGroup -> FlowGroup -> Bool
isSubFlow Empty          _              = True
isSubFlow _              Empty          = False
isSubFlow (FlowMatch sw1 m1) (FlowMatch sw2 m2) = 
  let swContained = case (sw1, sw2) of
        (_, Nothing) -> True -- Remember: Nothing is the wildcard here
        (Just s, Just s') -> s == s'
        (Nothing, Just _) -> False
    in swContained && Match.subset m1 m2
