module FlowController
  ( State
  , ShareTree
  , emptyState
  , emptyStateWithTime
  , emptyShareReq
  , createSpeaker
  , giveReference
  , giveDefaultReference
  , newShare
  , request
  , isAdmControl
  , reqDepth
  , stateNow
  , shareTree
  , listShareRefsByFlowGroup
  , listShareRefsByUser
  , Share (..)
  , getSchedule
  , rootShareRef
  , rootSpeaker
  , getShareTree
  ) where

import Control.Monad
import Debug.Trace
import Set (Set)
import qualified Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Tree
import Tree (Tree)
import qualified Data.Tree
import Data.Maybe (maybe, mapMaybe, fromMaybe, catMaybes)
import Base
import qualified TokenGraph as TG
import TokenGraph (TokenGraph)
import qualified Flows as Flows
import qualified Data.List as List

data Share = Share {
  shareName :: ShareRef,       -- ^must match name in the 'ShareTree'
  shareFlows :: FlowGroup,     -- set of flows in this share
  shareHolders :: Set Speaker, -- users who can speak about this share
  shareReq :: [Req],          -- queue of accepted requests, sort by end
  -- Restrictions on what this share can be used for
  shareCanAllowFlows :: Bool,
  shareCanDenyFlows :: Bool,
  shareResv :: TokenGraph
} deriving (Show)

type ShareTree = Tree ShareRef Share

data State = State {
  shareTree :: ShareTree,
  stateSpeakers :: Set String,
  stateNow :: Integer
} deriving Show

-----------------------------
-- Useful defined variables
-----------------------------

rootSpeaker :: String
rootSpeaker = "root"

rootShareRef :: ShareRef
rootShareRef = "rootShare"

emptyShareReq = []

emptyStateWithTime t = 
  State (Tree.root 
           rootShareRef
           (Share rootShareRef Flows.all (Set.singleton rootSpeaker)
                  emptyShareReq True True TG.unconstrained))
        (Set.singleton rootSpeaker)
        t

emptyState = emptyStateWithTime 0
        
-----------------------------
-- Helper Functions
-----------------------------

reqStartOrder :: Req -> Req -> Bool
reqStartOrder req1 req2 = reqStart req1 <= reqStart req2

reqEndOrder :: Req -> Req -> Bool
reqEndOrder req1 req2 = reqEnd req1 <= reqEnd req2

isResv :: Req -> Bool
isResv req =
  case (reqData req) of
    (ReqResv _) -> True
    otherwise -> False

isAllow :: Req -> Bool
isAllow req =
  case (reqData req) of
    ReqAllow -> True
    otherwise -> False

isDeny :: Req -> Bool
isDeny req =
  case (reqData req) of
    ReqDeny -> True
    otherwise -> False

isAdmControl :: Req -> Bool
isAdmControl req = (isAllow req) || (isDeny req)

reqDepth :: Req -> State -> Int
reqDepth req state =
  Tree.depth (reqShare req) (shareTree state)

unReqResv :: ReqData -> Maybe Integer
unReqResv rd =
  case rd of
    (ReqResv n) -> (Just n)
    otherwise -> Nothing


isSubShare :: Share -> Share -> Bool
isSubShare (Share _ flows1 _ _ canAllow1 canDeny1 tg1)
          (Share _ flows2 _ _ canAllow2 canDeny2 tg2) = 
  flows1 `Flows.isSubFlow` flows2 &&
  canAllow1 <= canAllow2 &&
  canDeny1 <= canDeny2 &&
  tg1 `TG.isConstraintsContained` tg2

isSubShareWRefs :: ShareRef -> ShareRef -> ShareTree -> Bool
isSubShareWRefs sr1 sr2 sT =
  let s1 = Tree.lookup sr1 sT
      s2 = Tree.lookup sr2 sT in
    isSubShare s1 s2

getShareTree :: State -> Data.Tree.Tree Share
getShareTree st = Tree.expose (shareTree st)

-----------------------------
-- API Functions
-----------------------------

createSpeaker :: Speaker -- ^name of new speaker
              -> State -- ^existing state
              -> Maybe State
createSpeaker newSpeaker st@(State {stateSpeakers=spk})  =
  if Set.exists (==newSpeaker) spk then
    Nothing
  else
    Just (st { stateSpeakers = Set.insert newSpeaker spk })

giveReference :: Speaker -- ^grantor
              -> ShareRef -- ^reference to share
              -> Speaker -- ^acceptor
              -> State -- ^existing state
              -> Maybe State
giveReference from ref to st@(State {shareTree=sT, stateSpeakers=refs}) = 
  if not (Set.member from refs) || not (Set.member to refs) ||
     not (Tree.member ref sT) then
    Nothing
  else
    let share = Tree.lookup ref sT
      in case from `Set.member` shareHolders share of
        True -> 
          let share' = share {shareHolders = Set.insert to (shareHolders share)}
              sT' = Tree.update ref share' sT
            in Just (st {shareTree = sT'})
        False -> Nothing

giveDefaultReference :: Speaker -- ^grantor
                     -> ShareRef -- ^reference to share
                     -> State -- ^existing state
                     -> Maybe State
giveDefaultReference from ref st@(State {shareTree=sT, stateSpeakers=refs}) = 
  if not (Set.member from refs) then
    Nothing
  else
    let share = Tree.lookup ref sT 
      in case from `Set.member` shareHolders share of
        True -> 
          let share' = share {shareHolders = Set.all}
              sT' = Tree.update ref share' sT
            in Just (st {shareTree = sT'})
        False -> Nothing

newShare :: Speaker
           -> ShareRef
           -> Share
           -> State
           -> Maybe State
newShare spk parentName newShare@(Share { shareName = shareName })
           st@(State {shareTree = sT}) =
  if Tree.member parentName sT then
    let parentShare = Tree.lookup parentName sT
      in case Set.member spk (shareHolders parentShare) of
        True -> case newShare `isSubShare` parentShare of
                  True -> 
                    Just (st { shareTree =
                             (Tree.insert shareName newShare parentName sT) } )
                  False -> Nothing
        False -> Nothing
  else
    Nothing

-- For resources which need to be checked up the tree
-- TODO(arjun): A single reservation in a share, is inserted in all shares on
-- the chain to the root. We then repurpose the share-tree as an HFT, so it
-- looks like we have several reservations to the compiler. But, this
-- accidentally works because of the particular +P function we use.
recursiveRequest :: Req
                 -> State
                 -> Maybe State
recursiveRequest req@(Req shareRef _ start end rData _)
                 st@(State {shareTree = sT }) =
  let chain = Tree.chain shareRef sT
      f Nothing _ = Nothing
      f (Just sT) (thisShareName, thisShare@(Share {shareReq=reqs,
                                                    shareResv=tg})) =
        case rData of
           (ReqResv drain) -> do tg <- TG.drain start end drain tg
                                 let thisShare' = thisShare {
                                            shareReq = req:reqs,
                                            shareResv = tg }
                                 return (Tree.update thisShareName thisShare' sT)
           ReqAllow -> Nothing
           ReqDeny -> Nothing  
    in case foldl f (Just sT) chain of
         Nothing -> Nothing
         Just sT' -> 
           Just (st { shareTree = sT' })

-- For resources which only need to be checked in their share (not up the tree)
localRequest :: Req
             -> State
             -> Maybe State
localRequest req@(Req shareRef _ _ _ _ _)
             st@(State {shareTree = sT }) =
  let share = Tree.lookup shareRef sT in
    if (isAllow req && shareCanAllowFlows share) ||
       (isDeny req && shareCanDenyFlows share)
      then
        let share' = share { shareReq = req:(shareReq share) }
            sT' = Tree.update shareRef share' sT
          in Just (st { shareTree = sT' })
      else
        Nothing

-- For strict application of allow/deny: 1) check if any existing admission 
-- control statements in non-parent shares apply 2) if none do, then it is
-- ok to proceed to local request
strictAdmControl :: Req
                 -> State
                 -> Maybe State
strictAdmControl req@(Req shareRef flow _ _ _ _)
                 st@(State {shareTree = sT }) =
  let isNonParent (Req sR' _ _ _ _ _) =
        -- the only shares that should return false are parents of shareRef
        not (isSubShareWRefs shareRef sR' sT) -- || shareRef == sR'
  in case filter (\x -> isNonParent x && isAdmControl x && 
                        -- only care if allow blocked by deny, and vice versa
                        ((reqData req) /= (reqData x)))
                 (listReqByIntersectingFG flow st) of
    [] -> localRequest req st
    _ -> Nothing -- TODO: Return why strict failed


-- TODO: Make more general so it can be used in three functions:
-- 1) IsAvailable  2)  HoldIfAvailable  3) RequestIfAvailable (existing use)
request :: Speaker
        -> Req
        -> State
        -> Maybe State
request spk req@(Req shareRef flow start end rD strict)
        st@(State {shareTree = sT }) =
  if Tree.member shareRef sT then
    let share = Tree.lookup shareRef sT
      in case Set.member spk (shareHolders share) && 
               flow `Flows.isSubFlow` (shareFlows share) &&
               start >= (stateNow st) &&
               (DiscreteLimit start) < end of
        False -> Nothing
        True -> case strict of
                  True -> case rD of
                             -- TODO: need to send reason why if request rejected
                             (ReqResv _) -> recursiveRequest req st
                             ReqAllow -> strictAdmControl req st
                             ReqDeny -> strictAdmControl req st
                  False -> case rD of
                             -- TODO: need to send explanation of what happened
                             ReqAllow -> localRequest req st
                             ReqDeny -> localRequest req st
                             _ -> error "FAIL. PARTIAL RESERVE UNIMPLEMENTED."
  else
    Nothing

-- |'getSchedule speaker share state' returns the reservation schedule on 'share'
-- to 'speaker'. The reservation schedule is a list of 3-tuples,
-- '(timestamp, bandwidth-available, tokens-available)'. The list is ordered by
-- 'timestamp'.
--
-- The operation fails if 'speaker' does not hold a reference to 'share'.
getSchedule :: Speaker
              -> ShareRef
              -> State
              -> Maybe [(Limit, Limit, Limit)]
getSchedule speaker shareName (State { shareTree=shares, stateNow=now }) = do
  let share = Tree.lookup shareName shares
  when (not (Set.member speaker (shareHolders share)))
    (fail "does not hold share")
  return (TG.graph (shareResv share))

-----------------------------
-- Query Functions
-----------------------------

listShareRefsByFlowGroup :: FlowGroup -> State -> [ShareRef]
listShareRefsByFlowGroup fg (State {shareTree=sT}) =
  findInTree fg (rootShareRef, (Tree.lookup rootShareRef sT)) sT where
    findInTree flow (shareRef, share) tr = 
      let next = (foldl (++) [] (map (\x -> findInTree flow x tr)
                                  (Tree.children shareRef tr)))
        in case fg `Flows.isSubFlow` (shareFlows share) of
          False -> [] -- skip the children b/c tree constructed with isSubFlow
          True -> shareRef:next

listShareRefsByUser :: User -> State -> [ShareRef]
listShareRefsByUser user (State {shareTree=sT}) =
  findInTree user (rootShareRef, (Tree.lookup rootShareRef sT)) sT where
    findInTree holder (shareRef, share) tr = 
      let next = (foldl (++) [] (map (\x -> findInTree holder x tr)
                                  (Tree.children shareRef tr)))
        in case Set.member holder (shareHolders share) of
          False -> next -- we want recursive descent when checking for users
          True -> shareRef:next

filterRequests pred (Data.Tree.Node share children) =
  filter pred (shareReq share) ++ List.concatMap (filterRequests pred) children

listReqByFlowGroup :: FlowGroup -> State -> [Req]
listReqByFlowGroup fg (State {stateNow=now, shareTree=tree}) =
  filterRequests f (Tree.expose tree)
    where f req = fg `Flows.isSubFlow` reqFlows req &&
                  fromInteger now <= reqEnd req -- active and accepted

listReqWithSubFlow :: FlowGroup -> State -> [Req]
listReqWithSubFlow fg (State {stateNow=now, shareTree=tree}) =
  filterRequests f (Tree.expose tree)
    where f req = reqFlows req `Flows.isSubFlow` fg  &&
                  fromInteger now <= reqEnd req

listReqByIntersectingFG :: FlowGroup -> State -> [Req]
listReqByIntersectingFG fg (State {stateNow=now, shareTree=tree}) =
  filterRequests f (Tree.expose tree)
    where f req = reqFlows req `Flows.isOverlapped` fg  &&
                  fromInteger now <= reqEnd req
