module FlowController where

import Set (Set)
import qualified Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Tree
import Tree (Tree)
import qualified PriorityQueue as PQ
import PriorityQueue (PQ)
import Data.Maybe (maybe, mapMaybe)

type Speaker = String

type User = String
type Port = Integer

data FlowGroup = FlowGroup {
  flowSend :: Set User,
  flowRecv :: Set User,
  flowSrcPort ::  Set Port,
  flowDestPort :: Set Port
} deriving (Ord, Eq, Show)

data Limit = NoLimit | DiscreteLimit Integer deriving (Eq, Show)

instance Ord Limit where
  _ <= NoLimit = True
  (DiscreteLimit m) <= (DiscreteLimit n) = m <= n
  NoLimit <= (DiscreteLimit _) = False


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

data Share = Share {
  shareResvLimit :: Limit,
  shareReq :: PQ Req,
  shareFlows :: FlowGroup,
  shareHolders :: Set Speaker
--  shareStart :: Integer, -- invariant: start < end
--  shareEnd :: Limit,  TODO (?)
} deriving (Show)

type ShareTree = Tree ShareRef Share


data State = State {
  shareTree :: ShareTree,
  stateSpeakers :: Set String,
  acceptedReqs :: PQ Req,
  activeReqs :: PQ Req,
  stateNow :: Integer
} deriving Show

-----------------------------
-- Useful defined variables
-----------------------------

anyFlow = FlowGroup Set.all Set.all Set.all Set.all

rootSpeaker :: String
rootSpeaker = "root"

rootShareRef :: ShareRef
rootShareRef = "rootShare"

emptyShareReq = PQ.empty reqStartOrder

emptyState = 
  State (Tree.root rootShareRef
                   (Share NoLimit emptyShareReq anyFlow
                   (Set.singleton rootSpeaker)))
        (Set.singleton rootSpeaker)
        (PQ.empty reqStartOrder)
        (PQ.empty reqEndOrder)
        0
        

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

unReqResv :: ReqData -> Maybe Integer
unReqResv rd =
  case rd of
    (ReqResv n) -> (Just n)
    otherwise -> Nothing

isSubFlow :: FlowGroup -> FlowGroup -> Bool
isSubFlow (FlowGroup fs1 fr1 fsp1 fdp1) (FlowGroup fs2 fr2 fsp2 fdp2) =
  Set.isSubsetOf fs1 fs2 &&
  Set.isSubsetOf fr1 fr2 &&
  Set.isSubsetOf fsp1 fsp2 &&
  Set.isSubsetOf fdp1 fdp2

isSubShare :: Share -> Share -> Bool
isSubShare (Share resLim1 _ flows1 _)
          (Share resLim2 _ flows2 _) = 
  resLim1 <= resLim2 &&
  flows1 `isSubFlow` flows2

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
           -> String
           -> FlowGroup
           -> Limit
           -> State
           -> Maybe State
newShare spk parentName shareName shareFlows shareLimit
           st@(State {shareTree = sT}) =
  if Tree.member parentName sT then
    let parentShare = Tree.lookup parentName sT
      in case Set.member spk (shareHolders parentShare) of
        True -> 
          let newShare = Share shareLimit emptyShareReq shareFlows 
                                        (Set.singleton spk)
            in case newShare `isSubShare` parentShare of
                 True -> 
                   Just (st { shareTree =
                            (Tree.insert shareName newShare parentName sT) } )
                 False -> Nothing
        False -> Nothing
  else
    Nothing

injLimit n = DiscreteLimit n

simulate :: PQ Req -- ^ sorted by start time
         -> [(Limit, Integer)] -- ^time and height
simulate reqsByStart = simStep 0 reqsByStart (PQ.empty reqEndOrder) where 
  simStep size byStart byEnd 
    | PQ.isEmpty byStart && PQ.isEmpty byEnd = []
    | otherwise =
    let now = min (maybe NoLimit (injLimit.reqStart) (PQ.peek byStart))
                  (maybe NoLimit reqEnd (PQ.peek byEnd))
        (startingNow, byStart') = PQ.dequeueWhile (\r ->
                                         (injLimit.reqStart) r == now)
                                      byStart
        (endingNow, byEnd') = PQ.dequeueWhile (\r -> reqEnd r == now)
                                    byEnd
        byEnd'' = foldr PQ.enqueue byEnd' startingNow
        size' = size + sum (mapMaybe (unReqResv.reqData) startingNow)
                     - sum (mapMaybe (unReqResv.reqData) endingNow)
        in (now, size'):(simStep size' byStart' byEnd'')

-- TODO: Make more general so it can be used in three functions:
-- 1) IsAvailable  2)  HoldIfAvailable  3) ReserveIfAvailable (existing use)
reserve :: Speaker
        -> Req
        -> State
        -> Maybe State
reserve spk req@(Req shareRef flow start end rD)
        st@(State {shareTree = sT,
                   acceptedReqs = accepted }) =
  if Tree.member shareRef sT then
    let share = Tree.lookup shareRef sT
      in case Set.member spk (shareHolders share) && 
               flow `isSubFlow` (shareFlows share) &&
               start >= (stateNow st) &&
               (DiscreteLimit start) < end of
        False -> Nothing
        True ->
          let chain = Tree.chain shareRef sT
              f Nothing _ = Nothing
              f (Just sT) (thisShareName,thisShare@(Share {shareReq=reqs})) = 
                let g (Req { reqStart = start', reqEnd = end' }) =
                         not (end' < (DiscreteLimit start) ||
                              (DiscreteLimit start') > end)
                    simReqs = PQ.enqueue req (PQ.filter g reqs)
                    (_, sizes) = unzip (simulate simReqs)
-- TODO: this next line is the specific test for reservations, rather than
-- any type of request
                    in if injLimit (maximum sizes) > shareResvLimit thisShare
                       then
                          Nothing
                       else
                          let thisShare' = thisShare { shareReq = PQ.enqueue
                                                          req reqs }
                            in Just (Tree.update thisShareName thisShare' sT)
            in case foldl f (Just sT) chain of
                 Nothing -> Nothing
                 Just sT' -> 
                   Just (tick 0 (st { shareTree = sT',
                              acceptedReqs = PQ.enqueue req accepted }))
  else
    Nothing

tick :: Integer -> State -> State
tick t st@(State {acceptedReqs=byStart, activeReqs=byEnd, stateNow=now}) =
  st { acceptedReqs = byStart', activeReqs = byEnd'', stateNow = now' }
  where now' = now + t
        (startingNow, byStart') = PQ.dequeueWhile (\r -> reqStart r <= now')
                                       byStart
        (endingNow, byEnd') = PQ.dequeueWhile (\r -> reqEnd r
                                       <= (injLimit now'))
                                       byEnd
-- TODO: We should delete the endingNow reservations from the shareTree (optimization)
-- TODO: After we can delete reservations, make it possible to delete shares
        byEnd'' = foldr PQ.enqueue byEnd' startingNow

currentRequests = PQ.toList.activeReqs

-----------------------------
-- Query Functions
-----------------------------

findSharesByFlowGroup :: FlowGroup -> State -> [(ShareRef, Share)]
findSharesByFlowGroup fg st@(State {shareTree=sT}) =
  findInTree fg (rootShareRef, (Tree.lookup rootShareRef sT)) sT where
    findInTree flow (shareRef, share) tr = 
      let next = (foldl (++) [] (map (\x -> findInTree flow x tr)
                                  (Tree.children shareRef tr)))
        in case fg `isSubFlow` (shareFlows share) of
          False -> [] -- skip the children
          True -> (shareRef, share):next

findSharesByUser :: User -> State -> [(ShareRef, Share)]
findSharesByUser user st@(State {shareTree=sT}) =
  findInTree user (rootShareRef, (Tree.lookup rootShareRef sT)) sT where
    findInTree holder (shareRef, share) tr = 
      let next = (foldl (++) [] (map (\x -> findInTree holder x tr)
                                  (Tree.children shareRef tr)))
        in case Set.member holder (shareHolders share) of
          False -> next -- we want recursive descent when checking for users
          True -> (shareRef, share):next

findReqByFlowGroup :: FlowGroup -> State -> [Req]
findReqByFlowGroup fg st@(State {acceptedReqs=accepted,activeReqs=active}) =
  PQ.toList (PQ.filter (\x -> fg `isSubFlow` (reqFlows x)) active) ++
    PQ.toList (PQ.filter (\x -> fg `isSubFlow` (reqFlows x)) accepted)
