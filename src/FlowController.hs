module FlowController where

import Set (Set)
import qualified Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Tree
import Tree (Tree)
import qualified PriorityQueue as PQ
import PriorityQueue (PQ)
import Data.Maybe (maybe)

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

data Resv = Resv {
  resvShare :: ShareRef,
  resvFlows :: FlowGroup,
  resvStart :: Integer, -- invariant: start < end
  resvEnd :: Limit,
  resvSize :: Integer
} deriving (Show, Ord, Eq)

resvStartOrder :: Resv -> Resv -> Bool
resvStartOrder resv1 resv2 = resvStart resv1 <= resvStart resv2

resvEndOrder :: Resv -> Resv -> Bool
resvEndOrder resv1 resv2 = resvEnd resv1 <= resvEnd resv2

data Share = Share {
  shareResvLimit :: Limit,
  shareResv :: PQ Resv,
  shareFlows :: FlowGroup,
  shareHolders :: Set Speaker
--  shareStart :: Integer, -- invariant: start < end
--  shareEnd :: Limit,  TODO (?)
} deriving (Show)

type ShareTree = Tree ShareRef Share


data State = State {
  shareTree :: ShareTree,
  stateSpeakers :: Set String,
  acceptedResvs :: PQ Resv,
  activeResvs :: PQ Resv,
  stateNow :: Integer
} deriving Show

anyFlow = FlowGroup Set.all Set.all Set.all Set.all

rootSpeaker :: String
rootSpeaker = "root"

rootShareRef :: ShareRef
rootShareRef = "rootShare"

emptyShareResv = PQ.empty resvStartOrder

emptyState = 
  State (Tree.root rootShareRef
                   (Share NoLimit emptyShareResv anyFlow
                   (Set.singleton rootSpeaker)))
        (Set.singleton rootSpeaker)
        (PQ.empty resvStartOrder)
        (PQ.empty resvEndOrder)
        0
        

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
          let newShare = Share shareLimit emptyShareResv shareFlows 
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

simulate :: PQ Resv -- ^ sorted by start time
         -> [(Limit, Integer)] -- ^time and height
simulate resvsByStart = simStep 0 resvsByStart (PQ.empty resvEndOrder) where 
  simStep size byStart byEnd 
    | PQ.isEmpty byStart && PQ.isEmpty byEnd = []
    | otherwise =
    let now = min (maybe NoLimit (injLimit.resvStart) (PQ.peek byStart))
                  (maybe NoLimit resvEnd (PQ.peek byEnd))
        (startingNow, byStart') = PQ.dequeueWhile (\r ->
                                         (injLimit.resvStart) r == now)
                                      byStart
        (endingNow, byEnd') = PQ.dequeueWhile (\r -> resvEnd r == now)
                                    byEnd
        byEnd'' = foldr PQ.enqueue byEnd' startingNow
        size' = size + sum (map resvSize startingNow)
                     - sum (map resvSize endingNow)
        in (now, size'):(simStep size' byStart' byEnd'')

-- TODO: Make more general so it can be used in three functions:
-- 1) IsAvailable  2)  HoldIfAvailable  3) ReserveIfAvailable (existing use)
reserve :: Speaker
        -> Resv
        -> State
        -> Maybe State
reserve spk resv@(Resv shareRef flow start end size) 
        st@(State {shareTree = sT,
                   acceptedResvs = accepted }) =
  if Tree.member shareRef sT then
    let share = Tree.lookup shareRef sT
      in case Set.member spk (shareHolders share) && 
               flow `isSubFlow` (shareFlows share) &&
               start < (stateNow st) &&
               (DiscreteLimit start) < end of
        False -> Nothing
        True ->
          let chain = Tree.chain shareRef rootShareRef sT
              f Nothing _ = Nothing
              f (Just sT) (thisShareName,thisShare@(Share {shareResv=resvs})) = 
                let g (Resv { resvStart = start', resvEnd = end' }) =
                         not (end' < (DiscreteLimit start) ||
                              (DiscreteLimit start') > end)
                    simResvs = PQ.enqueue resv (PQ.filter g resvs)
                    (_, sizes) = unzip (simulate simResvs)
                    in if injLimit (maximum sizes) > shareResvLimit thisShare
                       then
                          Nothing
                       else
                          let thisShare' = thisShare { shareResv = PQ.enqueue
                                                          resv resvs }
                            in Just (Tree.update thisShareName thisShare' sT)
            in case foldl f (Just sT) chain of
                 Nothing -> Nothing
                 Just sT' -> 
                   Just (tick 0 (st { shareTree = sT',
                              acceptedResvs = PQ.enqueue resv accepted }))
  else
    Nothing

tick :: Integer -> State -> State
tick t st@(State {acceptedResvs=byStart, activeResvs=byEnd, stateNow=now}) =
  st { acceptedResvs = byStart', activeResvs = byEnd'', stateNow = now' }
  where now' = now + t
        (startingNow, byStart') = PQ.dequeueWhile (\r -> resvStart r <= now')
                                       byStart
        (endingNow, byEnd') = PQ.dequeueWhile (\r -> resvEnd r
                                       <= (injLimit now'))
                                       byEnd
-- TODO: We should delete the endingNow reservations from the shareTree (optimization)
-- TODO: After we can delete reservations, make it possible to delete shares
        byEnd'' = foldr PQ.enqueue byEnd' startingNow

currentReservations = PQ.toList.activeResvs


findSharesByFlowGroup :: FlowGroup -> State -> [(ShareRef, Share)]
findSharesByFlowGroup fg st@(State {shareTree=sT}) =
  findInTree fg (rootShareRef, (Tree.lookup rootShareRef sT)) sT where
    findInTree flow (shareRef, share) tr = 
      let next = (foldl (++) [] (map (\x -> findInTree flow x tr)
                                  (Tree.children shareRef tr)))
        in case fg `isSubFlow` (shareFlows share) of
          False -> next
          True -> (shareRef, share):next
