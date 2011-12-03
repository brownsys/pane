module FlowController where

import Set (Set)
import qualified Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Tree
import Tree (Tree)

type Speaker = String

type User = String
type Port = Int

data FlowGroup = FlowGroup {
  flowSend :: Set User,
  flowRecv :: Set User,
  flowSrcPort ::  Set Port,
  flowDestPort :: Set Port
} deriving (Eq, Ord, Show)

data Limit = NoLimit | DiscreteLimit Integer deriving (Eq, Show)

instance Ord Limit where
  _ <= NoLimit = True
  (DiscreteLimit m) <= (DiscreteLimit n) = m <= n
  NoLimit <= (DiscreteLimit _) = False

type AcctRef = String

data ResourceAccount = ResourceAccount {
  shareResvLimit :: Limit,
  shareResv :: Integer,
  shareFlows :: FlowGroup,
  shareSpeakers :: Set Speaker,
  shareHolders :: Set Speaker
} deriving (Eq, Ord, Show)

type AccountTree = Tree AcctRef ResourceAccount

data State = State {
  accountTree :: AccountTree,
  stateSpeakers :: Set String
} deriving Show

anyFlow = FlowGroup Set.all Set.all Set.all Set.all

rootAcct :: String
rootAcct = "root"

rootAcctRef :: AcctRef
rootAcctRef = "root"

emptyState = 
  State (Tree.root rootAcctRef
                   (ResourceAccount NoLimit 0 anyFlow Set.all 
                                    (Set.singleton rootAcct)))
        (Set.singleton rootAcct)

isSubFlow :: FlowGroup -> FlowGroup -> Bool
isSubFlow (FlowGroup fs1 fr1 fsp1 fdp1) (FlowGroup fs2 fr2 fsp2 fdp2) =
  Set.isSubsetOf fs1 fs2 &&
  Set.isSubsetOf fr1 fr2 &&
  Set.isSubsetOf fsp1 fsp2 &&
  Set.isSubsetOf fdp1 fdp2

isSubAcct :: ResourceAccount -> ResourceAccount -> Bool
isSubAcct (ResourceAccount resLim1 _ flows1 spk1 _)
          (ResourceAccount resLim2 _ flows2 spk2 _) = 
  spk1 `Set.isSubsetOf` spk2 &&
  resLim1 <= resLim2 &&
  flows1 <= flows2

createSpeaker :: Speaker -- ^name of new speaker
              -> State -- ^existing state
              -> Maybe State
createSpeaker newSpeaker (State aT spk) =
  if Set.exists (==newSpeaker) spk then
    Nothing
  else
    Just (State aT (Set.insert newSpeaker spk))

giveReference :: Speaker -- ^grantor
              -> AcctRef -- ^reference to account
              -> Speaker -- ^acceptor
              -> State -- ^existing state
              -> Maybe State
giveReference from ref to (State aT refs) = 
  if not (Set.member from refs) || not (Set.member to refs) then
    Nothing
  else
    let share = Tree.lookup ref aT 
      in case from `Set.member` shareHolders share of
        True -> 
          let share' = share {shareHolders = Set.insert to (shareHolders share)}
              aT' = Tree.update ref share' aT
            in Just (State aT' refs)
        False -> Nothing

giveDefaultReference :: Speaker -- ^grantor
                     -> AcctRef -- ^reference to account
                     -> State -- ^existing state
                     -> Maybe State
giveDefaultReference from ref (State aT refs) = 
  if not (Set.member from refs) then
    Nothing
  else
    let share = Tree.lookup ref aT 
      in case from `Set.member` shareHolders share of
        True -> 
          let share' = share {shareHolders = Set.all}
              aT' = Tree.update ref share' aT
            in Just (State aT' refs)
        False -> Nothing

newResAcct :: Speaker
           -> AcctRef
           -> String
           -> Set Speaker
           -> FlowGroup
           -> Limit
           -> State
           -> Maybe State
newResAcct spk parentName acctName acctSpk acctFlows acctLimit (State aT refs) =
  if Tree.member parentName aT then
    let parentShare = Tree.lookup parentName aT
      in case Set.member spk (shareHolders parentShare) of
        True -> 
          let newAcct = ResourceAccount acctLimit 0 acctFlows acctSpk 
                                       (Set.singleton spk)
            in case newAcct `isSubAcct` parentShare of
                 True -> 
                   Just (State (Tree.insert acctName newAcct parentName aT) 
                               refs)
                 False -> Nothing
        False -> Nothing
  else
    Nothing

reserve :: Speaker
        -> AcctRef
        -> Integer
        -> State
        -> Maybe State
reserve spk acctRef resv (State aT refs) = 
  if Tree.member acctRef aT then
    let share = Tree.lookup acctRef aT
      in case Set.member spk (shareHolders share) of
        False -> Nothing
        True ->
          let chain = Tree.chain acctRef rootAcct aT
              f Nothing _ = Nothing
              f (Just aT) (acctName, acct) = 
                if DiscreteLimit (resv + shareResv acct) <= shareResvLimit acct then
                  Just (Tree.update acctName (acct { shareResv = resv + shareResv acct }) aT)
                else
                  Nothing
            in case foldl f (Just aT) chain of
                 Nothing -> Nothing
                 Just aT' -> Just (State aT' refs)
  else
    Nothing
