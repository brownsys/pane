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
} deriving (Eq, Ord)

data Limit = NoLimit | DiscreteLimit Integer deriving (Eq)

instance Ord Limit where
  _ <= NoLimit = True
  (DiscreteLimit m) <= (DiscreteLimit n) = m <= n
  NoLimit <= (DiscreteLimit _) = False

data AcctRef = AcctRef {
  acctRefSpeakers :: Set Speaker,
  acctRefFlows :: FlowGroup,
  acctRefReservation :: Limit,
  acctRefAccount :: String
} deriving (Eq, Ord)


data ResourceAccount = ResourceAccount {
  resAcctReservationLimit :: Limit,
  resAcctReservation :: Limit
} deriving (Eq, Ord)

type AccountTree = Tree String ResourceAccount

data State = State {
  accountTree :: AccountTree,
  references :: Map Speaker (Set AcctRef)
}

anyFlow = FlowGroup Set.all Set.all Set.all Set.all

rootAcct :: String
rootAcct = "root account"

rootAcctRef :: AcctRef
rootAcctRef = AcctRef Set.all anyFlow NoLimit rootAcct

emptyState = 
  State (Tree.root rootAcct (ResourceAccount NoLimit (DiscreteLimit 0)))
        (Map.singleton "root" (Set.singleton rootAcctRef))

isSubRef :: AccountTree -> AcctRef -> AcctRef -> Bool
isSubRef aT (AcctRef speakers1 flow1 lim1 ref1) (AcctRef speakers2 flow2 lim2 ref2) =
  lim1 <= lim2 &&
  isSubFlow flow1 flow2 &&
  speakers1 `Set.isSubsetOf` speakers2 &&
  Tree.lessThanOrEq ref1 ref2 aT

isSubFlow :: FlowGroup -> FlowGroup -> Bool
isSubFlow (FlowGroup fs1 fr1 fsp1 fdp1) (FlowGroup fs2 fr2 fsp2 fdp2) =
  Set.isSubsetOf fs1 fs2 &&
  Set.isSubsetOf fr1 fr2 &&
  Set.isSubsetOf fsp1 fsp2 &&
  Set.isSubsetOf fdp1 fdp2

createSpeaker :: Speaker -- ^name of new speaker
              -> State -- ^existing state
              -> Maybe State
createSpeaker newSpeaker (State aT refs) =
  if Map.member newSpeaker refs then
    Nothing
  else
    let refs' = Map.insert newSpeaker Set.empty refs in
      Just (State aT refs')

giveReference :: Speaker -- ^grantor
              -> AcctRef -- ^reference to account
              -> Speaker -- ^acceptor
              -> State -- ^existing state
              -> Maybe State
giveReference from ref to (State aT refs) = 
  if not (Map.member from refs) || not (Map.member to refs) then
    Nothing
  else
    case Map.lookup from refs of
      Nothing -> error "Assplosion"
      Just fromRefs -> 
        if Set.exists (isSubRef aT ref) fromRefs then
          let refs' = Map.adjust (\ toSet -> Set.insert ref toSet) to refs in
            Just (State aT refs')
        else
          Nothing

{-
newResAcct :: Speaker
           -> AcctRef
           -> String
           -> Limit
           -> State
           -> Maybe State
newResAcct spk acctRef acctName acctLimit (State aT refs) =
  case Map.lookup spk refs of
    Nothing -> Nothing
    Just acctRefs -> case Set.exists (isSubRef aT acctRef acctRefs) of
      False -> Nothing
      True -> 
        let newAcct = ResourceAcct acctName acctLimit (DiscreteLimit 0) in
          Just (State (Tree.insert newAcct (acctRefAccount acctRef) aT) refs)
-}
 
