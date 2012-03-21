-- |Implementation of the gaurantee semantics and forwarding semantics of 
-- 'ShareTree's.
module ShareSemantics 
  ( evalShareTree
  , Action (..)
  , Admit (..)
  , MatchTable (..)
  , compileShareTree
  , evalTable
  ) where

import Data.Tree (Tree (..))
import FlowController (Share (..))
import qualified PriorityQueue as PQ
import Base
import Data.List (groupBy, find)
import qualified Flows as Flows

data Admit = Allow | Deny deriving (Show, Eq)

data Action = Action {
  gmb   :: Maybe Integer,
  admit :: Maybe Admit
} deriving (Show, Eq)

emptyAction = Action Nothing Nothing

activeAt now req = reqStart req <= now && fromInteger now <= reqEnd req

-- TODO(arjun): I believe its okay to skip strict. Explain why in a comment.
reqToAction (Req _ _ _ _  (ReqResv n) strict) = Action (Just n) Nothing
reqToAction (Req _ _ _ _  ReqAllow strict) = Action Nothing (Just Allow)
reqToAction (Req _ _ _ _  ReqDeny strict) = Action Nothing (Just Deny)

combineSiblingActions (Action gmb1 admit1) (Action gmb2 admit2) =
  Action (combineMaybe max gmb1 gmb2) 
         (combineMaybe denyOverride admit1 admit2)
    where denyOverride Allow Allow = Allow
          denyOverride _     _     = Deny

combineParentChildActions (Action gmbParent admitParent)
                          (Action gmbChild admitChild) =
  Action (combineMaybe max gmbParent gmbChild) 
         (combineMaybe (\_ ch -> ch) admitParent admitChild)

evalShare :: Integer -- ^current time
          -> Share
          -> Flow
          -> Action
evalShare now share flowIn = foldl combineSiblingActions emptyAction actions
  where matchedReqs = filter (\r -> flowIn `flowInGroup` reqFlows r) $
                        filter (activeAt now) (PQ.toList (shareReq share))
        actions = map reqToAction matchedReqs

evalShareTree :: Integer    -- ^current time
              -> Tree Share -- ^share tree
              -> Flow       -- ^input packet's flow
              -> Action     -- ^output action
evalShareTree now (Node share children) inFlow = action
  where thisAction     = evalShare now share inFlow
        childActions   = map (\tree -> evalShareTree now tree inFlow) children 
        cmbChildAction = foldl combineSiblingActions emptyAction childActions
        action         = combineParentChildActions thisAction cmbChildAction

--
--
--

data MatchTable = MatchTable [(Flows.FlowGroup, Action)] deriving (Show, Eq)

emptyTable :: MatchTable
emptyTable = MatchTable []

evalTable :: MatchTable -> Flow -> Action
evalTable (MatchTable lst) flow = 
  -- relies on 'find' scanning left to right
  case find (\(fg, _) -> Flows.flowInGroup flow fg) lst of
    Nothing -> emptyAction
    Just (_, action) -> action


intersectTable :: (Action -> Action -> Action) 
               -> MatchTable 
               -> MatchTable
               -> MatchTable
intersectTable cmb (MatchTable tbl1) (MatchTable tbl2) = MatchTable tbl'
  where tbl' = [ (f1 ∩ f2, cmb a1 a2) | (f1, a1) <- tbl1,
                                        (f2, a2) <- tbl2,
                                        not (Flows.null (f1 ∩ f2)) ]
        (∩) = Flows.intersection

unionTable :: (Action -> Action -> Action)
           -> MatchTable
           -> MatchTable
           -> MatchTable
unionTable cmb mt1@(MatchTable tbl1) mt2@(MatchTable tbl2) = 
  MatchTable (tbl' ++ tbl1 ++ tbl2)
    where (MatchTable tbl') = intersectTable cmb mt1 mt2

shareToTable :: Integer
             -> Share
             -> MatchTable
shareToTable now share = 
  foldl (unionTable combineSiblingActions) emptyTable (map reqToTbl reqs)
    where reqs = filter (activeAt now) (PQ.toList (shareReq share))
          reqToTbl req = MatchTable [(reqFlows req, reqToAction req)]
--                                     (Flows.all, emptyAction)]

shareTreeToTable :: Integer    -- ^current time
                 -> Tree Share -- ^share tree
                 -> MatchTable
shareTreeToTable now (Node share children) = tbl
  where thisTbl     = shareToTable now share
        childTbls   = map (shareTreeToTable now) children
        cmbChildTbl = 
          foldl (unionTable combineSiblingActions) emptyTable childTbls
        tbl         = unionTable combineParentChildActions thisTbl cmbChildTbl

condense :: MatchTable -> MatchTable
condense (MatchTable tbl) = MatchTable (loop [] tbl)
  where loop _           [] = []
        loop lhs ((f, action):rest) = 
          case any (\leftF -> f `Flows.isSubFlow` leftF) lhs of
            True  -> loop lhs rest
            False -> (f,action):(loop (f:lhs) rest)

compileShareTree :: Integer    -- ^current time
                 -> Tree Share -- ^share tree
                 -> MatchTable
compileShareTree now tree = condense (shareTreeToTable now tree)