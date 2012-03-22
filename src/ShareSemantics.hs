-- |Implementation of the gaurantee semantics and forwarding semantics of 
-- 'ShareTree's.
module ShareSemantics 
  ( evalShareTree
  , Action
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

type Action = Maybe (ReqData, Limit)

data MatchTable = MatchTable [(Flows.FlowGroup, Action)] deriving (Show, Eq)

emptyAction = Nothing

activeAt now req = reqStart req <= now && fromInteger now <= reqEnd req

-- TODO(arjun): I believe its okay to skip strict. Explain why in a comment.
reqToAction (Req _ _ _ end req strict) = Just (req, end)

combineMaybe :: (a -> a -> a) 
             -> Maybe (a, Limit) 
             -> Maybe (a, Limit) 
             -> Maybe (a, Limit)
combineMaybe _ Nothing       Nothing       = Nothing
combineMaybe _ (Just (a, t)) Nothing       = Just (a, t)
combineMaybe _ Nothing       (Just (b, t)) = Just (b, t)
combineMaybe f (Just (a, s)) (Just (b, t)) = Just (f a b, min s t)


combineSiblingActions act1 act2 = combineMaybe f act1 act2
  where f ReqDeny     _           = ReqDeny
        f _           ReqDeny     = ReqDeny
        f (ReqResv m) (ReqResv n) = ReqResv (max m n)
        f (ReqResv m) ReqAllow    = ReqResv m
        f ReqAllow    (ReqResv n) = ReqResv n
        f ReqAllow    ReqAllow    = ReqAllow

combineParentChildActions act1 act2 = combineMaybe f act1 act2
  where -- f parent child = child overrides parent, with the exceptions below
        f (ReqResv m) (ReqResv n) = ReqResv (max m n) -- pick max, doesn't hurt
        f (ReqResv m) ReqAllow    = ReqResv m  -- give guarantee, doesn't hurt
        f _           ch          = ch

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
