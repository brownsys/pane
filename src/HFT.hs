-- |Implementation of the gaurantee semantics and forwarding semantics of 
-- PolicyTrees
module HFT
  ( Action
  , MatchTable (..)
  , compileShareTree
  , emptyTable
  , unionTable
  , concatTable
  , condense
  ) where

import Data.Tree (Tree (..))
import ShareTree (Share (..))
import Base
import Data.List (groupBy, find)
import qualified Flows as Flows

type Action = Maybe (ReqData, Limit) -- ^ (Request, timeout in absolute time)

data MatchTable = MatchTable [(Flows.FlowGroup, Action)] deriving (Show, Eq)

emptyAction = Nothing

activeAt now req = reqStart req <= now && fromInteger now <= reqEnd req

-- Strictness is a static restriction on share trees. They are not part of
-- HFTs. The Req datatype was designed to be a share-tree element, but we're
-- overloading it here to be an HFT. (Same reasoning applies to speakers/
-- principals, but we didn't make that part of the request.)
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
  where f (ReqResv m)   (ReqResv n)   = ReqResv (max m n)
        f (ReqResv m)   ReqAllow      = ReqResv m
        f ReqAllow      (ReqResv n)   = ReqResv n
        f ReqAllow      ReqAllow      = ReqAllow
        f (ReqRlimit m) (ReqRlimit n) = ReqRlimit (min m n)
        f (ReqRlimit m) ReqAllow      = ReqRlimit m
        f ReqAllow      (ReqRlimit n) = ReqRlimit n
        f _             _             = ReqDeny

combineParentChildActions act1 act2 = combineMaybe f act1 act2
  where -- f parent child = child overrides parent, with the exceptions below
        f (ReqResv m)   (ReqResv n)   = ReqResv (max m n) -- pick max, doesn't hurt
        f (ReqResv m)   ReqAllow      = ReqResv m  -- give guarantee, doesn't hurt
        f (ReqRlimit m) (ReqRlimit n) = ReqRlimit (min m n) -- pick min, doesn't hurt
        f (ReqRlimit m) ReqAllow      = ReqRlimit m -- enforce limit , doesn't hurt
        f _             ch            = ch

emptyTable :: MatchTable
emptyTable = MatchTable []

intersectTable :: (Action -> Action -> Action) 
               -> MatchTable 
               -> MatchTable
               -> Bool        -- Set to true iff used in unionTable
               -> MatchTable
intersectTable cmb (MatchTable tbl1) (MatchTable tbl2) optForUnion =
  MatchTable tbl'
  where tbl' = [ (f1 ∩ f2, cmb a1 a2) | (f1, a1) <- tbl1,
                                        (f2, a2) <- tbl2,
                                        not (Flows.null (f1 ∩ f2)),
                          -- skip unnecessary intersection as an optimization:
                                        not (optForUnion && (a1 == a2)) ]
        (∩) = Flows.intersection

-- Note: this is a left-biased union.
unionTable :: (Action -> Action -> Action)
           -> MatchTable
           -> MatchTable
           -> MatchTable
unionTable cmb mt1@(MatchTable tbl1) mt2@(MatchTable tbl2) = 
  MatchTable (tbl' ++ tbl1 ++ tbl2)
    where (MatchTable tbl') = intersectTable cmb mt1 mt2 True

concatTable mt1@(MatchTable tbl1) mt2@(MatchTable tbl2) =
  MatchTable (tbl1 ++ tbl2)

shareToTable :: Integer
             -> Share
             -> MatchTable
shareToTable now share = 
  foldl (unionTable combineSiblingActions) emptyTable (map reqToTbl reqs)
    where reqs = filter (activeAt now) (shareReq share)
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

-- Scans MatchTable left to right and drops any rules whose flow is shadowed
-- by a flow to the left in the table.
condense :: Integer -> MatchTable -> MatchTable
condense now (MatchTable tbl) = MatchTable (loop [] tbl')
  where loop _           [] = []
        loop lhs ((f, action):rest) = 
          case any (\leftF -> f `Flows.isSubFlow` leftF) lhs of
            True  -> loop lhs rest
            False -> (f,action):(loop (f:lhs) rest)
        tbl' = filter isNotExpired tbl
        isNotExpired (_, Just (_, expiry)) = expiry >= (fromInteger now)
        isNotExpired (_, Nothing) = False

compileShareTree :: Integer    -- ^current time
                 -> Tree Share -- ^share tree
                 -> MatchTable
compileShareTree now tree = condense now (shareTreeToTable now tree)
