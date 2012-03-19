-- |Implementation of the gaurantee semantics and forwarding semantics of 
-- 'ShareTree's.
module ShareSemantics 
  ( evalShareTree
  , Action (..)
  , Admit (..)
  ) where

import Data.Tree (Tree (..))
import FlowController (Share (..))
import qualified PriorityQueue as PQ
import Base

data Admit = Allow | Deny

data Action = Action {
  gmb   :: Maybe Integer,
  admit :: Maybe Admit
}

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
        
   