module FlowControllerLang 
  ( PANE 
  , createSpeakerM
  , giveReferenceM
  , giveDefaultReferenceM
  , newShareM
  , requestM
  , getTimeM
  , listShareRefsByFlowGroupM
  , listShareRefsByUserM
  , getScheduleM
  , evalPANE
  , runPANE
  ) where

import Base
import qualified Control.Monad.State as StateM
import FlowController
import Set (Set)
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Trans
import HFT

type PANE a = StateM.StateT State IO a

evalPANE :: PANE a -> IO a
evalPANE m = StateM.evalStateT m emptyState

runPANE = StateM.runStateT

boolWrapper exp = do
  s <- StateM.get
  case exp s of
    Nothing -> return (BoolResult False)
    Just s' -> do
      StateM.put s'
      return (BoolResult True)


createSpeakerM :: Speaker -> PANE Result
createSpeakerM spk = boolWrapper (createSpeaker spk)

giveReferenceM :: Speaker
              -> ShareRef
              -> Speaker
              -> PANE Result
giveReferenceM fromSpk share toSpk = 
  boolWrapper (giveReference fromSpk share toSpk)

giveDefaultReferenceM fromSpk share = 
  boolWrapper (giveDefaultReference fromSpk share)

newShareM x1 x2 x3 = boolWrapper (newShare x1 x2 x3)

requestM x1 x2 = boolWrapper (request x1 x2)

getScheduleM :: Speaker -> ShareRef -> PANE Result
getScheduleM speaker shareName = do
  state <- StateM.get
  case getSchedule speaker shareName state of
    Nothing -> return (BoolResult False)
    Just sched -> return (ScheduleResult sched)

getTimeM :: PANE(Integer)
getTimeM = do
  s <- StateM.get
  return (stateNow s)

listShareRefsByFlowGroupM :: FlowGroup -> PANE Result
listShareRefsByFlowGroupM x1 = do
  s <- StateM.get
  return (ShareRefsResult (listShareRefsByFlowGroup x1 s))

listShareRefsByUserM :: Speaker -> PANE Result
listShareRefsByUserM x1 = do
  s <- StateM.get
  return (ShareRefsResult (listShareRefsByUser x1 s))
