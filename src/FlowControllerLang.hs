module FlowControllerLang 
  ( DNP
  , createSpeakerM
  , giveReferenceM
  , giveDefaultReferenceM
  , newShareM
  , requestM
  , currentRequestsM
  , tickM
  , getTimeM
  , listShareRefsByFlowGroupM
  , listShareRefsByUserM
  , getScheduleM
  , evalDNP
  , runDNP
  ) where

import Base
import qualified Control.Monad.State as StateM
import FlowController
import Set (Set)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as Aeson
import Control.Monad.Trans
import ShareSemantics

type DNP a = StateM.StateT State IO a

evalDNP :: DNP a -> IO a
evalDNP m = StateM.evalStateT m emptyState

runDNP = StateM.runStateT

boolWrapper exp = do
  s <- StateM.get
  case exp s of
    Nothing -> return (BoolResult False)
    Just s' -> do
      StateM.put s'
      return (BoolResult True)


createSpeakerM :: Speaker -> DNP DNPResult
createSpeakerM spk = boolWrapper (createSpeaker spk)

giveReferenceM :: Speaker
              -> ShareRef
              -> Speaker
              -> DNP DNPResult
giveReferenceM fromSpk share toSpk = 
  boolWrapper (giveReference fromSpk share toSpk)

giveDefaultReferenceM fromSpk share = 
  boolWrapper (giveDefaultReference fromSpk share)

newShareM x1 x2 x3 = boolWrapper (newShare x1 x2 x3)

requestM x1 x2 = boolWrapper (request x1 x2)

currentRequestsM :: DNP ([Req])
currentRequestsM = do
  s <- StateM.get
  return (currentRequests s)

getScheduleM :: Speaker -> ShareRef -> DNP DNPResult
getScheduleM speaker shareName = do
  state <- StateM.get
  case getSchedule speaker shareName state of
    Nothing -> return (BoolResult False)
    Just sched -> return (ScheduleResult sched)
  

tickM :: Integer -> DNP DNPResult
tickM t = do
  s <- StateM.get
  showStateHandle <- lift (readIORef traceFile)
  case showStateHandle of
    Nothing -> return ()
    Just handle -> do
      let str = BS.concat ["var instant = ", Aeson.encode s, ";\n"]
      lift $ BS.hPutStr handle str
  let s' = tick t s
  StateM.put s'
  return (BoolResult True)

getTimeM :: DNP(Integer)
getTimeM = do
  s <- StateM.get
  return (stateNow s)

listShareRefsByFlowGroupM :: FlowGroup -> DNP DNPResult
listShareRefsByFlowGroupM x1 = do
  s <- StateM.get
  return (ShareRefsResult (listShareRefsByFlowGroup x1 s))

listShareRefsByUserM :: Speaker -> DNP DNPResult
listShareRefsByUserM x1 = do
  s <- StateM.get
  return (ShareRefsResult (listShareRefsByUser x1 s))
