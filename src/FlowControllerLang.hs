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
  , findSharesByFlowGroupM
  , evalDNP
  , fmlDNP
  , runDNP
  ) where

import qualified Control.Monad.State as StateM
import EmitFML
import FlowController
import Set(Set)

type DNP a = StateM.State State a

evalDNP :: DNP a -> a
evalDNP m = StateM.evalState m emptyState

runDNP = StateM.runState


fmlDNP :: DNP a -> String
fmlDNP m = emitFML (StateM.execState m emptyState)

boolWrapper exp = do
  s <- StateM.get
  case exp s of
    Nothing -> return False
    Just s' -> do
      StateM.put s'
      return True


createSpeakerM :: Speaker -> DNP Bool
createSpeakerM spk = boolWrapper (createSpeaker spk)

giveReferenceM :: Speaker
              -> ShareRef
              -> Speaker
              -> DNP Bool
giveReferenceM fromSpk share toSpk = 
  boolWrapper (giveReference fromSpk share toSpk)

giveDefaultReferenceM fromSpk share = boolWrapper (giveDefaultReference fromSpk share)

newShareM x1 x2 x3 x4 x5 = boolWrapper (newShare x1 x2 x3 x4 x5)

requestM x1 x2 = boolWrapper (request x1 x2)

currentRequestsM :: DNP ([Req])
currentRequestsM = do
  s <- StateM.get
  return (currentRequests s)

tickM :: Integer -> DNP Bool
tickM t = do
  s <- StateM.get
  let s' = tick t s
  StateM.put s'
  return True

getTimeM :: DNP(Integer)
getTimeM = do
  s <- StateM.get
  return (stateNow s)

findSharesByFlowGroupM x1 = do
  s <- StateM.get
  return (findSharesByFlowGroup x1 s)
