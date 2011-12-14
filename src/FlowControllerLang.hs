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

import Control.Monad
import Data.IORef
import Base
import qualified Control.Monad.State as StateM
import EmitFML
import FlowController
import Set(Set)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as Aeson
import Control.Monad.Trans
import StateDump

type DNP a = StateM.StateT State IO a

evalDNP :: DNP a -> IO a
evalDNP m = StateM.evalStateT m emptyState

runDNP = StateM.runStateT


fmlDNP :: DNP a -> IO String
fmlDNP m = do
  st <- StateM.execStateT m emptyState
  return (emitFML st)

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

newShareM x1 x2 x3 x4 = boolWrapper (newShare x1 x2 x3 x4)

requestM x1 x2 = boolWrapper (request x1 x2)

currentRequestsM :: DNP ([Req])
currentRequestsM = do
  s <- StateM.get
  return (currentRequests s)

tickM :: Integer -> DNP Bool
tickM t = do
  showState <- lift (readIORef isTracing)
  s <- StateM.get
  when showState $ lift (BS.putStrLn (Aeson.encode s))
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
