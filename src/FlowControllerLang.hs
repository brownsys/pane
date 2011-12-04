module FlowControllerLang 
  ( DNP
  , createSpeakerM
  , giveReferenceM
  , giveDefaultReferenceM
  , newResAcctM
  , reserveM
  , currentReservationsM
  , runDNP
  , fmlDNP
  ) where

import qualified Control.Monad.State as StateM
import EmitFML
import FlowController
import Set(Set)

type DNP a = StateM.State State a

runDNP :: DNP a -> a
runDNP m = StateM.evalState m emptyState

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
              -> AcctRef
              -> Speaker
              -> DNP Bool
giveReferenceM fromSpk share toSpk = 
  boolWrapper (giveReference fromSpk share toSpk)

giveDefaultReferenceM fromSpk share = boolWrapper (giveDefaultReference fromSpk share)

newResAcctM x1 x2 x3 x4 x5 x6 = boolWrapper (newResAcct x1 x2 x3 x4 x5 x6)

reserveM x1 x2 x3 x4 = boolWrapper (reserve x1 x2 x3 x4)

currentReservationsM :: DNP (Set (FlowGroup, Integer))
currentReservationsM = do
  s <- StateM.get
  return (currentReservations s)
