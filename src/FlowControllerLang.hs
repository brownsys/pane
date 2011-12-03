module FlowControllerLang 
  ( DNP
  , createSpeakerM
  , giveReferenceM
  , newResAcctM
  , runDNP
  ) where

import qualified Control.Monad.State as StateM
import FlowController

type DNP a = StateM.State State a

runDNP :: DNP a -> a
runDNP m = StateM.evalState m emptyState

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

newResAcctM x1 x2 x3 x4 x5 x6 = boolWrapper (newResAcct x1 x2 x3 x4 x5 x6)


