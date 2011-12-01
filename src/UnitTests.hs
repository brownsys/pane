module UnitTests where

import Test.HUnit
import FlowController
import qualified Set

instance AssertionPredicable (Maybe a) where
  assertionPredicate Nothing = return False
  assertionPredicate (Just _) = return True

unexpectedState (Just _) = Nothing
unexpectedState Nothing = Just emptyState


test1 =
  createSpeaker "arjun" emptyState

test2 =
  (unexpectedState (createSpeaker "root" emptyState))

test3 = do
  st <- createSpeaker "arjun" emptyState
  st <- createSpeaker "adf" st
  return True

test4 = unexpectedState $ do
  st <- createSpeaker "arjun" emptyState
  st <- createSpeaker "arjun" st
  return True

test5 = do
  st <- createSpeaker "arjun" emptyState
  giveReference "root" rootAcctRef "arjun" st

test6 = do
  st <- createSpeaker "arjun" emptyState
  let ref = AcctRef (Set.singleton "arjun") anyFlow (DiscreteLimit 100) 
  giveReference "root" ref "arjun" st

test7 = unexpectedState $ do
  st <- createSpeaker "arjun" emptyState
  st <- createSpeaker "adf" st
  let ref = AcctRef (Set.singleton "arjun") anyFlow (DiscreteLimit 100) 
  st <- giveReference "root" ref "arjun" st
  giveReference "arjun" (ref { acctRefReservation = DiscreteLimit 200 })
    "adf" st

test8 = do
  st <- createSpeaker "arjun" emptyState
  st <- createSpeaker "adf" st
  let ref = AcctRef (Set.singleton "arjun") anyFlow (DiscreteLimit 100) 
  st <- giveReference "root" ref "arjun" st
  giveReference "arjun" (ref { acctRefReservation = DiscreteLimit 50 })
    "adf" st

test9 = unexpectedState $ do
  st <- createSpeaker "arjun" emptyState
  st <- createSpeaker "adf" st
  let ref = AcctRef (Set.singleton "arjun") anyFlow (DiscreteLimit 100) 
  st <- giveReference "root" ref "arjun" st
  giveReference "arjun" (ref { acctRefReservation = DiscreteLimit 50 })
    "sk" st
  
  
 

allTests = TestList
  [ test1 ~? "cannot create speaker"
  , test2 ~? "duplicate speaker"
  , test3 ~? "cannot create two speakers"
  , test4 ~? "duplicate speaker arjun"
  , test5 ~? "giving root-reference"
  , test6 ~? "giving user-restricted reference"
  , test7 ~? "cannot increase limit on a reference"
  , test8 ~? "can decrease limit on a reference"
  , test9 ~? "can decrease limit on a reference"
  ]

main :: IO ()
main = do
  runTestTT allTests
  return ()
