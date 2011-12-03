module UnitTests where

import Test.HUnit
import FlowController
import qualified Set
import FlowControllerLang

instance AssertionPredicable (Maybe a) where
  assertionPredicate Nothing = return False
  assertionPredicate (Just _) = return True

unexpectedState (Just _) = Nothing
unexpectedState Nothing = Just emptyState


test1 = runDNP $ do
  createSpeakerM "arjun"

test2 = runDNP $ do
  b <- createSpeakerM "root"
  return (not b)

test3 = runDNP $ do
  b1 <- createSpeakerM "arjun"
  b2 <- createSpeakerM "adf"
  return (b1 && b2)

test4 = runDNP $ do
  b1 <- createSpeakerM "arjun"
  b2 <- createSpeakerM "arjun"
  return (b1 && not b2)

test5 = runDNP $ do
  b1 <- createSpeakerM "arjun"
  b2 <- giveReferenceM "root" rootAcctRef "arjun"
  return (b1 && b2)

test6 = do
  st <- createSpeaker "arjun" emptyState
  st <- newResAcct "root" rootAcct "arjun-account" (Set.singleton "arjun") 
           anyFlow (DiscreteLimit 100) st
  giveReference "root" "arjun-account" "arjun" st

frag2 limitForAdf = do
  st <- createSpeaker "arjun" emptyState
  st <- createSpeaker "adf" st
  st <- newResAcct "root" rootAcct "arjun-account" Set.all
          anyFlow (DiscreteLimit 100) st
  st <- giveReference "root" "arjun-account" "arjun" st
  st <- newResAcct "arjun" "arjun-account" "adf-account" (Set.singleton "adf") 
          anyFlow (DiscreteLimit limitForAdf) st
  giveReference "arjun" "adf-account" "adf" st

test7 = unexpectedState (frag2 200)

test8 = frag2 50

test9 = unexpectedState $ do
  st <- frag2 50
  giveReference "arjun" "adf-account" "non user" st

  
test10 = 
 newResAcct "root" rootAcctRef "net0" Set.all anyFlow 
           (DiscreteLimit 200) emptyState

test11 = unexpectedState $ do
  st <- test10
  reserve "root" "net0" 300 st

test12 = do
  st <- test10
  reserve "root" "root" 300 st

test13 = do
  st <- test10
  st <- reserve "root" "net0" 100 st
  reserve "root" "net0" 100 st

test14 = unexpectedState $ do
  st <- test10
  st <- reserve "root" "net0" 100 st
  st <- reserve "root" "net0"  100 st
  reserve "root" "net0" 100 st

test15 = do
  st <- test10
  st <- createSpeaker "adf" st
  st <- newResAcct "root" "net0" "adfAcct" (Set.singleton "adf") anyFlow 
           (DiscreteLimit 150) st
  st <- giveReference "root" "adfAcct" "adf" st
  reserve "adf" "adfAcct" 100 st


test16 = do
  st <- test15
  reserve "root" "net0" 100 st


test17 = unexpectedState $ do
  st <- test15
  reserve "root" "net0" 101 st

test18 = unexpectedState $ do
  st <- test15
  reserve "adf" "adfAcct" 51 st

allTests = TestList
  [ test1 ~? "cannot create speaker"
  , test2 ~? "duplicate speaker"
  , test3 ~? "cannot create two speakers"
  , test4 ~? "duplicate speaker arjun"
  , test5 ~? "giving root-reference"
  , test6 ~? "giving user-restricted reference"
  , test7 ~? "cannot increase limit on a reference"
  , test8 ~? "can decrease limit on a reference"
  , test9 ~? "giving to non-existant user"
  , test10 ~? "create physical account"
  , test11 ~? "cannot exceed quota"
  , test12 ~? "it is silly to reserve from unlimited account"
  , test13 ~? "reserve to limit"
  , test14 ~? "reserve beyond limit"
  , test15 ~? "adf can reserve"
  , test16 ~? "reserve to limit in separate accounts"
  , test17 ~? "cannot give root what he wants"
  , test18 ~? "exceeded limit on subaccount"
  ]

main :: IO ()
main = do
  runTestTT allTests
  return ()
