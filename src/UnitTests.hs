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

test6 = runDNP $ do
  b1 <- createSpeakerM "arjun" 
  b2 <- newResAcctM "root" rootAcct "arjun-account" (Set.singleton "arjun") 
           anyFlow (DiscreteLimit 100)
  b3 <- giveReferenceM "root" "arjun-account" "arjun"
  return (b1 && b2 && b3)

frag1 limitForAdf = do
  b1 <- createSpeakerM "arjun"
  b2 <- createSpeakerM "adf"
  b3 <- newResAcctM "root" rootAcct "arjun-account" Set.all
                    anyFlow (DiscreteLimit 100)
  b4 <- giveReferenceM "root" "arjun-account" "arjun"
  b5 <- newResAcctM "arjun" "arjun-account" "adf-account" (Set.singleton "adf") 
                    anyFlow (DiscreteLimit limitForAdf)
  b6 <- giveReferenceM "arjun" "adf-account" "adf"
  return (b1 && b2 && b3 && b4 && b5 && b6)

test7 = runDNP $ do
   b1 <- frag1 200
   return (not b1)

test8 = runDNP $ do frag1 50

test9 =  runDNP $ do
  b1 <- frag1 50
  b2 <- giveReferenceM "arjun" "adf-account" "non user" 
  return (b1 && not b2)

  
frag2 = do
  b1 <- newResAcctM "root" rootAcctRef "net0" Set.all anyFlow 
                    (DiscreteLimit 200)
  return b1

test10 = runDNP $ do frag2

test11 = runDNP $ do
  b1 <- frag2 
  b2 <- reserveM "root" "net0" 300
  return (b1 && not b2)

test12 = runDNP $ do
  b1 <- frag2
  b2 <- reserveM "root" "root" 300
  return (b1 && b2)

test13 = runDNP $ do
  b1 <- frag2
  b2 <- reserveM "root" "net0" 100
  b3 <- reserveM "root" "net0" 100
  return (b1 && b2 && b3)

test14 = runDNP $ do
  b1 <- frag2
  b2 <- reserveM "root" "net0" 100
  b3 <- reserveM "root" "net0"  100
  b4 <- reserveM "root" "net0" 100
  return (b1 && b2 && b3 && not b4)


frag3 = do 
  b1 <- frag2
  b2 <- createSpeakerM "adf"
  b3 <- newResAcctM "root" "net0" "adfAcct" (Set.singleton "adf") anyFlow 
           (DiscreteLimit 150)
  b4 <- giveReferenceM "root" "adfAcct" "adf"
  b5 <- reserveM "adf" "adfAcct" 100
  return (b1 && b2 && b3 && b4 && b5)

test15 = runDNP $ do frag3

test16 = runDNP $ do
  b1 <- frag3
  b2 <- reserveM "root" "net0" 100
  return (b1 && b2)

test17 = runDNP $ do
  b1 <- frag3
  b2 <- reserveM "root" "net0" 101
  return (b1 && not b2)

test18 = runDNP $ do
  b1 <- frag3
  b2 <- reserveM "adf" "adfAcct" 51
  return (b1 && not b2)

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
