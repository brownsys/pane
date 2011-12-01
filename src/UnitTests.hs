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
                    rootAcct
  giveReference "root" ref "arjun" st

test7 = unexpectedState $ do
  st <- createSpeaker "arjun" emptyState
  st <- createSpeaker "adf" st
  let ref = AcctRef (Set.singleton "arjun") anyFlow (DiscreteLimit 100) 
                    rootAcct
  st <- giveReference "root" ref "arjun" st
  giveReference "arjun" (ref { acctRefResLimit = DiscreteLimit 200 })
    "adf" st

test8 = do
  st <- createSpeaker "arjun" emptyState
  st <- createSpeaker "adf" st
  let ref = AcctRef (Set.singleton "arjun") anyFlow (DiscreteLimit 100) 
                    rootAcct
  st <- giveReference "root" ref "arjun" st
  giveReference "arjun" (ref { acctRefResLimit = DiscreteLimit 50 })
    "adf" st

test9 = unexpectedState $ do
  st <- createSpeaker "arjun" emptyState
  st <- createSpeaker "adf" st
  let ref = AcctRef (Set.singleton "arjun") anyFlow (DiscreteLimit 100) 
                    rootAcct
  st <- giveReference "root" ref "arjun" st
  giveReference "arjun" (ref { acctRefResLimit = DiscreteLimit 50 })
    "sk" st
  
test10 = do
  st <- newResAcct "root" rootAcctRef "net0" (DiscreteLimit $ 10^100) emptyState
  return ()

test11 = unexpectedState $ do
  st <- newResAcct "root" rootAcctRef "net0" (DiscreteLimit 200) emptyState
  let net0Ref = rootAcctRef { acctRefAccount = "net0" }
  reserve "root" net0Ref 300 st

test12 = do
  st <- newResAcct "root" rootAcctRef "net0" (DiscreteLimit 200) emptyState
  reserve "root" rootAcctRef 300 st

test13 = do
  st <- newResAcct "root" rootAcctRef "net0" (DiscreteLimit 200) emptyState
  let net0Ref = rootAcctRef { acctRefAccount = "net0" }
  reserve "root" net0Ref 100 st
  reserve "root" net0Ref 100 st

test14 = do
  st <- newResAcct "root" rootAcctRef "net0" (DiscreteLimit 200) emptyState
  let net0Ref = rootAcctRef { acctRefAccount = "net0" }
  reserve "root" net0Ref 100 st
  reserve "root" net0Ref 100 st
  reserve "root" net0Ref 100 st

test15 = do
  st <- newResAcct "root" rootAcctRef "net0" (DiscreteLimit 200) emptyState
  st <- createSpeaker "adf" st
  let adfRef = rootAcctRef { acctRefAccount = "net0", 
                             acctRefResLimit = DiscreteLimit 150 }
  st <- giveReference "root" adfRef "adf" st
  reserve "adf" adfRef 100 st

test16 = do
  st <- newResAcct "root" rootAcctRef "net0" (DiscreteLimit 200) emptyState
  st <- createSpeaker "adf" st
  let net0Ref = rootAcctRef { acctRefAccount = "net0" }
  let adfRef = rootAcctRef { acctRefAccount = "net0", 
                             acctRefResLimit = DiscreteLimit 150 }
  st <- giveReference "root" adfRef "adf" st
  st <- reserve "root" net0Ref 100 st
  reserve "adf" adfRef 100 st

test17 = unexpectedState $ do
  st <- newResAcct "root" rootAcctRef "net0" (DiscreteLimit 200) emptyState
  st <- createSpeaker "adf" st
  let net0Ref = rootAcctRef { acctRefAccount = "net0" }
  let adfRef = rootAcctRef { acctRefAccount = "net0", 
                             acctRefResLimit = DiscreteLimit 150 }
  st <- giveReference "root" adfRef "adf" st
  st <- reserve "root" net0Ref 100 st
  reserve "adf" adfRef 101 st

test18 = do
  st <- newResAcct "root" rootAcctRef "net0" (DiscreteLimit 200) emptyState
  st <- createSpeaker "adf" st
  st <- createSpeaker "arjun" st
  let net0Ref = rootAcctRef { acctRefAccount = "net0" }
  let adfRef = rootAcctRef { acctRefAccount = "net0", 
                             acctRefResLimit = DiscreteLimit 150 }
  st <- giveReference "root" adfRef "adf" st
--  st <- reserve "root" net0Ref 100 st
  let arjunRef = adfRef { acctRefResLimit = DiscreteLimit 75 }
  st <- giveReference "adf" arjunRef "arjun" st
  st <- newResAcct "arjun" arjunRef "arjunAcct" (DiscreteLimit 70) st
  let arjunSubRef = arjunRef { acctRefAccount = "arjunAcct" }
  reserve "arjun" arjunSubRef 50 st

test19 = unexpectedState $ do
  st <- newResAcct "root" rootAcctRef "net0" (DiscreteLimit 200) emptyState
  st <- createSpeaker "adf" st
  st <- createSpeaker "arjun" st
  let net0Ref = rootAcctRef { acctRefAccount = "net0" }
  let adfRef = rootAcctRef { acctRefAccount = "net0", 
                             acctRefResLimit = DiscreteLimit 150 }
  st <- giveReference "root" adfRef "adf" st
--  st <- reserve "root" net0Ref 100 st
  let arjunRef = adfRef { acctRefResLimit = DiscreteLimit 75 }
  st <- giveReference "adf" arjunRef "arjun" st
  st <- newResAcct "arjun" arjunRef "arjunAcct" (DiscreteLimit 70) st
  let arjunSubRef = arjunRef { acctRefAccount = "arjunAcct" }
  reserve "arjun" arjunSubRef 71 st
 
test20 = do
  st <- newResAcct "root" rootAcctRef "net0" (DiscreteLimit 200) emptyState
  st <- createSpeaker "adf" st
  st <- createSpeaker "arjun" st
  let net0Ref = rootAcctRef { acctRefAccount = "net0" }
  let adfRef = rootAcctRef { acctRefAccount = "net0", 
                             acctRefResLimit = DiscreteLimit 150 }
  st <- giveReference "root" adfRef "adf" st
--  st <- reserve "root" net0Ref 100 st
  let arjunRef = adfRef { acctRefResLimit = DiscreteLimit 75 }
  st <- giveReference "adf" arjunRef "arjun" st
  st <- newResAcct "arjun" arjunRef "arjunAcct" (DiscreteLimit 70) st
  let arjunSubRef = arjunRef { acctRefAccount = "arjunAcct" }
  reserve "arjun" arjunRef 71 st

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
  , test10 ~? "create physical account"
  , test11 ~? "cannot exceed quota"
  , test12 ~? "it is silly to reserve from unlimited account"
  , test13 ~? "reserve to limit"
  , test14 ~? "reserve beyond limit"
  , test15 ~? "adf can reserve"
  , test16 ~? "reserve to limit in separate accounts"
  , test17 ~? "cannot give adf what he wants"
  , test18 ~? "reserve from subaccount"
  , test19 ~? "exceeded limit on subaccount"
  , test20 ~? "reserve from account which also has subaccount"
  ]

main :: IO ()
main = do
  runTestTT allTests
  return ()
