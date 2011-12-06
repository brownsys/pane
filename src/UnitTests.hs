module UnitTests where

import Test.HUnit
import FlowController
import qualified Set
import FlowControllerLang
-- import EmitFML

instance AssertionPredicable (Maybe a) where
  assertionPredicate Nothing = return False
  assertionPredicate (Just _) = return True

unexpectedState (Just _) = Nothing
unexpectedState Nothing = Just emptyState

foreverResv shareRef flow size =
 Resv shareRef flow 0 NoLimit size

test1 = evalDNP $ do
  createSpeakerM "arjun"

test2 = evalDNP $ do
  b <- createSpeakerM "root"
  return (not b)

test3 = evalDNP $ do
  b1 <- createSpeakerM "arjun"
  b2 <- createSpeakerM "adf"
  return (b1 && b2)

test4 = evalDNP $ do
  b1 <- createSpeakerM "arjun"
  b2 <- createSpeakerM "arjun"
  return (b1 && not b2)

test5 = evalDNP $ do
  b1 <- createSpeakerM "arjun"
  b2 <- giveReferenceM "root" rootShareRef "arjun"
  return (b1 && b2)

test6 = evalDNP $ do
  b1 <- createSpeakerM "arjun" 
  b2 <- newShareM "root" rootShareRef "arjun-share" anyFlow (DiscreteLimit 100)
  b3 <- giveReferenceM "root" "arjun-share" "arjun"
  return (b1 && b2 && b3)

frag1 limitForAdf = do
  b1 <- createSpeakerM "arjun"
  b2 <- createSpeakerM "adf"
  b3 <- newShareM "root" rootShareRef "arjun-share" anyFlow (DiscreteLimit 100)
  b4 <- giveReferenceM "root" "arjun-share" "arjun"
  b5 <- newShareM "arjun" "arjun-share" "adf-share" anyFlow (DiscreteLimit limitForAdf)
  b6 <- giveReferenceM "arjun" "adf-share" "adf"
  return (b1 && b2 && b3 && b4 && b5 && b6)

test7 = evalDNP $ do
   b1 <- frag1 200
   return (not b1)

test8 = evalDNP $ do frag1 50

test9 =  evalDNP $ do
  b1 <- frag1 50
  b2 <- giveReferenceM "arjun" "adf-share" "non user" 
  return (b1 && not b2)

  
frag2 = do
  b1 <- newShareM "root" rootShareRef "net0" anyFlow (DiscreteLimit 200)
  return b1

test10 = evalDNP $ do frag2

test11 = evalDNP $ do
  b1 <- frag2 
  b2 <- reserveM "root" (foreverResv "net0" anyFlow 300)
  return (b1 && not b2)

test12 = evalDNP $ do
  b1 <- frag2
  b2 <- reserveM "root" (foreverResv rootShareRef anyFlow 300)
  return (b1 && b2)

test13 = evalDNP $ do
  b1 <- frag2
  b2 <- reserveM "root" (foreverResv "net0" anyFlow 100)
  b3 <- reserveM "root" (foreverResv "net0" anyFlow 100)
  return (b1 && b2 && b3)

test14 = evalDNP $ do
  b1 <- frag2
  b2 <- reserveM "root" (foreverResv "net0" anyFlow 100)
  b3 <- reserveM "root" (foreverResv "net0" anyFlow 100)
  b4 <- reserveM "root" (foreverResv "net0" anyFlow 100)
  return (b1 && b2 && b3 && not b4)


frag3 = do 
  b1 <- frag2
  b2 <- createSpeakerM "adf"
  b3 <- newShareM "root" "net0" "adfShare" anyFlow 
           (DiscreteLimit 150)
  b4 <- giveReferenceM "root" "adfShare" "adf"
  b5 <- reserveM "adf" (foreverResv "adfShare" anyFlow 100)
  return (b1 && b2 && b3 && b4 && b5)

test15 = evalDNP $ do frag3

test16 = evalDNP $ do
  b1 <- frag3
  b2 <- reserveM "root" (foreverResv "net0" anyFlow 100)
  return (b1 && b2)

test16a = evalDNP $ do
  b1 <- frag3
  b2 <- reserveM "root" (foreverResv "adfShare" anyFlow 50)
  return (b1 && b2)

test16b = evalDNP $ do
  b1 <- frag3
  b2 <- reserveM "root" (foreverResv "adfShare" anyFlow 51)
  return (b1 && not b2)

test17 = evalDNP $ do
  b1 <- frag3
  b2 <- reserveM "root" (foreverResv "net0" anyFlow 101)
  return (b1 && not b2)

test18 = evalDNP $ do
  b1 <- frag3
  b2 <- reserveM "adf" (foreverResv "adfShare" anyFlow 51)
  return (b1 && not b2)

frag4 = do
  b1 <- createSpeakerM "arjun"
  b2 <- newShareM "root" rootShareRef "hadoop-share" anyFlow (DiscreteLimit 100)
  b3 <- giveDefaultReferenceM "root" "hadoop-share"
  b4 <- reserveM "arjun" (foreverResv "hadoop-share" anyFlow 25)
  return (b1 && b2 && b3 && b4)

test19 = evalDNP $ do frag4

test20 = evalDNP $ do
  b1 <- frag4
  b2 <- createSpeakerM "adf"
  b3 <- reserveM "adf" (foreverResv "hadoop-share" anyFlow 25)
  return (b1 && b2 && b3)

arjunFlow = anyFlow { flowSend = Set.singleton "arjun" }

frag5 = do
  b1 <- createSpeakerM "arjun"
  b2 <- newShareM "root" rootShareRef "arjun-share" arjunFlow (DiscreteLimit 100)
  b3 <- giveReferenceM "root" "arjun-share" "arjun"
  b4 <- reserveM "arjun" (foreverResv "arjun-share" arjunFlow 50)
  s  <- currentReservationsM
  let b5 = Set.fromList s == Set.singleton (
                               foreverResv "arjun-share" arjunFlow 50)
  return (b1 && b2 && b3 && b4 && b5)

test21 = evalDNP $ frag5


frag6 = do
  b1 <- frag5
  let arjunWebFlow = (arjunFlow { flowSrcPort = Set.singleton 80}) 
  b2 <- reserveM "arjun" (foreverResv "arjun-share" arjunWebFlow 50)
  s  <- currentReservationsM
  let b3 = Set.fromList s == Set.fromList [
                                   (foreverResv "arjun-share" arjunFlow  50),
                                   (foreverResv "arjun-share" arjunWebFlow 50)]
  return (b1 && b2 && b3)

test22 = evalDNP frag6

test23 = evalDNP $ do
  b1 <- frag5
  b2 <- reserveM "arjun" (foreverResv "arjun-share" anyFlow 50)
  return (b1 && not b2)

test24 = evalDNP $ do
  b1 <- frag2 -- root creates net0 share
  let resv = Resv rootShareRef anyFlow 0 (DiscreteLimit 10) 100
  b2 <- reserveM rootSpeaker resv
  s <- currentReservationsM
  let b3 = s == [resv]
  tickM 5
  s <- currentReservationsM
  let b4 = s == [resv]
  tickM 2
  s <- currentReservationsM
  let b5 = s == [resv]
  tickM 4
  s <- currentReservationsM
  let b6 = s == []
  return (b1 && b2 && b3 && b4 && b5 && b6)

test25 = evalDNP $ do
  b1 <- frag2
  let resv = Resv rootShareRef anyFlow 5 (DiscreteLimit 10) 100
  b2 <- reserveM rootSpeaker resv
  s <- currentReservationsM
  let b3 = s == []
  tickM 4
  s <- currentReservationsM
  let b4 = s == []
  tickM 1
  s <- currentReservationsM
  let b5 = s == [resv]
  tickM 5
  s <- currentReservationsM
  let b6 = s == [resv]
  tickM 1
  s <- currentReservationsM
  let b7 = s == []
  return (b1 && b2 && b3 && b4 && b5 && b7)
  

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
  , test10 ~? "create physical share"
  , test11 ~? "cannot exceed quota"
  , test12 ~? "it is silly to reserve from unlimited share"
  , test13 ~? "reserve to limit"
  , test14 ~? "reserve beyond limit"
  , test15 ~? "adf can reserve"
  , test16 ~? "reserve to limit in separate shares"
  , test16a ~? "root reserve from ADF share"
  , test16b ~? "root reserve too much from ADF share"
  , test17 ~? "cannot give root what he wants"
  , test18 ~? "exceeded limit on subshare"
  , test19 ~? "use default reference"
  , test20 ~? "use default reference with new user"
  , test21 ~? "put flow restrictions on subshare"
  , test22 ~? "reserve with more restricted flow than restriction on share"
  , test23 ~? "cannot reserve with less restricted flow than restr. on share"
  , test24 ~? "reservation should expire"
  , test25 ~? "reservation in the future"
  ]

main :: IO ()
main = do
  runTestTT allTests
  return ()
