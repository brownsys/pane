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
  b2 <- giveReferenceM "root" rootShareRef "arjun"
  return (b1 && b2)

test6 = runDNP $ do
  b1 <- createSpeakerM "arjun" 
  b2 <- newShareM "root" rootShareRef "arjun-share" (Set.singleton "arjun") 
           anyFlow (DiscreteLimit 100)
  b3 <- giveReferenceM "root" "arjun-share" "arjun"
  return (b1 && b2 && b3)

frag1 limitForAdf = do
  b1 <- createSpeakerM "arjun"
  b2 <- createSpeakerM "adf"
  b3 <- newShareM "root" rootShareRef "arjun-share" Set.all -- IRL, Arjun's group?
                    anyFlow (DiscreteLimit 100)
  b4 <- giveReferenceM "root" "arjun-share" "arjun"
  b5 <- newShareM "arjun" "arjun-share" "adf-share" (Set.singleton "adf") 
                    anyFlow (DiscreteLimit limitForAdf)
  b6 <- giveReferenceM "arjun" "adf-share" "adf"
  return (b1 && b2 && b3 && b4 && b5 && b6)

test7 = runDNP $ do
   b1 <- frag1 200
   return (not b1)

test8 = runDNP $ do frag1 50

test9 =  runDNP $ do
  b1 <- frag1 50
  b2 <- giveReferenceM "arjun" "adf-share" "non user" 
  return (b1 && not b2)

  
frag2 = do
  b1 <- newShareM "root" rootShareRef "net0" Set.all anyFlow 
                    (DiscreteLimit 200)
  return b1

test10 = runDNP $ do frag2

test11 = runDNP $ do
  b1 <- frag2 
  b2 <- reserveM "root" (foreverResv "net0" anyFlow 300)
  return (b1 && not b2)

test12 = runDNP $ do
  b1 <- frag2
  b2 <- reserveM "root" (foreverResv rootShareRef anyFlow 300)
  return (b1 && b2)

test13 = runDNP $ do
  b1 <- frag2
  b2 <- reserveM "root" (foreverResv "net0" anyFlow 100)
  b3 <- reserveM "root" (foreverResv "net0" anyFlow 100)
  return (b1 && b2 && b3)

test14 = runDNP $ do
  b1 <- frag2
  b2 <- reserveM "root" (foreverResv "net0" anyFlow 100)
  b3 <- reserveM "root" (foreverResv "net0" anyFlow 100)
  b4 <- reserveM "root" (foreverResv "net0" anyFlow 100)
  return (b1 && b2 && b3 && not b4)


frag3 = do 
  b1 <- frag2
  b2 <- createSpeakerM "adf"
  b3 <- newShareM "root" "net0" "adfShare" (Set.singleton "adf") anyFlow 
           (DiscreteLimit 150)
  b4 <- giveReferenceM "root" "adfShare" "adf"
  b5 <- reserveM "adf" (foreverResv "adfShare" anyFlow 100)
  return (b1 && b2 && b3 && b4 && b5)

test15 = runDNP $ do frag3

test16 = runDNP $ do
  b1 <- frag3
  b2 <- reserveM "root" (foreverResv "net0" anyFlow 100)
  return (b1 && b2)

test16a = runDNP $ do
  b1 <- frag3
  b2 <- reserveM "root" (foreverResv "adfShare" anyFlow 50)
  return (b1 && b2)

test16b = runDNP $ do
  b1 <- frag3
  b2 <- reserveM "root" (foreverResv "adfShare" anyFlow 51)
  return (b1 && not b2)

test17 = runDNP $ do
  b1 <- frag3
  b2 <- reserveM "root" (foreverResv "net0" anyFlow 101)
  return (b1 && not b2)

test18 = runDNP $ do
  b1 <- frag3
  b2 <- reserveM "adf" (foreverResv "adfShare" anyFlow 51)
  return (b1 && not b2)

frag4 = do
  b1 <- createSpeakerM "arjun"
  b2 <- newShareM "root" rootShareRef "hadoop-share" (Set.singleton "root") --ppl who can delegate
                    anyFlow (DiscreteLimit 100)
  b3 <- giveDefaultReferenceM "root" "hadoop-share"
  b4 <- reserveM "arjun" (foreverResv "hadoop-share" anyFlow 25)
  return (b1 && b2 && b3 && b4)

test19 = runDNP $ do frag4

test20 = runDNP $ do
  b1 <- frag4
  b2 <- createSpeakerM "adf"
  b3 <- reserveM "adf" (foreverResv "hadoop-share" anyFlow 25)
  return (b1 && b2 && b3)

arjunFlow = anyFlow { flowSend = Set.singleton "arjun" }

frag5 = do
  b1 <- createSpeakerM "arjun"
  b2 <- newShareM "root" rootShareRef "arjun-share" (Set.singleton "arjun")
             arjunFlow (DiscreteLimit 100)
  b3 <- giveReferenceM "root" "arjun-share" "arjun"
  b4 <- reserveM "arjun" (foreverResv "arjun-share" arjunFlow 50)
  s  <- currentReservationsM
  let b5 = Set.fromList s == Set.singleton (
                               foreverResv "arjun-share" arjunFlow 50)
  return (b1 && b2 && b3 && b4 && b5)

test21 = runDNP $ frag5


frag6 = do
  b1 <- frag5
  let arjunWebFlow = (arjunFlow { flowSrcPort = Set.singleton 80}) 
  b2 <- reserveM "arjun" (foreverResv "arjun-share" arjunWebFlow 50)
  s  <- currentReservationsM
  let b3 = Set.fromList s == Set.fromList [
                                   (foreverResv "arjun-share" arjunFlow  50),
                                   (foreverResv "arjun-share" arjunWebFlow 50)]
  return (b1 && b2 && b3)

test22 = runDNP frag6

test23 = runDNP $ do
  b1 <- frag5
  b2 <- reserveM "arjun" (foreverResv "arjun-share" anyFlow 50)
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
  ]

main :: IO ()
main = do
  runTestTT allTests
  return ()
