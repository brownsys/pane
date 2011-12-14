module UnitTests where

import Test.HUnit
import FlowController
import qualified Set
import FlowControllerLang
import Base
import qualified TokenBucket as TB
-- import EmitFML

instance AssertionPredicable (Maybe a) where
  assertionPredicate Nothing = return False
  assertionPredicate (Just _) = return True

unexpectedState (Just _) = Nothing
unexpectedState Nothing = Just emptyState

foreverReq shareRef flow rd =
 Req shareRef flow 0 NoLimit rd True

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
  let s = Share anyFlow (Set.singleton "root") emptyShareReq (DiscreteLimit 100)
            False False TB.unlimited
  b2 <- newShareM "root" rootShareRef "arjun-share" s
  b3 <- giveReferenceM "root" "arjun-share" "arjun"
  return (b1 && b2 && b3)

frag1 limitForAdf = do
  b1 <- createSpeakerM "arjun"
  b2 <- createSpeakerM "adf"
  let s = Share anyFlow (Set.singleton "root") emptyShareReq (DiscreteLimit 100)
            False False TB.unlimited
  b3 <- newShareM "root" rootShareRef "arjun-share" s 
  b4 <- giveReferenceM "root" "arjun-share" "arjun"
  let s' = Share anyFlow (Set.singleton "arjun") emptyShareReq (DiscreteLimit limitForAdf)
            False False TB.unlimited
  b5 <- newShareM "arjun" "arjun-share" "adf-share" s' 
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
  let s = Share anyFlow (Set.singleton "root") emptyShareReq (DiscreteLimit 200)
            True True TB.unlimited
  b1 <- newShareM "root" rootShareRef "net0" s
  return b1

test10 = evalDNP $ do frag2

test11 = evalDNP $ do
  b1 <- frag2 
  b2 <- requestM "root" (foreverReq "net0" anyFlow (ReqResv 300))
  return (b1 && not b2)

test12 = evalDNP $ do
  b1 <- frag2
  b2 <- requestM "root" (foreverReq rootShareRef anyFlow (ReqResv 300))
  return (b1 && b2)

test13 = evalDNP $ do
  b1 <- frag2
  b2 <- requestM "root" (foreverReq "net0" anyFlow (ReqResv 100))
  b3 <- requestM "root" (foreverReq "net0" anyFlow (ReqResv 100))
  return (b1 && b2 && b3)

test14 = evalDNP $ do
  b1 <- frag2
  b2 <- requestM "root" (foreverReq "net0" anyFlow (ReqResv 100))
  b3 <- requestM "root" (foreverReq "net0" anyFlow (ReqResv 100))
  b4 <- requestM "root" (foreverReq "net0" anyFlow (ReqResv 100))
  return (b1 && b2 && b3 && not b4)


frag3 = do 
  b1 <- frag2
  b2 <- createSpeakerM "adf"
  let s = Share anyFlow (Set.singleton "root") emptyShareReq (DiscreteLimit 150)
            False False TB.unlimited
  b3 <- newShareM "root" "net0" "adfShare" s
  b4 <- giveReferenceM "root" "adfShare" "adf"
  b5 <- requestM "adf" (foreverReq "adfShare" anyFlow (ReqResv 100))
  return (b1 && b2 && b3 && b4 && b5)

test15 = evalDNP $ do frag3

test16 = evalDNP $ do
  b1 <- frag3
  b2 <- requestM "root" (foreverReq "net0" anyFlow (ReqResv 100))
  return (b1 && b2)

test16a = evalDNP $ do
  b1 <- frag3
  b2 <- requestM "root" (foreverReq "adfShare" anyFlow (ReqResv 50))
  return (b1 && b2)

test16b = evalDNP $ do
  b1 <- frag3
  b2 <- requestM "root" (foreverReq "adfShare" anyFlow (ReqResv 51))
  return (b1 && not b2)

test17 = evalDNP $ do
  b1 <- frag3
  b2 <- requestM "root" (foreverReq "net0" anyFlow (ReqResv 101))
  return (b1 && not b2)

test18 = evalDNP $ do
  b1 <- frag3
  b2 <- requestM "adf" (foreverReq "adfShare" anyFlow (ReqResv 51))
  return (b1 && not b2)

frag4 = do
  b1 <- createSpeakerM "arjun"
  let s = Share anyFlow (Set.singleton "root") emptyShareReq (DiscreteLimit 100)
            False False TB.unlimited
  b2 <- newShareM "root" rootShareRef "hadoop-share" s
  b3 <- giveDefaultReferenceM "root" "hadoop-share"
  b4 <- requestM "arjun" (foreverReq "hadoop-share" anyFlow (ReqResv 25))
  return (b1 && b2 && b3 && b4)

test19 = evalDNP $ do frag4

test20 = evalDNP $ do
  b1 <- frag4
  b2 <- createSpeakerM "adf"
  b3 <- requestM "adf" (foreverReq "hadoop-share" anyFlow (ReqResv 25))
  return (b1 && b2 && b3)

arjunFlow = anyFlow { flowSend = Set.singleton "arjun" }

frag5 = do
  b1 <- createSpeakerM "arjun"
  let s = Share arjunFlow (Set.singleton "root") emptyShareReq (DiscreteLimit 100)
            False False TB.unlimited
  b2 <- newShareM "root" rootShareRef "arjun-share" s
  b3 <- giveReferenceM "root" "arjun-share" "arjun"
  b4 <- requestM "arjun" (foreverReq "arjun-share" arjunFlow (ReqResv 50))
  s  <- currentRequestsM
  let b5 = Set.fromList s == Set.singleton (
                               foreverReq "arjun-share" arjunFlow (ReqResv 50))
  return (b1 && b2 && b3 && b4 && b5)

test21 = evalDNP $ frag5


frag6 = do
  b1 <- frag5
  let arjunWebFlow = (arjunFlow { flowSrcPort = Set.singleton 80}) 
  b2 <- requestM "arjun" (foreverReq "arjun-share" arjunWebFlow (ReqResv 50))
  s  <- currentRequestsM
  let b3 = Set.fromList s == Set.fromList [
                           (foreverReq "arjun-share" arjunFlow  (ReqResv 50)),
                           (foreverReq "arjun-share" arjunWebFlow (ReqResv 50))]
  return (b1 && b2 && b3)

test22 = evalDNP frag6

test23 = evalDNP $ do
  b1 <- frag5
  b2 <- requestM "arjun" (foreverReq "arjun-share" anyFlow (ReqResv 50))
  return (b1 && not b2)

test24 = evalDNP $ do
  b1 <- frag2 -- root creates net0 share
  let req = Req rootShareRef anyFlow 0 (DiscreteLimit 10) (ReqResv 100) True
  b2 <- requestM rootSpeaker req
  s <- currentRequestsM
  let b3 = s == [req]
  tickM 5
  s <- currentRequestsM
  let b4 = s == [req]
  tickM 2
  s <- currentRequestsM
  let b5 = s == [req]
  tickM 4
  s <- currentRequestsM
  let b6 = s == []
  return (b1 && b2 && b3 && b4 && b5 && b6)

test25 = evalDNP $ do
  b1 <- frag2
  let req = Req rootShareRef anyFlow 5 (DiscreteLimit 10) (ReqResv 100) True
  b2 <- requestM rootSpeaker req
  s <- currentRequestsM
  let b3 = s == []
  tickM 4
  s <- currentRequestsM
  let b4 = s == []
  tickM 1
  s <- currentRequestsM
  let b5 = s == [req]
  tickM 4
  s <- currentRequestsM
  let b6 = s == [req]
  tickM 1
  s <- currentRequestsM
  let b7 = s == []
  return (b1 && b2 && b3 && b4 && b5 && b6 && b7)
  

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
