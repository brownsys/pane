module Main where

import Prelude hiding (putStrLn)
import Test.HUnit
import qualified TokenGraph as TG
import TokenGraph (TokenGraph)
import System.Exit
import System.IO (stderr, hPutStrLn)
import Base
import qualified FlowController as FC
import ShareSemantics
import qualified Flows
import Nettle.IPv4.IPAddress
import qualified Set

putStrLn = hPutStrLn stderr


testFill = TestLabel "should fill and not exceed capacity" $ TestCase $ do
  let g = TG.new 10 0 50 100
  putStrLn (show g)
  assertEqual "zero initial tokens" 0 (TG.tokensAt 0 g)
  assertEqual "should be below capacity at t=9" 90 (TG.tokensAt 9 g)
  assertEqual "should reach capacity at t=10" 100 (TG.tokensAt 10 g)
  assertEqual "should not exceed capacity at t=11" 100 (TG.tokensAt 11 g)
 
testDrainForever = TestLabel "can drain forever" $ TestCase $ do
  case TG.drain 0 NoLimit 10 (TG.new 10 0 50 100) of
    Nothing -> assertFailure "should be able to drain"
    Just g -> do
      putStrLn (show g)
      assertEqual "should have 0 tokens at t=0" 0 (TG.tokensAt 0 g)
      assertEqual "should have 0 tokens at t=100" 0 (TG.tokensAt 100 g)
      assertEqual "should have 0 tokens at t=2000" 0 (TG.tokensAt 2000 g)
      assertEqual "should have 0 tokens at t=inf" 0 (TG.tokensAt NoLimit g)

testDrainBurst = TestLabel "can spend accumulated tokens" $ TestCase $ do
  case TG.drain 2 22 11 (TG.new 10 0 50 100) of
    Nothing -> assertFailure "should be able to drain"
    Just g -> do
      putStrLn (show g)
      assertEqual "should have 20 tokens at t=2" 20 (TG.tokensAt 2 g)
      let check (toks, t) = do
            let msg = "should have " ++ show toks ++ " tokens at t=" ++ show t
            assertEqual msg toks (TG.tokensAt t g)
      mapM_ check (zip [19 .. 0] [3 .. ])
      mapM_ check (zip [10, 20 .. 100] [23 ..])

testMinDrain1 = TestLabel "slope cannot be < minDrain" $ TestCase $ do
  case TG.drain 1 5 1 (TG.new 10 2 50 100) of
    Nothing -> return ()
    Just g -> do
      putStrLn (show g)
      assertFailure "should not be able to drain"

testMinDrain2 = TestLabel "overlapping drains with a minDrain" $ TestCase $ do
  let g = TG.new 10 4 50 100
  case TG.drain 1 5 4 g of
    Nothing -> do
      putStrLn (show g)
      assertFailure "should be able to drain"
    Just g -> case TG.drain 3 7 2 g of
      Nothing -> return ()
      Just g -> do
      putStrLn (show g)
      assertFailure "second drain should fail"

testMaxDrain1 = TestLabel "slope cannot be > maxDrain" $ TestCase $ do
  let g = TG.new 10 4 10 100
  assertEqual "100 tokens at step 10" 100 (TG.tokensAt 10 g)
  case TG.drain 10 11 11 g of
    Nothing -> return ()
    Just _  -> assertFailure "should not be able to drain"

testRegress0 = TestLabel "regression test 0" $ TestCase $ do
  let g = TG.new 2000 0 2000 2000
  case TG.drain 0 10 2000 g of
    Nothing -> assertFailure "should be able to drain all tokens"
    Just g -> do
      putStrLn (show g)
      assertEqual "should not be able to drain from t=0" 
                  Nothing (TG.drain 0 NoLimit 1 g)
      assertEqual "should not be able to drain from t=5" 
                  Nothing (TG.drain 5 NoLimit 1 g)
      case TG.drain 10 NoLimit 1 g of
        Nothing -> assertFailure "should be able to drain from t=10" 
        Just _  -> return ()

testRegress1 = TestLabel "regression test 1" $ TestCase $ do
  let g = TG.unconstrained
  case TG.drain 0 10 2000 g of
    Nothing -> do
      putStrLn (show g)
      assertFailure "should be able to drain all tokens"
    Just _ -> return ()

testRegress2 = TestLabel "regression test 2" $ TestCase $ do
  let g = TG.new 200 NoLimit 200 200
  case TG.drain 5 10 10 g of
    Nothing -> assertFailure "should be able to drain (1)"
    Just g  -> case TG.drain 0 1 200 g of
      Nothing -> assertFailure "should be able to drain (2)"
      Just g  -> case TG.drain 4 5 200 g of
        Nothing -> do
          putStrLn (show g)
          assertFailure "should be able to drain (3)"
        Just _ -> return ()

tokenGraphTests = TestLabel "token graph tests" $ TestList
  [ testFill
  , testDrainForever
  , testDrainBurst
  , testMinDrain1
  , testMinDrain2
  , testMaxDrain1
  , testRegress0
  , testRegress1
  , testRegress2
  ]

flow0 = Flows.simple (parseIPAddress "132.161.10.1") (Just 80) 
                     (parseIPAddress "64.12.23.1") (Just 90)

flowHttpAll = Flows.simple Nothing Nothing Nothing (Just 80)

flowHttp1 = Flows.simple Nothing Nothing 
                         (parseIPAddress "10.0.0.1") (Just 80)

testSingleResv = TestLabel "make single reservation" $ TestCase $ do
  let req = Req FC.rootShareRef flow0 0 10 (ReqResv 100) True
  case FC.request FC.rootSpeaker req FC.emptyState of
    Nothing -> assertFailure "should be able to request in rootShare"
    Just state -> do
      assertEqual "should have a single entry (GMB)"
                  (MatchTable [(flow0, Action (Just (100, 10)) Nothing)])
                  (compileShareTree 0 (FC.getShareTree state)) 

testOverlapInShare = TestLabel "make overlap in share" $ TestCase $ do
  let req1 = Req FC.rootShareRef flowHttpAll 0 15 ReqAllow False
  let req2 = Req FC.rootShareRef flowHttp1 0 10 ReqDeny False
  assertEqual "should overlap"
              flowHttp1
              (Flows.intersection flowHttp1 flowHttpAll)
  assertEqual "should not be empty" False (Flows.null flowHttp1)
  case FC.request FC.rootSpeaker req1 FC.emptyState of
    Nothing -> assertFailure "should be able to allow all HTTP in rootShare"
    Just state -> do
      case FC.request FC.rootSpeaker req2 state of
        Nothing -> assertFailure "should be able to deny in rootShare"
        Just state -> do
          putStrLn (show (FC.getShareTree state))
          assertEqual "should have allow and deny entries"
            (MatchTable [(flowHttp1, Action Nothing (Just (Deny, 10))),
                         (flowHttpAll, Action Nothing (Just (Allow, 15)))])
            (compileShareTree 1 (FC.getShareTree state)) 
          assertEqual "should have only the allow entry at t=11"
            (MatchTable [(flowHttpAll, Action Nothing (Just (Allow, 15)))])
            (compileShareTree 11 (FC.getShareTree state))

testChildParentOverlap = TestLabel "make child/parent overlap" $ TestCase $ do
  let req1 = Req FC.rootShareRef flowHttp1 0 15 ReqDeny False
  let req2 = Req "net0" flowHttp1 0 10 ReqAllow False
  let share = FC.Share "net0" Flows.all (Set.singleton FC.rootSpeaker)
                       FC.emptyShareReq True True TG.unconstrained
  case FC.request FC.rootSpeaker req1 FC.emptyState of
    Nothing -> assertFailure "should be able to deny in in rootShare"
    Just s -> case FC.newShare FC.rootSpeaker FC.rootShareRef share s of
      Nothing -> putStrLn (show s) >> assertFailure "should be able to create sub-share"
      Just s -> case FC.request FC.rootSpeaker req2 s of
        Nothing -> assertFailure "should be able to allow in sub-share"
        Just s -> do
          putStrLn (show (FC.getShareTree s))
          assertEqual "should have only allow (child overrides)"
            (MatchTable [(flowHttp1, Action Nothing (Just (Allow, 10)))])
            (compileShareTree 1 (FC.getShareTree s)) 
          assertEqual "should have only deny entry at t=11"
            (MatchTable [(flowHttp1, Action Nothing (Just (Deny, 15)))])
            (compileShareTree 11 (FC.getShareTree s)) 


shareSemanticsTests = TestLabel "share semantics tests" $ TestList
  [ testSingleResv
  , testOverlapInShare
  , testChildParentOverlap
  ]

allTests = TestList
  [ tokenGraphTests
  , shareSemanticsTests
  ]

main = do
  putStrLn "Starting unit tests..."
  (results, _) <- runTestText (putTextToHandle stderr False) allTests
  if errors results > 0 || failures results > 0 
    then exitFailure
    else putStrLn "All tests passed."
