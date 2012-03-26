module Main where

import Parser (paneMan)
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
import Nettle.Ethernet.EthernetAddress (ethernetAddress64)
import qualified Nettle.OpenFlow as OF
import qualified Set
import qualified Data.Set as S
import qualified OFCompiler as OFC
import qualified NIB as NIB
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified MacLearning as ML
import qualified Data.ByteString as BS
import Data.HList
import Data.Word
import CombinedPaneMac

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

ip_10_0_0_1 = fromJust (parseIPAddress "10.0.0.1")

ip_10_0_0_2 = fromJust (parseIPAddress "10.0.0.2")

flow0 = Flows.simple (Just ip_10_0_0_1) (Just 80) (Just ip_10_0_0_2) (Just 90)

flowHttpAll = Flows.simple Nothing Nothing Nothing (Just 80)

flowHttp1 = Flows.simple (Just ip_10_0_0_2) Nothing (Just ip_10_0_0_1) (Just 80)

testSingleResv = TestLabel "make single reservation" $ TestCase $ do
  let req = Req FC.rootShareRef flow0 0 10 (ReqResv 100) True
  case FC.request FC.rootSpeaker req FC.emptyState of
    Nothing -> assertFailure "should be able to request in rootShare"
    Just state -> do
      assertEqual "should have a single entry (GMB)"
                  (MatchTable [(flow0, Just (ReqResv 100, 10))])
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
            (MatchTable [(flowHttp1, Just (ReqDeny, 10)),
                         (flowHttpAll, Just (ReqAllow, 15))])
            (compileShareTree 1 (FC.getShareTree state)) 
          assertEqual "should have only the allow entry at t=11"
            (MatchTable [(flowHttpAll, Just (ReqAllow, 15))])
            (compileShareTree 11 (FC.getShareTree state))

testChildParentOverlap = TestLabel "make child/parent overlap" $ TestCase $ do
  let req1 = Req FC.rootShareRef flowHttp1 0 15 ReqDeny False
  let req2 = Req "net0" flowHttp1 0 10 ReqAllow False
  let share = FC.Share "net0" Flows.all (Set.singleton FC.rootSpeaker)
                       FC.emptyShareReq True True TG.unconstrained
  case FC.request FC.rootSpeaker req1 FC.emptyState of
    Nothing -> assertFailure "should be able to deny in in rootShare"
    Just s -> case FC.newShare FC.rootSpeaker FC.rootShareRef share s of
      Nothing -> assertFailure "should be able to create sub-share"
      Just s -> case FC.request FC.rootSpeaker req2 s of
        Nothing -> assertFailure "should be able to allow in sub-share"
        Just s -> do
          putStrLn (show (FC.getShareTree s))
          assertEqual "should have only allow (child overrides)"
            (MatchTable [(flowHttp1, Just (ReqAllow, 10))])
            (compileShareTree 1 (FC.getShareTree s)) 
          assertEqual "should have only deny entry at t=11"
            (MatchTable [(flowHttp1, Just (ReqDeny, 15))])
             (compileShareTree 11 (FC.getShareTree s)) 


shareSemanticsTests = TestLabel "share semantics tests" $ TestList
  [ testSingleResv
  , testOverlapInShare
  , testChildParentOverlap
  ]

testDeny1Switch = TestLabel "compile deny to 1 switch" $ TestCase $ do
  let req1 = Req FC.rootShareRef flowHttp1 0 15 ReqDeny False
  case FC.request FC.rootSpeaker req1 FC.emptyState of
    Nothing -> assertFailure "should be able to deny in rootShare"
    Just state -> do
      let tbl = compileShareTree 0 (FC.getShareTree state)
      putStrLn "testCompile1"
      nib1 <- mkNib1
      cfg <- OFC.compile nib1 tbl
      case Map.toList cfg of
        [(0, NIB.Switch _ tbl)] ->
          assertEqual "expected deny rule"
            [(65535, Flows.toMatch' flowHttp1, [], 15)]
            (S.toList tbl)
        x -> assertFailure $ "should see a single switch, got " ++ show x

testResv1Switch = TestLabel "compile Resv to 1 switch" $ TestCase $ do
  let req1 = Req FC.rootShareRef flowHttp1 0 15 (ReqResv 200) True
  case FC.request FC.rootSpeaker req1 FC.emptyState of
    Nothing -> assertFailure "should be able to resv in rootShare"
    Just state -> do
      let tbl = compileShareTree 0 (FC.getShareTree state)
      putStrLn "testCompile1"
      putStrLn (show tbl)
      nib1 <- mkNib1
      cfg <- OFC.compile nib1 tbl
      case Map.toList cfg of
        [(0, NIB.Switch _ tbl)] ->
          assertEqual "should resv flowHttp1" 
            [(65535, Flows.toMatch' flowHttp1, [OF.Enqueue 0 0], 15)]
            (S.toList tbl)
        x -> assertFailure $ "should see a single switch, got " ++ show x

testDeny2Switch = TestLabel "compile deny to 2 switches" $ TestCase $ do
  let req1 = Req FC.rootShareRef flowHttp1 0 15 ReqDeny False
  case FC.request FC.rootSpeaker req1 FC.emptyState of
    Nothing -> assertFailure "should be able to deny in rootShare"
    Just state -> do
      let tbl = compileShareTree 0 (FC.getShareTree state)
      putStrLn "----------- testDeny2Switch ---------------"
      putStrLn (show tbl)
      nib2 <- mkNib2
      cfg <- OFC.compile nib2 tbl
      case Map.toList cfg of
        [(0, NIB.Switch _ tbl0), (1, NIB.Switch _ tbl1)] -> do
          assertEqual "should have empty table at switch 0" [] (S.toList tbl0)
          assertEqual "should have 1 deny in switch 1"
            [(65535, Flows.toMatch' flowHttp1, [], 15)]
            (S.toList tbl1)
        x -> assertFailure $ "should see two switches, got " ++ show x

testResv2Switch = TestLabel "compile Resv to 2 switches" $ TestCase $ do
  let req1 = Req FC.rootShareRef flowHttp1 0 15 (ReqResv 200) True
  case FC.request FC.rootSpeaker req1 FC.emptyState of
    Nothing -> assertFailure "should be able to resv in rootShare"
    Just state -> do
      let tbl = compileShareTree 0 (FC.getShareTree state)
      putStrLn "---------- testResv2Switch ------------"
      nib2 <- mkNib2
      cfg <- OFC.compile nib2 tbl
      case Map.toList cfg of
        [(0, NIB.Switch _ tbl0), (1, NIB.Switch _ tbl1)] -> do
          assertEqual "should have queue on switch 0"
            [(65535, Flows.toMatch' flowHttp1, [OF.Enqueue 0 0], 15)]
            (S.toList tbl0)
          assertEqual "should have queue on switch 1"
            [(65535, Flows.toMatch' flowHttp1, [OF.Enqueue 1 0], 15)]
            (S.toList tbl1)
        x -> assertFailure $ "should see two switches, got " ++ show x

      
compileWithNIBTests = TestLabel "compile with NIB tests" $ TestList
  [ testDeny1Switch
  , testResv1Switch
  , testDeny2Switch
  , testResv2Switch
  ]

mkNib1 = do
  ch <- newChan
  nib <- NIB.newEmptyNIB ch
  (Just sw1) <- NIB.addSwitch 0 nib
  (Just ep1) <- NIB.addEndpoint (ethernetAddress64 1111) ip_10_0_0_1 nib
  (Just ep2) <- NIB.addEndpoint (ethernetAddress64 2222) ip_10_0_0_2 nib
  (Just p0) <- NIB.addPort 0 sw1
  (Just p1) <- NIB.addPort 1 sw1
  True <- NIB.linkPorts p0 (NIB.endpointPort ep1)
  True <- NIB.linkPorts p1 (NIB.endpointPort ep2)
  return nib

mkNib2 = do
  ch <- newChan
  nib <- NIB.newEmptyNIB ch
  (Just sw1) <- NIB.addSwitch 0 nib
  (Just sw2) <- NIB.addSwitch 1 nib
  (Just ep1) <- NIB.addEndpoint (ethernetAddress64 1111) ip_10_0_0_1 nib
  (Just ep2) <- NIB.addEndpoint (ethernetAddress64 2222) ip_10_0_0_2 nib
  (Just p00) <- NIB.addPort 0 sw1
  (Just p01) <- NIB.addPort 1 sw1
  (Just p10) <- NIB.addPort 0 sw2
  (Just p11) <- NIB.addPort 1 sw2
  True <- NIB.linkPorts p00 (NIB.endpointPort ep1)
  True <- NIB.linkPorts p10 (NIB.endpointPort ep2)
  True <- NIB.linkPorts p01 p11
  return nib

testNib1Path = TestLabel "should find singleton path" $ TestCase $ do
  nib <- mkNib1 
  p <- NIB.getPath (ethernetAddress64 1111) (ethernetAddress64 2222) nib
  assertEqual "paths should be equal" [(0, 0, 1)] p

testNib2Path = TestLabel "should find two-switch path" $ TestCase $ do
  nib <- mkNib2
  p <- NIB.getPath (ethernetAddress64 1111) (ethernetAddress64 2222) nib
  assertEqual "paths should be equal" [(0, 0, 1), (1, 1, 0)] p

nibTests = TestLabel "NIB tests" $ TestList
  [ testNib1Path
  , testNib2Path
  ]

packet :: Word64 -- ^ src MAC
       -> Word64 -- ^ dst MAC
       -> OF.PortID -- ^ in port
       -> OF.PacketInfo
packet srcMAC dstMAC inPort = 
  let hdr = OF.EthernetHeader (ethernetAddress64 dstMAC)
                              (ethernetAddress64 srcMAC) 0x0800
      frame = HCons hdr (HCons (OF.UninterpretedEthernetBody BS.empty) HNil)
    in OF.PacketInfo Nothing 1500 inPort OF.NotMatched BS.empty (Right frame)


testMacLearn1 = TestLabel "should learn route " $ TestCase $ do
  swChan <- newChan
  pktChan <- newChan
  (tblChan, _) <- ML.macLearning swChan pktChan
  writeChan swChan (55, True)
  writeChan swChan (34, True)
  b <- isEmptyChan tblChan
  assertEqual "switches should not trigger table updates" True b
  writeChan pktChan (99 {- txID -}, 0, 34, packet 100 200 1)
  tbl <- readChan tblChan
  assertEqual "should learn a flood and 1 route"
    (MatchTable [ML.rule 0 34 (ethernetAddress64 100) (Just (1, 60)), 
                 ML.rule 0 34 (ethernetAddress64 200) Nothing]) tbl 
  writeChan pktChan (98 {- txID -}, 0, 34, packet 200 100 2)
  tbl <- readChan tblChan
  assertEqual "should learn two routes"
    (MatchTable [ML.rule 0 34 (ethernetAddress64 100) (Just (1, 60)), 
                 ML.rule 0 34 (ethernetAddress64 200) (Just (2, 60))]) tbl 



macLearningTests = TestLabel "MAC Learning tests" $ TestList
  [ testMacLearn1
  ]

mkPaneMan = do
  req <- newChan
  time <- newChan
  (tbl, resp) <- paneMan req time
  return (tbl, resp, req, time)

assertReadChanEqual msg val chan = do
  val' <- readChan chan
  assertEqual msg val val'

testAdmComm1 = TestLabel "overlapping non-strict admission control decisions \
  \ should succeed" $ TestCase $ do
  (tbl, resp, req, _) <- mkPaneMan
  writeChan req ("root", "allow(user=adf) on rootShare.")
  assertReadChanEqual "allow should succeed" ("root", "True") resp
  writeChan req ("root", "deny(user=adf, dstHost=10.200.0.1) on rootShare.")
  assertReadChanEqual "deny should also succeed" ("root", "True") resp
  -- Flip order of requests
  (tbl, resp', req, _) <- mkPaneMan
  writeChan req ("root", "deny(user=adf, dstHost=10.200.0.1) on rootShare.")
  assertReadChanEqual "deny also succeed" ("root", "True") resp
  writeChan req ("root", "allow(user=adf) on rootShare.")
  assertReadChanEqual "allow should also succeed" ("root", "True") resp

testAdmComm2 = TestLabel "strict should comm" $ TestCase $ do
  (tbl, resp, req, time) <- mkPaneMan
  writeChan req ("root", "deny(user=adf, dstHost=10.200.0.1) on rootShare.")
  assertReadChanEqual "deny should succeed" ("root", "True") resp
  writeChan req ("root", "allow(user=adf) strict on rootShare.")
  assertReadChanEqual "strict allow should fail" ("root", "True") resp
  -- Flip order of requests
  (tbl, resp, req, time) <- mkPaneMan
  writeChan req ("root", "allow(user=adf) strict on rootShare.")
  assertReadChanEqual "allow should succeed" ("root", "True") resp
  writeChan req ("root", "deny(user=adf, dstHost=10.200.0.1) on rootShare.")
  assertReadChanEqual "deny should fail (breaks allow)" ("root", "False") resp


testPane10 = TestLabel "root should be able to create sub-share" $ TestCase $ do
  (tbl, resp, req, time) <- mkPaneMan
  writeChan req ("root", "NewShare net0 for (*) [reserve <= 200] on rootShare.")
  assertReadChanEqual "root should be able create sub-share" 
    ("root", "True") resp

testPane24 = TestLabel "token graphs should work" $ TestCase $ do
  (tbl, resp, req, time) <- mkPaneMan
  writeChan req ("root", "NewShare net0 for (*) [reserve <= 200] on rootShare.")
  assertReadChanEqual "root should be able to create net0" ("root", "True") resp
  writeChan req ("root", "reserve(*) = 10 on net0 from 0 to 10.")
  assertReadChanEqual "root should be able to reserve" ("root", "True") resp
  writeChan time 0
  let resvTbl = MatchTable [(Flows.all, Just (ReqResv 10, 10))]
  assertReadChanEqual "compiled table should have a reservation" resvTbl tbl
  writeChan req ("root", "reserve(*) = 191 on net0.")
  assertReadChanEqual "root should be able to reserve" ("root", "False") resp
  -- Tell root no, and ensure that table is empty!
  writeChan time 5
  writeChan req ("root", "reserve(*) = 191 on net0.")
  assertReadChanEqual "reservation should fail at t=5" ("root", "False") resp
  assertReadChanEqual "compiled table should be the same" resvTbl tbl
  writeChan time 7
  writeChan req ("root", "reserve(*) = 191 on net0.")
  assertReadChanEqual "reservation should fail at t=7" ("root", "False") resp
  assertReadChanEqual "compiled table should be the same" resvTbl tbl
  writeChan time 11
  writeChan req ("root", "reserve(*) = 200 on net0.")
  assertReadChanEqual "reservation should pass at t=11" ("root", "True") resp
  assertReadChanEqual "table should be empty until next tick" emptyTable tbl
  writeChan time 11
  assertReadChanEqual "compiled table should have resv of 200"
    (MatchTable [(Flows.all, Just (ReqResv 200, NoLimit))]) tbl  

paneTests = TestLabel "Test PANE manager" $ TestList
  [ testPane10
  , testPane24
  , testAdmComm1
  , testAdmComm2
  ]

mkPaneWithMacLearning = do
  switch <- newChan
  packet <- newChan
  paneReq <- newChan
  time <- newChan
  (tbl, paneResp, _) <- combinedPaneMac switch packet paneReq time
  return (tbl, paneResp, switch, packet, paneReq, time)

testPaneMac0 = TestLabel "test PANE overriding MAC learning" $ TestCase $ do
  (tbl, resp, switch, pkt, req, time) <- mkPaneWithMacLearning
  writeChan switch (55, True)
  writeChan switch (34, True)
  writeChan pkt (977 {- txID -}, 0, 34, packet 0xbb 0xfe 1)
  let ethbb = ethernetAddress64 0xbb
  assertReadChanEqual "should learn a flood and 1 route"
    (MatchTable [ML.rule 0 34 ethbb (Just (1, 60)), 
                 ML.rule 0 34 (ethernetAddress64 0xfe) Nothing]) 
    tbl 
  writeChan pkt (988 {- txID -}, 0, 34, packet 0xfe 0xbb 2)
  assertReadChanEqual "should learn two routes"
    (MatchTable [ML.rule 0 34 (ethernetAddress64 0xbb) (Just (1, 60)), 
                 ML.rule 0 34 (ethernetAddress64 0xfe) (Just (2, 60))])
    tbl 
  writeChan req ("root", "deny(dstEth=00:00:00:00:00:bb) on rootShare.")
  assertReadChanEqual "root should be able to deny" ("root", "True") resp
  writeChan time 0 -- TODO(arjun): paneMan only writes on ticks. problem?
  assertReadChanEqual "deny should override MAC learning"
    (MatchTable [
      (Flows.fromSwitchMatch 34 (OF.matchAny { OF.dstEthAddress = Just ethbb }),
       Just (ReqDeny, NoLimit)),
      (Flows.fromMatch (OF.matchAny { OF.dstEthAddress = Just ethbb }),
       Just (ReqDeny, NoLimit)),
      ML.rule 0 34 (ethernetAddress64 0xfe) (Just (2, 60))])
    tbl 
  
  


paneMacTests = TestLabel "Test PANE with Mac learning" $ TestList
  [ testPaneMac0
  ]

allTests = TestList
  [ tokenGraphTests
  , shareSemanticsTests
  , compileWithNIBTests
  , nibTests
  , macLearningTests
  , paneTests
  , paneMacTests
  ]

main = do
  putStrLn "Starting unit tests..."
  (results, _) <- runTestText (putTextToHandle stderr False) allTests
  if errors results > 0 || failures results > 0 
    then exitFailure
    else putStrLn "All tests passed."
