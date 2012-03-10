module Main where

import Prelude hiding (putStrLn)
import Test.HUnit
import qualified TokenGraph as TG
import TokenGraph (TokenGraph)
import System.Exit
import System.IO (stderr, hPutStrLn)
import Base (Limit (..))



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
      assertEqual "should not be able to drain from t=1" 
                  Nothing (TG.drain 0 NoLimit 1 g)
      assertEqual "should not be able to drain from t=5" 
                  Nothing (TG.drain 5 NoLimit 1 g)
      case TG.drain 10 NoLimit 1 g of
        Nothing -> assertFailure "should be able to drain from t=10" 
        Just _  -> return ()

allTests = TestList
  [ testFill
  , testDrainForever
  , testDrainBurst
  , testMinDrain1
  , testMinDrain2
  , testMaxDrain1
  , testRegress0
  ]

main = do
  putStrLn "Starting unit tests..."
  (results, _) <- runTestText (putTextToHandle stderr False) allTests
  if errors results > 0 || failures results > 0 
    then exitFailure
    else putStrLn "All tests passed."
