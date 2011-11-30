module UnitTests where

import Test.HUnit
import Rep

traceTest :: String -> [Bool] -> DnpM () -> Test
traceTest msg trace commands = msg ~: (trace ~=? (traceDnp commands))

test1 = traceTest "creating principals" 
  [True, False, False, False, True, True] $ do 
  newPrinM "arjun" "root"
  newPrinM "arjun" "root" -- already exists
  newPrinM "root" "arjun" -- already exists
  newPrinM "adf" "fake" -- does not exist
  newPrinM "adf" "root"
  newPrinM "daisy" "adf"

test2Cmds = do
  newPrinM "arjun" "root"
  allowBwResM "root" "arjun" 300 -- error: root has no capacity
  allowBwResM "root" "root" 5000
  bwResM "root" 3000
  bwResM "arjun" 2000 -- cannot reserve yet

test2 = traceTest "reservations" 
  [ True, False, True, True, False ] test2Cmds

allTests = TestList [ test1, test2 ]

main :: IO ()
main = do
  runTestTT allTests
  return ()
