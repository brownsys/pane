-- Code is law
module Rep where

import Data.Map (Map, (!))
import Data.Set (Set)
import Lattice (Lattice)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Lattice as L
import qualified Control.Monad.State as StateM

type Prin = String
type Flow = String
type Node = String

rootPrin = "root"


data State = State (Lattice Prin) (Map Prin Policy) deriving Show

data Policy = Policy {
  availableFlows :: Set Flow,
  bwAvailable :: Integer,
  bwReserved :: Integer,
  bwDescendantReserved :: Integer
} deriving (Show)

emptyPolicy :: Policy
emptyPolicy = Policy S.empty 0 0 0

emptyState = State (L.root rootPrin L.empty) (M.singleton rootPrin emptyPolicy)



newPrin :: Prin         -- ^new principal
        -> Prin         -- ^parent principal
        -> State
        -> Maybe State  -- ^fails if parent does not exist and principal exists
newPrin newPrin parentPrin (State lat pols) =
  case not (L.member newPrin lat) && (L.member parentPrin lat) of
    True -> Just (State lat' pols') where
      lat' = L.insert newPrin parentPrin lat
      pols' = M.insert newPrin emptyPolicy pols
    False -> Nothing



allowRes :: Prin
         -> Prin
         -> Integer
         -> State
         -> Maybe State
allowRes prin subject maxReservation (State lat pols) = 
  case L.lessThan subject prin lat || prin == subject of
    False -> Nothing -- prin does not supervise subject
    True -> 
      case bwAvailable (pols ! prin) >= maxReservation || prin == rootPrin of
        False -> Nothing -- prin does not have the bandwidth to give
        True -> Just (State lat pols') where
          pol' = (pols ! subject) { bwAvailable = maxReservation }
          pols' = M.insert subject pol' pols

reserve :: Prin
        -> Integer
        -> State
        -> Maybe State
reserve prin reserve (State lat pols) = 
  let prinPol = pols ! prin
      prinLimit = bwAvailable prinPol - bwDescendantReserved prinPol
      updateAncestors Nothing _  = Nothing
      updateAncestors (Just pols) prin = 
        let pol = pols ! prin
            limit = bwAvailable pol - bwDescendantReserved pol
            reserved' = bwDescendantReserved pol + reserve
          in case reserve <= limit of
            False -> Nothing
            True -> 
              Just (M.insert prin (pol { bwDescendantReserved = reserved' })
                             pols)
    in case reserve <= prinLimit of
      False -> Nothing
      True -> 
        let pols' = M.insert prin (prinPol { bwReserved = reserve }) pols
            (_:ancestors) = L.chain prin rootPrin lat
          in case foldl updateAncestors (Just pols') ancestors of
            Nothing -> Nothing
            Just pols' -> Just (State lat pols')


admitAllow :: Prin -- ^the speaking principal
           -> Prin -- ^the principal that can allow this flow
           -> Flow -- ^the flow that can be allowed
           -> State
           -> Maybe State
admitAllow prin subject flow (State lat pols) = 
  case L.lessThan subject prin lat of
    False -> Nothing
    True -> 
      let allowed pr = flow `S.member` (availableFlows (pols ! pr))
        in case all allowed (L.descendingChain rootPrin prin lat) of
             False -> Nothing
             True ->  Just (State lat pols)

allow :: Prin -- ^the principal that wants to allow
      -> Flow -- ^the flow to allow
      -> State
      -> Maybe State
allow prin flow (State lat pols) = Nothing

-- ----------------------------------------------------------------------------
--

type DnpM a = StateM.State ([Bool], State) a

inDnp :: (State -> Maybe State) -> DnpM ()
inDnp f = do
  (trace, state) <- StateM.get
  case f state of
    Nothing -> StateM.put (False:trace, state)
    Just state' -> StateM.put (True:trace, state')

newPrinM prin parentPrin = inDnp (newPrin prin parentPrin)

allowBwResM prin subject resLimit = inDnp (allowRes prin subject resLimit)

bwResM prin res = inDnp (reserve prin res)

traceDnp :: DnpM () -> [Bool]
traceDnp m = reverse $ fst (StateM.execState m ([], emptyState))


{-

type Env = Map Prin Constraint

data Constraint
  = AdmiAllow Prin Flow
  | AdmitWaypoint Prin Node
  | BoundReservation Integer Prin Integer
  | BoundRatelimit Integer Prin Integer


-- |A primitive policy is a '[Prim]'.
-- 
-- WF constraint: TODO
data Prim
  = Allow Flow
  | Deny Flow
  | Waypoint Flow Node
  | Ratelimit Flow Integer -- ^upper bound
  | Reservation Flow Integer -- ^lower bound

-- |Combines constraints in two environments, with the right-hand side
-- as the outer environment that is constrained by the left-hand side
-- environment.
combineRightBias :: Env -> Env -> Env

-- |Account for the reservation in the environment, tightening constraints
-- as necessary.
restrict :: [Prim] -> Env -> Env


flatten :: ([Prims], Env) -> Tree -> ([Prim], Env)
flatten (prims, env) (Tree prin children res env') = 
  foldl flatten 
    (res ++ prims, restrict res (combineRightBias env' env))
    children
  -} 
