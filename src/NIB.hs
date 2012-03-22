module NIB
  ( Network
  , Node
  , Switch (..)
  , Endpoint (..)
  , FlowTbl (..)
  , PortCfg (..)
  , Edge (..)
  , path
  , newQueue
  , switches
  , switchWithNPorts
  ) where

import Debug.Trace
import qualified Nettle.OpenFlow as OF
import ShareSemantics (MatchTable (..))
import Base
import qualified Nettle.OpenFlow as OF
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word16)

data Queue = Queue Word16 Limit deriving (Show, Eq)

data PortCfg = PortCfg (Map OF.QueueID Queue) deriving (Show, Eq)

type FlowTblEntry = (OF.Match, [OF.Action], OF.TimeOut)

type FlowTbl = [FlowTblEntry]

data Switch = Switch (Map OF.PortID PortCfg) FlowTbl deriving (Show, Eq)

data Endpoint = Endpoint OF.IPAddress OF.EthernetAddress deriving (Show, Eq)

data Edge
  = Inner OF.SwitchID OF.PortID OF.SwitchID OF.PortID
  | Leaf OF.IPAddress OF.SwitchID OF.PortID
  deriving (Show, Eq)

type Network = (Map OF.SwitchID Switch, [Endpoint], [Edge])

type Node = Either OF.IPAddress OF.SwitchID

switches :: Network -> Map OF.SwitchID Switch
switches (sws, _, _) = sws

neighbors :: Network -> Node -> [(Node, Edge)]
neighbors (_, _, edges) (Left ip) = map pick (filter pred edges)
  where pred (Leaf ip' _ _) = ip == ip'
        pred _              = False
        pick e@(Leaf _ switchID _) = (Right switchID, e)
        pick _                     = error "unexpected Inner in neighbors"
neighbors (_, _, edges) (Right sw) = map pick (filter pred edges)
  where pred (Leaf _ sw' _) = sw == sw'
        pred (Inner sw' _ sw'' _) = sw == sw' || sw == sw''
        pick e@(Leaf ip _ _)       = (Left ip, e)
        pick e@(Inner sw1 p1 sw2 p2) 
          | sw == sw1 = (Right sw2, e)
          | sw == sw2 = (Right sw1, (Inner sw2 p2 sw1 p1))
          | otherwise = error "bad edge in neighbors"

shortestPath :: Network -> Node -> Node -> Maybe [Edge]
shortestPath net src dst = bfs (src, []) [] Set.empty
  where bfs (node, path) fringe visited = case node == dst of
          True -> Just path
          False ->
            let fringe' = fringe ++ 
                  (filter (\(n,_) -> not (n `Set.member` visited))
                          (neighbors net node))
              in case fringe' of
                   ((next,edge):tl) -> bfs (next, path ++ [edge]) 
                                           tl (Set.insert node visited)
                   [] -> Nothing

-- |'path network flow' produces the path 'flow' would take through 'network'.
-- The returned path is a sequence of '(switchID, inPort, outPort)' tuples.
--
-- 'path' fails if 'flow' does not exactly specify its endpoints
path :: Network 
     -> OF.Match
     -> Maybe [(OF.SwitchID, OF.PortID, OF.PortID)]
path net (OF.Match{OF.srcIPAddress=(src,32), OF.dstIPAddress=(dst,32)}) =
  case shortestPath net (Left src) (Left dst) of
    Nothing -> Nothing
    Just edges -> trace ("Path is " ++ show edges) (Just (concisePath edges))
path _ _ = Nothing

concisePath ((Leaf _ _ p0):(Inner s1 p1 s2 p2):rest) =
  (s1, p0, p1):(concisePath ((Inner s1 p1 s2 p2):rest))
concisePath ((Inner _ _ _ inPort):(Inner s1 outPort s2 p2):rest) =
  (s1,inPort,outPort):(concisePath ((Inner s1 outPort s2 p2):rest))
concisePath [Leaf _ s p0, Leaf _ _ p1] = 
  [(s,p0,p1)]
concisePath [Inner _ _ s p1, Leaf _ _ p2] =
  [(s, p1, p2)]
concisePath p = error ("concisePath failed on " ++ show p)

-- |'unusedNum lst' returns the smallest positive number that is not in 'lst'.
-- Assumes that 'lst' is in ascending order.
unusedNum :: (Num a, Ord a) => [a] -> a
unusedNum lst = loop 0 lst
  where loop m [] = m
        loop m (n:ns) | m < n     = m
                      | m == n    = loop (m+1) ns
                      | otherwise = error "unusedNum : lst not ascending"

newQueue :: Map OF.PortID PortCfg -- ^ports
         -> OF.PortID             -- ^port to adjust
         -> Word16                -- ^queue GMB
         -> Limit                 -- ^queue lifetime
         -> (OF.QueueID, Map OF.PortID PortCfg) -- ^new configuration
newQueue ports portID gmb timeOut = (queueID, ports')
  where queueID = unusedNum (Map.keys queues)
        queues  = case Map.lookup portID ports of
                    Just (PortCfg q)  -> q
                    Nothing -> error "newQueue: bad portID"
        queues' = Map.insert queueID (Queue gmb timeOut) queues
        ports'  = Map.adjust (\(PortCfg queues) -> PortCfg queues') portID ports



switchWithNPorts :: Word16 -> Switch
switchWithNPorts n = 
  Switch (Map.fromList [(k, PortCfg Map.empty) | k <- [0 .. n-1]]) []