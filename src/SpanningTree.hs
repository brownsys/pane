module SpanningTree (
    spanningTreeTests
) where

import NIB2
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Ord as Ord
import Test.HUnit
import Data.List (intercalate)


-- two parts:
-- find spaning trees
-- implement spanning trees in network TODO(adf)
--
-- disconnected graph:
-- - needs multiple spanning trees  TODO(adf)

------------------------------------------------------------------------------
--
-- | Support types to return spanning trees and handle weighted paths.
--
------------------------------------------------------------------------------

--
-- Spanning tree with a root node, and a map from child nodes to their trees
--

data SpanningTree a = SpanningTree a (Map a (SpanningTree a))

singletonTree x = SpanningTree x Map.empty

instance Eq a => Eq (SpanningTree a) where
    SpanningTree root children == SpanningTree root' children' =
        (root, children) == (root', children')

instance Ord a => Ord (SpanningTree a) where
    SpanningTree root children `compare` SpanningTree root' children' =
        (root, children) `Ord.compare` (root', children')

instance Show a => Show (SpanningTree a) where
    show (SpanningTree root children) = "(" ++
            (intercalate " " (show root : map show (Map.elems children))) ++ ")"

--
-- Weighted paths
--

data WeightedPath a w = WeightedPath [a] w

instance (Eq w, Eq a) => Eq (WeightedPath a w) where
    WeightedPath path weight == WeightedPath path' weight' =
        path == path' && weight == weight'

-- compare weights first
instance (Ord w, Ord a) => Ord (WeightedPath a w) where
    WeightedPath path weight `compare` WeightedPath path' weight' =
        (weight, path) `Ord.compare` (weight', path')

------------------------------------------------------------------------------
--
-- | Returns a graph's spanning tree where each node in the tree is on the
-- minimum-cost path from the root node. Note that this is not the minimum
-- spanning tree; rather the one constructed by the Spanning Tree Protocol.
--
------------------------------------------------------------------------------

findSpanningTree :: (Ord a, Ord w, Num w) => a -- ^ Spanning tree's root node
                    -> (a -> [(a, w)]) -- ^ returns a node's neighbors & weights
                    -> SpanningTree a
findSpanningTree root neighbors =
    recFind (singletonTree root) startPaths (Set.singleton root) where
    recFind tree frontier explored =
        case Set.minView frontier of
            Nothing -> tree
            Just (WeightedPath path weight, frontier) ->
                if Set.member (last path) explored
                then recFind tree frontier explored
                else let explored' = Set.insert (last path) explored
                         frontier' = Set.union frontier (nextPaths path weight)
                         tree' = extendTree tree path in
                     recFind tree' frontier' explored'
    -- given a tree and a path, build a new tree that includes the path
    extendTree tree [] = tree
    extendTree tree@(SpanningTree root children) (x:xs) =
        let pathAsTree = extendTree (singletonTree x) xs
            children' = Map.insertWith (\_ tree -> extendTree tree xs)
                                       x pathAsTree children
        in SpanningTree root children'
    -- given a path, find the paths that include it but are one edge longer
    nextPaths [] _ = error ("internal error: findSpanningTree: " ++
                           "should not reach this case")
    nextPaths [x] orig_w =
        Set.fromList $ map (\(y, w) -> WeightedPath [x, y] (orig_w+w))
                           (neighbors x)
    nextPaths (x:xs) orig_w =
        Set.map (\(WeightedPath path w) -> WeightedPath (x:path) w)
                (nextPaths xs orig_w)
    -- construct start paths by hand as paths do not include root
    startPaths = Set.fromList $ map (\(x, w) -> WeightedPath [x] w)
                                    (neighbors root)

------------------------------------------------------------------------------
--
-- Helper functions to run spanning tree over our NIB
--
------------------------------------------------------------------------------

--
-- | Returns a switch to serve as the root of the spanning tree. We first
-- look for the switch attached to the gateway endpoint. If that search is
-- not successful, we return the lowest numbered switch.
--
getRootSwitch :: NIB -> Maybe Switch
getRootSwitch nib@(NIB sws _ gwip) =
  let firstSwitch = case Map.keys sws of
        []     -> Nothing
        (x:xs) -> Map.lookup x sws
  in case gwip of
    Nothing -> firstSwitch
    Just gwip -> case lookupIP nib gwip of
        [] -> firstSwitch
        (gw : _) -> case endpointLink gw of
            EndpointLink _ -> firstSwitch
            SwitchLink swid _ -> case Map.lookup swid sws of
                Nothing -> firstSwitch
                Just sw -> Just sw -- common case

------------------------------------------------------------------------------
--
-- UnitTests
--
-- Directed graphs for unit tests are represented by lists of triples of
-- the form: (vertex src, edge weight, vertex dest)
--
------------------------------------------------------------------------------

-- Requisite neighbor function. Returns the neighbors and their weights
-- for a given node in a unit test graph.
testNeighbors graph node =
    map (\ (x, w, y) -> (y, w)) (filter (\ (x, w, y) -> x == node) graph)

-- Helper to crete spanning tree from minimal syntax of the form:
-- "testTree <root node> [ <child testTree> ]"
testTree root childTrees =
    SpanningTree root (Map.fromList
        (map (\ tree@(SpanningTree child _) -> (child, tree)) childTrees))

-- Wrapper to test findSpanningTree
testGraph desc graph expST =
    assertEqual desc expST (findSpanningTree 0 (testNeighbors graph))

--
-- Actual unit test cases
--

testST0 = TestLabel "Basic ST functionality" $ TestCase $ do
    testGraph "two node ST"
        [(0, 2, 1)]
        (testTree 0 [testTree 1 []])

testST1 = TestLabel "ST in graph with cycle" $ TestCase $ do
    testGraph "three node ST"
        [(0, 10, 1), (1, 20, 2), (2, 30, 0)]
        (testTree 0 [testTree 1 [testTree 2 []]])

testST2 = TestLabel "actual ST" $ TestCase $
    testGraph "actual ST"
        [(0, 50, 1), (0, 20, 2), (2, 20, 1)]
        (testTree 0 [testTree 2 [testTree 1 []]])

testST3 = TestLabel "convoluted ST" $ TestCase $ do
    testGraph "convoluted ST"
        [(1, 10, 4), (4, 10, 1),
         (0, 10, 6), (6, 10, 6),
         (6, 40, 2), (2, 10, 0), (0, 20, 2),
         (2, 10, 3), (3, 10, 2), (0, 50, 3)]
        (testTree 0 [testTree 6 [], testTree 2 [testTree 3 []]])

-- Test that we didn't implement an MST algorithm. We want a tree with the
-- least cost path to each edge.
testST4 = TestLabel "don't use Prim's algorithm" $ TestCase $ do
    testGraph "don't give MST"
        [(0, 10, 1), (0, 30, 2),
         (1, 20, 3),
         (2, 10, 3)]
        (testTree 0 [testTree 1 [testTree 3 []], testTree 2 []])
    testGraph "again, don't give the MST"
        [(0, 20, 1), (0, 10, 2),
         (1, 20, 3),
         (2, 25, 3)]
        (testTree 0 [testTree 2 [testTree 3 []], testTree 1 []])

spanningTreeTests = TestList
  [ testST0
  , testST1
  , testST2
  , testST3
  , testST4
  ]
