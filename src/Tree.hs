module Tree (Tree, root, insert, member, lessThan, lessThanOrEq, chain,
  descendingChain, depth, adjust, lookup, update, children) where

import Prelude hiding (lookup)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map ((!))


data Tree a b = 
  Tree (Map a a)       -- ^parent relation
       (Map a (Set a)) -- ^transitive closure of the parent relation
       (Map a (Set a)) -- ^child relationship
       (Map a b)       -- ^actual data
  deriving Show


root :: Ord a
     => a
     -> b
     -> Tree a b
root k v = Tree Map.empty (Map.singleton k Set.empty) (Map.singleton k Set.empty)
                (Map.singleton k v)

insert :: Ord a
       => a
       -> b
       -> a
       -> Tree a b
       -> Tree a b
insert elt v pElt (Tree parent lt children kvs) =
  case not (elt `Map.member` lt) && pElt `Map.member` lt of
    True -> Tree parent' lt' children'' (Map.insert elt v kvs) where
      parent' = Map.insert elt pElt parent
      lt' = Map.insert elt (Set.insert pElt (lt ! pElt)) lt
      children' = Map.insert pElt (Set.insert elt (children ! pElt)) children
      children'' = Map.insert elt Set.empty children'
    False -> error "element already in lattice"

lessThan :: Ord a
         => a
         -> a
         -> Tree a b
         -> Bool
lessThan e p (Tree parent lt _ _) = 
  case (Map.member e lt) && (Map.member p lt) of
    True -> Set.member p (lt ! e)
    False -> error "element/parent not in lattice"

lessThanOrEq e p t = e == p || lessThan e p t

member :: Ord a
       => a
       -> Tree a b
       -> Bool
member p (Tree parent lt _ _) = Map.member p lt

chain :: Ord a
      => a
      -> Tree a b
      -> [(a, b)]
chain from (Tree parent lt _ kvs) = f from where
  f elt = case (Map.lookup elt parent, Map.lookup elt kvs) of
    (Nothing, Just v) -> [(elt, v)]
    (Just p, Just v) -> (elt, v) : (f p)
    (Just p, Nothing) -> error "ill-formed tree: parent, but no value"
    (Nothing, Nothing) -> error "element not in tree"

descendingChain from lat = reverse (chain from lat)

depth :: Ord a
      => a
      -> Tree a b
      -> Int
depth elt (Tree _ lt _ _) =
  case (Map.lookup elt lt) of
    Nothing -> error "element not in tree"
    Just x -> Set.size x

adjust :: Ord a
       => (b -> b)
       -> a
       -> Tree a b
       -> Tree a b
adjust f k (Tree m1 m2 m3 kvs) = Tree m1 m2 m3 (Map.adjust f k kvs)

lookup :: Ord a => a -> Tree a b -> b
lookup k (Tree _ _ _ kvs) = case Map.lookup k kvs of
  Nothing -> error "fail"
  Just v -> v

update :: Ord a => a -> b -> Tree a b -> Tree a b
update k v (Tree m1 m2 m3 kvs) = Tree m1 m2 m3 (Map.insert k v kvs)

children :: Ord a
         => a
         -> Tree a b
         -> [(a, b)]
children elt tr@(Tree parent _ chl kvs) = 
  case (Map.lookup elt chl) of
    Nothing -> []
    Just children -> map (\ x -> (x, lookup x tr)) (Set.toList children)
