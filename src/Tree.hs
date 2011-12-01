module Tree (Tree, root, insert, member, lessThan, lessThanOrEq, chain,
  descendingChain, adjust, lookup, update) where

import Prelude hiding (lookup)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map ((!))


data Tree a b = 
  Tree (Map a a)       -- ^parent relation
       (Map a (Set a)) -- ^transitive closure of the parent relation
       (Map a b)

instance Show a => Show (Tree a b) where
  show (Tree parent lt _) = show parent

root :: Ord a
     => a
     -> b
     -> Tree a b
root k v = Tree Map.empty (Map.singleton k Set.empty) (Map.singleton k v)

insert :: Ord a
       => a
       -> b
       -> a
       -> Tree a b
       -> Tree a b
insert elt v pElt (Tree parent lt kvs) =
  case not (elt `Map.member` lt) && pElt `Map.member` lt of
    True -> Tree parent' lt' (Map.insert elt v kvs) where
      parent' = Map.insert elt pElt parent
      lt' = Map.insert elt (Set.insert pElt (lt ! pElt)) lt
    False -> error "element already in lattice"

lessThan :: Ord a
         => a
         -> a
         -> Tree a b
         -> Bool
lessThan e p (Tree parent lt _) = 
  case (Map.member e lt) && (Map.member p lt) of
    True -> Set.member p (lt ! e)
    False -> error "element/parent not in lattice"

lessThanOrEq e p t = e == p || lessThan e p t

member :: Ord a
       => a
       -> Tree a b
       -> Bool
member p (Tree parent lt _) = Map.member p lt


chain :: Ord a
      => a
      -> a
      -> Tree a b
      -> [(a, b)]
chain from to (Tree parent lt kvs) = f from where
  f elt = case (Map.lookup elt parent, Map.lookup elt kvs) of
    (Nothing, Just v) -> [(elt, v)]
    (Just p, Just v) -> (elt, v) : (f p)

descendingChain from to lat = reverse (chain from to lat)

adjust :: Ord a
       => (b -> b)
       -> a
       -> Tree a b
       -> Tree a b
adjust f k (Tree m1 m2 kvs) = Tree m1 m2 (Map.adjust f k kvs)

lookup :: Ord a => a -> Tree a b -> b
lookup k (Tree _ _ kvs) = case Map.lookup k kvs of
  Nothing -> error "fail"
  Just v -> v

update :: Ord a => a -> b -> Tree a b -> Tree a b
update k v (Tree m1 m2 kvs) = Tree m1 m2 (Map.insert k v kvs)
