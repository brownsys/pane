module Tree (Tree, root, insert, member, lessThan, lessThanOrEq, chain,
  descendingChain) where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map ((!))


data Tree a = 
  Tree (Map a a)       -- ^parent relation
       (Map a (Set a)) -- ^transitive closure of the parent relation.

instance Show a => Show (Tree a) where
  show (Tree parent lt) = show parent

root :: Ord a
     => a
     -> Tree a
root root = Tree Map.empty (Map.singleton root Set.empty)

insert :: Ord a
       => a
       -> a
       -> Tree a
       -> Tree a
insert elt pElt (Tree parent lt) =
  case not (elt `Map.member` lt) && pElt `Map.member` lt of
    True -> Tree parent' lt' where
      parent' = Map.insert elt pElt parent
      lt' = Map.insert elt (Set.insert pElt (lt ! pElt)) lt
    False -> error "element already in lattice"

lessThan :: Ord a
         => a
         -> a
         -> Tree a
         -> Bool
lessThan e p (Tree parent lt) = 
  case (Map.member e lt) && (Map.member p lt) of
    True -> Set.member p (lt ! e)
    False -> error "element/parent not in lattice"

lessThanOrEq e p t = e == p || lessThan e p t

member :: Ord a
       => a
       -> Tree a 
       -> Bool
member p (Tree parent lt) = Map.member p lt


chain :: Ord a
      => a
      -> a
      -> Tree a
      -> [a]
chain from to (Tree parent lt) = f from where
  f elt = case Map.lookup elt parent of
    Nothing -> [elt]
    Just p -> elt : (f p)

descendingChain from to lat = reverse (chain from to lat)

