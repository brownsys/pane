module Lattice (Lattice, empty, root, insert, member, lessThan, chain,
  descendingChain) where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map ((!))


data Lattice a = 
  Lattice (Map a a)       -- ^parent relation
          (Map a (Set a)) -- ^transitive closure of the parent relation.

instance Show a => Show (Lattice a) where
  show (Lattice parent lt) = show parent

empty :: Lattice a
empty = Lattice Map.empty Map.empty

root :: Ord a
     => a
     -> Lattice a
     -> Lattice a
root e (Lattice parent lt) = 
  case e `Map.member` lt of
    False -> Lattice parent (Map.insert e Set.empty lt)
    True -> error "root already in lattice"

insert :: Ord a
       => a
       -> a
       -> Lattice a
       -> Lattice a
insert elt pElt (Lattice parent lt) =
  case not (elt `Map.member` lt) && pElt `Map.member` lt of
    True -> Lattice parent' lt' where
      parent' = Map.insert elt pElt parent
      lt' = Map.insert elt (Set.insert pElt (lt ! pElt)) lt
    False -> error "element already in lattice"

lessThan :: Ord a
         => a
         -> a
         -> Lattice a
         -> Bool
lessThan e p (Lattice parent lt) = 
  case (Map.member e lt) && (Map.member p lt) of
    True -> Set.member p (lt ! e)
    False -> error "element/parent not in lattice"

member :: Ord a
       => a
       -> Lattice a 
       -> Bool
member p (Lattice parent lt) = Map.member p lt


chain :: Ord a
      => a
      -> a
      -> Lattice a
      -> [a]
chain from to (Lattice parent lt) = f from where
  f elt = case Map.lookup elt parent of
    Nothing -> [elt]
    Just p -> elt : (f p)

descendingChain from to lat = reverse (chain from to lat)
