module Tree (Tree, root, insert, member, lessThan, lessThanOrEq, chain,
  descendingChain, depth, adjust, lookup, update, children, expose) where

import Prelude hiding (lookup)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Tree

data Tree a b = 
  Tree a               -- ^root
       (Map a a)       -- ^parent relation
       (Map a (Set a)) -- ^transitive closure of the parent relation
       (Map a (Set a)) -- ^child relationship
       (Map a b)       -- ^actual data
  deriving Show

instance Ord a => Functor (Tree a) where
  fmap f (Tree r p tr child kvs) = Tree r p tr child (fmap f kvs)

-- |In the parent relation, the root does not appear as a key.
-- In the child relation, the leaves do not appear as keys.
invertMap :: (Ord a, Ord b)
          => Map a b         -- ^parent relation
          -> Map b [a]       -- ^child relation
invertMap origMap = Map.foldrWithKey f Map.empty origMap
  where f a b newMap = Map.insertWith (++) b [a] newMap

toTree :: Ord a
       => Map a [a]
       -> Map a b
       -> a
       -> Data.Tree.Tree b
toTree childMap kvs node = case Map.lookup node kvs of
  Nothing  -> error "missing value"
  Just val -> Data.Tree.Node val children
    where children = case Map.lookup node childMap of
            Nothing  -> []
            Just lst -> map (toTree childMap kvs) lst
  
expose :: Ord a => Tree a b -> Data.Tree.Tree b
expose tree@(Tree root parent _ _ kvs) = toTree (invertMap parent) kvs root

root :: Ord a
     => a
     -> b
     -> Tree a b
root k v = Tree k Map.empty (Map.singleton k Set.empty) (Map.singleton k Set.empty)
                (Map.singleton k v)

insert :: Ord a
       => a
       -> b
       -> a
       -> Tree a b
       -> Tree a b
insert elt v pElt (Tree root parent lt children kvs) =
  case not (elt `Map.member` lt) && pElt `Map.member` lt of
    True -> Tree root parent' lt' children'' (Map.insert elt v kvs) where
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
lessThan e p (Tree _ parent lt _ _) = 
  case (Map.member e lt) && (Map.member p lt) of
    True -> Set.member p (lt ! e)
    False -> error "element/parent not in lattice"

lessThanOrEq e p t = e == p || lessThan e p t

member :: Ord a
       => a
       -> Tree a b
       -> Bool
member p (Tree _ parent lt _ _) = Map.member p lt

chain :: Ord a
      => a
      -> Tree a b
      -> [(a, b)]
chain from (Tree _ parent lt _ kvs) = f from where
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
depth elt (Tree _ _ lt _ _) =
  case (Map.lookup elt lt) of
    Nothing -> error "element not in tree"
    Just x -> Set.size x

adjust :: Ord a
       => (b -> b)
       -> a
       -> Tree a b
       -> Tree a b
adjust f k (Tree m0 m1 m2 m3 kvs) = Tree m0 m1 m2 m3 (Map.adjust f k kvs)

lookup :: Ord a => a -> Tree a b -> b
lookup k (Tree _ _ _ _ kvs) = case Map.lookup k kvs of
  Nothing -> error "fail"
  Just v -> v

update :: Ord a => a -> b -> Tree a b -> Tree a b
update k v (Tree m0 m1 m2 m3 kvs) = Tree m0 m1 m2 m3 (Map.insert k v kvs)

children :: Ord a
         => a
         -> Tree a b
         -> [(a, b)]
children elt tr@(Tree _ parent _ chl kvs) = 
  case (Map.lookup elt chl) of
    Nothing -> []
    Just children -> map (\ x -> (x, lookup x tr)) (Set.toList children)
