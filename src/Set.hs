module Set
  ( Set
  , all
  , isSubsetOf
  , empty
  , exists
  , insert
  , singleton
  , member
  , fromList
  , toList
  , intersection
  , union
  , null
  ) where

import Prelude hiding (all, null)
import qualified Data.Set

data Set a
  = FiniteSet (Data.Set.Set a)
  | All
  deriving (Eq, Show)

instance Ord a => Ord (Set a) where
  _ <= All = True
  (FiniteSet s1) <= (FiniteSet s2) = s1 <= s2
  All <= (FiniteSet _) = False

all :: Set a
all = All

member :: Ord a => a -> Set a -> Bool
member _ All = True
member x (FiniteSet s) = x `Data.Set.member` s

singleton :: a -> Set a
singleton x = FiniteSet (Data.Set.singleton x)

isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf (FiniteSet s1) (FiniteSet s2) = Data.Set.isSubsetOf s1 s2
isSubsetOf _ All = True
isSubsetOf All (FiniteSet _) = False

intersection :: Ord a => Set a -> Set a -> Set a
intersection (FiniteSet s1) (FiniteSet s2) = 
  FiniteSet (Data.Set.intersection s1 s2)
intersection (FiniteSet s1) All = (FiniteSet s1)
intersection All (FiniteSet s2) = (FiniteSet s2)
intersection All All = All

union :: Ord a => Set a -> Set a -> Set a
union (FiniteSet s1) (FiniteSet s2) = FiniteSet (Data.Set.union s1 s2)
union All            _              = All
union _              All            = All

empty :: Set a
empty = FiniteSet Data.Set.empty

exists :: Ord a => (a -> Bool) -> Set a -> Bool
exists f All = True
exists f (FiniteSet s) = any f (Data.Set.toList s)

insert :: Ord a => a -> Set a -> Set a
insert a (FiniteSet s) = FiniteSet (Data.Set.insert a s)
insert a All = All

fromList lst = (FiniteSet (Data.Set.fromList lst))

toList All = Nothing
toList (FiniteSet s) = Just (Data.Set.toList s)

null :: Set a -> Bool
null All = False
null (FiniteSet s) = Data.Set.null s
