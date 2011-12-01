module Set
  ( Set
  , all
  , isSubsetOf
  , empty
  , exists
  , insert
  , singleton
  ) where

import Prelude hiding (all)
import qualified Data.Set


data Set a
  = FiniteSet (Data.Set.Set a)
  | All
  deriving Eq

instance Ord a => Ord (Set a) where
  _ <= All = True
  (FiniteSet s1) <= (FiniteSet s2) = s1 <= s2
  All <= (FiniteSet _) = False


all :: Set a
all = All

singleton :: a -> Set a
singleton x = FiniteSet (Data.Set.singleton x)

isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf (FiniteSet s1) (FiniteSet s2) = Data.Set.isSubsetOf s1 s2
isSubsetOf _ All = True
isSubsetOf All (FiniteSet _) = False

empty :: Set a
empty = FiniteSet Data.Set.empty

exists :: Ord a => (a -> Bool) -> Set a -> Bool
exists f All = True
exists f (FiniteSet s) = any f (Data.Set.toList s)

insert :: Ord a => a -> Set a -> Set a
insert a (FiniteSet s) = FiniteSet (Data.Set.insert a s)
insert a All = All
