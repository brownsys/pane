module PriorityQueue
  ( PQ
  , empty
  , isEmpty
  , dequeue
  , dequeueWhile
  , enqueue
  , toList
  , peek
  , filter
  ) where

import Data.List (span)
import qualified Prelude
import Prelude hiding (filter)
import Data.Aeson

data PQ a = PQ { 
  leq :: a -> a -> Bool,
  items :: [a]
}

instance Show a => Show (PQ a) where
  show pq = show (items pq)

instance ToJSON a => ToJSON (PQ a) where
  toJSON pq = toJSON (items pq)

empty :: (a -> a -> Bool)
      -> PQ a
empty leq = PQ leq []

isEmpty :: PQ a
        -> Bool
isEmpty pq = null (items pq)

enqueue :: a
        -> PQ a
        -> PQ a
enqueue y (PQ leq xs) = PQ leq (ins xs)
  where ins [] = [y]
        ins (x:xs) | y `leq` x = y:x:xs
                   | otherwise = x:(ins xs)

dequeue :: PQ a
        -> Maybe (a, PQ a)
dequeue pq = case items pq of
  [] -> Nothing
  (x:xs) -> Just (x, pq { items = xs })

dequeueWhile :: (a -> Bool)
             -> PQ a 
             -> ([a], PQ a)
dequeueWhile f pq = 
  let (ret, keep) = span f (items pq) in
      (ret, pq { items = keep })

peek :: PQ a
     -> Maybe a
peek pq = case items pq of
  [] -> Nothing
  (x:_) -> Just x

toList :: PQ a
       -> [a]
toList = items

filter :: (a -> Bool)
       -> PQ a
       -> PQ a
filter f pq =
  pq { items = Prelude.filter f (items pq) }
