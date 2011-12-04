module PriorityQueue
  ( PQ
  , empty
  , isEmpty
  , dequeue
  , enqueue
  , toList
  ) where

data PQ a = PQ { 
  leq :: a -> a -> Bool,
  items :: [a]
}

instance Show a => Show (PQ a) where
  show pq = show (items pq)

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

toList :: PQ a
       -> [a]
toList = items


