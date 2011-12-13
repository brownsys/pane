module TokenBucket
  ( TokenBucket
  , unlimited
  , new
  , tick
  , consume
  , currTokens
  ) where

import Base

data TokenBucket = TokenBucket {
  currTokens :: Limit,     -- ^current capacity
  maxTokens     :: Limit,  -- ^'currTokens <= maxTokens'
  mintRate      :: Integer -- ^number of tokens inserted per tick
} deriving (Show)

unlimited :: TokenBucket
unlimited = TokenBucket NoLimit NoLimit 0


new :: Limit -> Limit -> Integer -> TokenBucket
new init max rate | init <= max = TokenBucket init max rate
                  | otherwise   = error "requires init <= max"

tick :: Integer -> TokenBucket -> TokenBucket
tick t bucket@(TokenBucket curr lim rate) = bucket { currTokens = curr' }
  where curr' = min lim (curr `addLimits` (DiscreteLimit (t * rate)))
                -- the expression above must handle cases where curr and lim are
                -- unbounded. Note that since curr <= lim, we never have curr
                -- unbounded with lim bounded.

consume :: Integer -> TokenBucket -> Maybe TokenBucket
consume toks bucket@(TokenBucket { currTokens = curr }) = 
  case DiscreteLimit toks <= curr of
    True  -> Just (bucket { currTokens = curr `addLimits` (DiscreteLimit (-toks)) })
    False ->  Nothing


