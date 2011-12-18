module TokenBucket
  ( TokenBucket
  , unlimited
  , new
  , tickBy
  , updateRate
  , currTokens
  ) where

import Base

data TokenBucket = TokenBucket {
  currTokens :: Limit,      -- ^current capacity
  maxTokens     :: Limit,   -- ^'currTokens <= maxTokens'
  mintRate      :: Integer, -- ^number of tokens inserted per tick
  fillRate      :: Integer  -- ^number of tokens inserted per tick when no flows present
                            -- ^invariant: mintRate <= fillRate
} deriving (Show)

unlimited :: TokenBucket
unlimited = TokenBucket NoLimit NoLimit 0 0


new :: Limit -> Limit -> Integer -> TokenBucket
new init max rate | init <= max = TokenBucket init max rate rate
                  | otherwise   = error "requires init <= max"

tick :: Integer -> TokenBucket -> TokenBucket
tick t bucket@(TokenBucket curr lim mrate _) = bucket { currTokens = curr' }
  where curr' = min lim (curr `addLimits` (DiscreteLimit (t * mrate)))
                -- the expression above must handle cases where curr and lim are
                -- unbounded. Note that since curr <= lim, we never have curr
                -- unbounded with lim bounded.

-- Designed to allow simulating this bucket to infinite time
tickBy (DiscreteLimit l) b
  | l < 0     = error $ "tickBy (DiscreteLimit " ++ show l ++ ") _"
  | otherwise = tick l b
tickBy NoLimit bucket@(TokenBucket curr lim mrate _)
  | mrate  > 0 = bucket { currTokens = lim }
  | mrate == 0 = bucket -- currTokens will stay fixed
  | otherwise  = case curr == NoLimit of
                   True  -> bucket -- infinity minus infinity is still infinity
                   -- HACK to indicate bucket will be empty at forever:
                   False -> bucket { currTokens = (DiscreteLimit (-1)) }

-- Adjusts the mintRate by the specified amount, cannot exceed fillRate
updateRate :: Integer -> TokenBucket -> TokenBucket
updateRate r bucket@(TokenBucket _ _ mrate frate) = bucket { mintRate = mrate' }
  where mrate' = min frate (mrate + r)
