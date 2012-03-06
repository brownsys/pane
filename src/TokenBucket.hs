module TokenBucket
  ( TokenBucket
  , unlimited
  , new
  , tickBy
  , updateRate
  , currTokens
  ) where

import Base
import Debug.Trace

data TokenBucket = TokenBucket {
  currTokens  :: Limit,   -- ^current capacity
  capacity    :: Limit,   -- ^'currTokens <= capacity'
  mintRate    :: Integer, -- ^number of tokens inserted per tick
  fillRate    :: Integer, -- ^number of tokens inserted per tick when no i
                          -- flows present
  minDrain    :: Integer,
  maxDrain    :: Limit
                            -- ^invariant: mintRate <= fillRate
} deriving (Show)

unlimited :: TokenBucket
unlimited = TokenBucket NoLimit NoLimit 0 0 0 NoLimit


new :: Limit -> Limit -> Integer -> TokenBucket
new init max rate | init <= max = TokenBucket init max rate rate 0 max
                  | otherwise   = error "requires init <= max"

-- |'tick' steps the 'TokenBucket' for 't' timesteps.
tick :: Integer -> TokenBucket -> Maybe TokenBucket
tick t bucket@(TokenBucket curr lim mrate _ _ _) =
  case min lim (curr `addLimits` (DiscreteLimit (t * mrate))) of
    DiscreteLimit n | n < 0 -> Nothing
    curr' -> Just (bucket { currTokens = curr' })

-- Designed to allow simulating this bucket to infinite time
tickBy :: Limit
       -> TokenBucket
       -> Maybe TokenBucket
tickBy (DiscreteLimit l) b
  | l < 0     = error $ "tickBy (DiscreteLimit " ++ show l ++ ") _"
  | otherwise = tick l b
tickBy NoLimit bucket@(TokenBucket curr lim mrate _ _ _)
  | mrate  > 0 = Just (bucket { currTokens = lim })
  | mrate == 0 = Just bucket -- currTokens will stay fixed
  | otherwise  = case curr == NoLimit of
      True  -> Just bucket -- infinity minus infinity is still infinity
      False -> trace "Nothing" $ Nothing

-- Adjusts the mintRate by the specified amount, cannot exceed fillRate
-- TODO(arjun): error cases? minDrain or maxDrain?
updateRate :: Integer -> TokenBucket -> TokenBucket
updateRate r bucket@(TokenBucket _ _ mrate frate _ _) = 
  bucket { mintRate = mrate' }
    where mrate' = min frate (mrate + r)
