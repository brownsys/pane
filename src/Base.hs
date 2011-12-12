module Base where

data Limit 
  = NoLimit 
  | DiscreteLimit Integer 
  deriving (Eq, Show)

instance Ord Limit where
  _ <= NoLimit = True
  (DiscreteLimit m) <= (DiscreteLimit n) = m <= n
  NoLimit <= (DiscreteLimit _) = False
