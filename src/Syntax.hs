module Syntax where

import Data.Set (Set)

{-

  Root says: AddUser Arjun <: Root
  Root says: for Arjun, Allow *
  
  ^^ What is Reservation(Arjun) here?
  effectively, Reservation(Arjun) = 0 and Ratelimit(Arjun) = MAX

  

  Root says: for Arjun, Ratelimit(Arjun) <= 300
  Arjun says, for Arjun, Reservation(Arjun) >= 100    FAIL
 

  instead,
  Root says: for Arjun, Reservation(Arjun) <= 200
  Arjun says, for Arjun Reservation(Arjun) >= 100     OKAY

  
  Root says: for Arjun, Ratelimit(Arjun) <= 100
  Arjun says, for Arjun Reservation(Arjun) >= 200     FAIL, cannot exceed

  Root says, for Root Ratelimit(Root) >= 20000
    ^^^ means that Arjun cannot further limit users

  - Principals higher than you in the lattice may break their guarantees.
    E.g., root, the physical network, may explode and all the guarantees
    about physical bandwidth may be reduced. However, principals lower
    in the lattice cannot break your guarantees.
    



-}

type Node = String

type ShareName = String

data Prin
  = SrcPort Integer
  | DstPort Integer
  | SrcHost String
  | DstHost String
-- | App String ? TODO: apps could be strings which map to sets of port numbers
  | User String -- TODO: split as sending & receiving users? do we even know recv user?
  | Network String -- TODO: These will refer to sets of nodes
  | Flow String -- TODO: wtf??
  deriving (Show, Eq, Ord)

{-
data NumExpr
  = Reservation (Set Prin)
  | Ratelimit (Set Prin)
  | Number Integer
  | Jitter (Set Prin)
  | Latency (Set Prin)
  deriving (Show)

data NumOp = NumLT | NumLEq | NumEq | NumGT | NumGEq deriving (Show)
-}

-- Root says, "Arjun gets less than 3mbps of bandwidth".
-- 
{-
data BoolExpr
  = And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  | True
  | False
  | NumPred NumExpr NumOp NumExpr
  | Allow (Set Prin)
  | Deny (Set Prin)
  | Waypoint Prin Node
  | Avoid Prin Node
  deriving (Show)
-}

{-
data Stmt
  = Stmt { expr :: BoolExpr, share :: ShareName } 
  | AddUser Prin -- new user
  | AddNetwork Prin Prin
  | NewShare { name :: ShareName, spks :: (Set Prin), stmt :: Stmt }
  | GrantUse Prin ShareName
  deriving (Show)
-}
