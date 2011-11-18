module Syntax where

import Data.Set (Set)

{-

  Root says: NewUser Arjun <: Root
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

data Prin
  = App String
  | User String
  | Network (Set Node)
  | Flow String -- TODO: wtf??
  deriving (Show)

data NumExpr
  = Reservation Prin
  | Ratelimit Prin
  | Number Integer
  | Jitter Prin
  | Latency Prin
  deriving (Show)

data NumOp = NumLT | NumLEq | NumEq deriving (Show)

-- Root says, "Arjun gets less than 3mbps of bandwidth".
-- 
data BoolExpr
  = And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  | True
  | False
  | NumPred NumExpr NumOp NumExpr
  | Allow Prin
  | Deny Prin
  | Waypoint Prin Node
  | Avoid Prin Node
  deriving (Show)

data Stmt
  = Stmt { about :: Prin, expr :: BoolExpr } 
  | NewUser Prin Prin -- new user, parent
  deriving (Show)

