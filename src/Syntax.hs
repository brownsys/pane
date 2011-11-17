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

type Prin
  = App String
  | User String
  | Network (Set Node)

data NumExpr =
  | Reservation Prin
  | Ratelimit Prin
  | Float Float
  | Jitter Prin
  | Latency Prin

data NumOp = LT | LEq | Eq

-- Root says, "Arjun gets less than 3mbps of bandwidth".
-- 
data BoolExpr =
  | And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  | True
  | False
  | NumPred NumExpr NumOp NumExpr
  | Allow Prin
  | Deny Prin
  | Waypoint Prin Node
  | Avoid Prin Node

data Stmt
  = Stmt { who :: Prin, about :: Prin, expr :: BoolExpr } 

















data Expr a
  = ENumber a Double
  | EString a String
  | EBool a Bool
  | EUndefined a
  | ENull a
  | ELambda a [Ident] (Expr a)
  | EObject a [(String, (Expr a))]
  | EId a Ident
  | EOp a Op [Expr a]
  | EApp a (Expr a) [Expr a]
  | ELet a [(Ident, (Expr a))] (Expr a)
  | ESetRef a (Expr a) (Expr a)
  | ERef a (Expr a)
  | EDeref a (Expr a)
  | EGetField a (Expr a) (Expr a)
  | EUpdateField a (Expr a) (Expr a) (Expr a)
  | EDeleteField a (Expr a) (Expr a)
  | ESeq a (Expr a) (Expr a)
  | EIf a (Expr a) (Expr a) (Expr a)
  | EWhile a (Expr a) (Expr a)
  | ELabel a Label (Expr a)
  | EBreak a Label (Expr a)
  | EThrow a (Expr a)
  | ECatch a (Expr a) (Expr a)
  | EFinally a (Expr a) (Expr a)
  -- |We use HOAS when possible so that we don't mess up bindings.  When
  -- pretty-printing, we unravel these to use conventional bindings.
  | ELet1 a (Expr a) (Ident -> (Expr a))
  | ELet2 a (Expr a) (Expr a) (Ident -> Ident -> (Expr a))
  | EEval a -- ^an expression that calls eval, or a related function.  If
            -- EEval becomes the active expression, our model immediately aborts.
  deriving (Show, Data, Typeable)

instance Show a => Show (Ident -> Expr a) where
    show a = "test"

instance Show a => Show (Ident -> Ident -> Expr a) where
    show a = "test"

-- Generic function to retrieve the value of type 'a' from a data structure of
-- type 'c a', if it exists.
--
label' :: (Typeable a, Data (c a)) => c a -> Maybe a
label' = gmapQl (<|>) Nothing (Nothing `mkQ` (Just :: a -> Maybe a))


-- Generic function to retrieve the value of type 'a' from a data structure of
-- type 'c a'.
--
-- Note that in order for this to be safe, each variant of the data type 'c a' 
-- must include a value of type 'a' as an immediate child. In the ANF syntax,
-- the 'Lit' variant of the 'Expr' data type violates this assumption.
-- 
label :: (Typeable a, Data (c a)) => c a -> a
label = fromJust . label'

-- 'label' specialized to Expr
--
exprLabel :: Data a => Expr a -> a
exprLabel = label



