Require Import Coq.Init.Datatypes.
Require Import Coq.Lists.List.
Require Import Coq.Arith.MinMax.
Require Import Coq.Classes.Equivalence.
Require Import Omega.
Require Import CpdtTactics.

Open Local Scope equiv_scope.

Module Type ACTION.

  Parameter A : Type.

  Parameter None : A.

  Parameter equiv : A -> A -> Prop.

  Declare Instance A_Equivalence : Equivalence equiv.

  Definition well_behaved (f : A -> A -> A) : Prop :=
    (forall (a : A), f a None === a) /\
    (forall (a : A), f None a === a) /\
    (forall a a' b b', a === a' -> b === b' -> f a b === f a' b').

End ACTION.
