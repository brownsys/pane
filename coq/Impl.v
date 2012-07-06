Require Import Coq.Init.Datatypes.
Require Import Coq.Lists.List.
Require Import Coq.Arith.MinMax.
Require Import Coq.Classes.Equivalence.
Require Import Omega.
Require Import CpdtTactics.

Set Implicit Arguments.
Open Local Scope equiv_scope.

Definition port := nat.

Module Type IMPL.

  Parameter A : Type.
  Parameter pkt : Set.
  Parameter pat : Set.

  Parameter ActionUnit: A.
  Parameter equiv : A -> A -> Prop.

  Declare Instance A_Equivalence : Equivalence equiv.

  Axiom pkt_eq_dec : forall (m1 m2 : pkt), { m1 = m2 } + { m1 <> m2 }.
  Axiom pat_eq_dec : forall (m1 m2 : pat), { m1 = m2 } + { m1 <> m2 }.

  Parameter intersect : pat -> pat -> pat.
  Parameter is_overlapped : pat -> pat -> bool.
  Parameter is_match : pkt -> port -> pat -> bool.
  Parameter exact_match : pkt -> port -> pat.

  Axiom no_match_subset_l : forall k n m1 m2, 
    is_match k n m1 = false ->
    is_match k n (intersect m1 m2) = false.

  Axiom no_match_subset_r : forall k n m1 m2,
    is_match k n m1 = false ->
    is_match k n (intersect m2 m1) = false.

  Axiom pkt_match_intersect : forall k n m m',
      is_match k n m = true -> 
      is_match k n m' = true ->
      is_match k n (intersect m m') = true.

  Axiom packet_split : forall k n m1 m2,
    is_overlapped m1 m2 = false ->
    is_match k n m1 = true  ->
    is_match k n m2 = false.


End IMPL.

Module ImplAux (Import TheImpl : IMPL).

  Definition well_behaved (f : A -> A -> A) : Prop :=
    (forall (a : A), f a ActionUnit === a) /\
    (forall (a : A), f ActionUnit a === a) /\
    (forall a a' b b', a === a' -> b === b' -> f a b === f a' b').

  Hint Rewrite pkt_eq_dec pat_eq_dec packet_split
    no_match_subset_l no_match_subset_r : packet.

End ImplAux.