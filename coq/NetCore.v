Require Import Coq.Lists.List.
Require Import Coq.Arith.Arith_base.
Require Import Coq.Arith.MinMax.
Require Import Impl.
Require Import Classifier.
Require Import Coq.MSets.MSetList.
Require Import Coq.Arith.NatOrderedType.
Require Import Coq.MSets.MSetDecide.
Require Import Coq.Classes.Equivalence.
Require Import CpdtTactics.
Require Import SimplePatPkt.

Module NatSet := MSetList.Make (Nat_as_OT).
Module NatSetDecide := MSetDecide.Decide (NatSet).

Definition port := nat.
Definition switch := nat.

Inductive A : Type :=
| Forward : NatSet.t -> A.

Section Equivalence.

  Inductive equiv_A : A -> A -> Prop :=
  | equiv_Forward : forall fs1 fs2,
    NatSet.Equal fs1 fs2 ->
    equiv_A (Forward fs1) (Forward fs2).
  
  Hint Constructors equiv_A.

  Lemma A_equiv_is_equivalence : Equivalence equiv_A.
  Proof with try solve [ auto | NatSetDecide.fsetdec ].
    split.
    unfold Reflexive.
    intros. destruct x. apply equiv_Forward...
  unfold Symmetric.
    intros. destruct x; destruct y. apply equiv_Forward.
    inversion H...
  unfold Transitive.
    intros. destruct y. inversion H. inversion H0. apply equiv_Forward...
  Qed.

End Equivalence.

Open Local Scope equiv_scope.

Module TheImpl <: IMPL.

  Definition A := A.
  Include SimplePatPkt.ImplFragment.
  Definition ActionUnit := Forward NatSet.empty.
  Definition equiv := equiv_A.
  Program Instance A_Equivalence : Equivalence equiv := A_equiv_is_equivalence.
End TheImpl.

Module TheImplAux := ImplAux (TheImpl).

Inductive pred : Set :=
| PredPat : pat -> pred.
 (* | PredAnd : pred -> pred -> pred. *)

Inductive pol : Type :=
| PolAtom : pred -> A -> pol
| PolUnion : pol -> pol -> pol.

Fixpoint match_pred (p : pred) (k : pkt) (pt : port) :=
  match p with
    | PredPat m => is_match k pt m
(*      | PredAnd p1 p2 => andb (match_pred p1 k) (match_pred p2 k) *)
  end.

Definition cmb_action a1 a2 := 
  match (a1, a2) with
    | (Forward ps, Forward ps') => Forward (NatSet.union ps ps')
  end.

Lemma well_behaved_cmb : TheImplAux.well_behaved cmb_action.
Proof with try solve [ auto | NatSetDecide.fsetdec ].
  repeat (split; intros).
  destruct a. unfold cmb_action. simpl. apply equiv_Forward...
  destruct a. unfold cmb_action. simpl. apply equiv_Forward...
  destruct a; destruct b; destruct a'; destruct b'.
  inversion H; inversion H0. unfold cmb_action.
  apply equiv_Forward...
Qed.

Fixpoint interp (p : pol) (k : pkt) (pt : port) := 
  match p with
    | PolAtom pred act => 
      if match_pred pred k pt then act else Forward NatSet.empty
    | PolUnion p1 p2 => cmb_action (interp p1 k pt) (interp p2 k pt)
  end.

Module Import Cl := Classifier.MakeClassifier (TheImpl).

Fixpoint compile_pred (p : pred) (a : A) := 
  match p with
    | PredPat pat => cons (pat, a) nil
(* | PredAnd p1 p2 => compile_pred p1 a ++ compile_pred p2 a *)
  end.

Fixpoint compile (p : pol) := 
  match p with
    | PolAtom pred act => compile_pred pred act
    | PolUnion p1 p2 => union cmb_action (compile p1) (compile p2)
  end.

Open Scope equiv_scope.

Hint Resolve well_behaved_cmb.

Lemma compile_correct : forall pkt pt pol,
  (interp pol pkt pt) === (scan pkt pt (compile pol)).
Proof with simpl; auto.
  intros pkt pt pol.
  induction pol.
    (* Predicate *)
  induction p.
  simpl.
  assert (is_match = TheImpl.is_match)...
  rewrite <- H.
  destruct (is_match pkt pt p); apply reflexivity.
    (* union *)
  simpl.
  rewrite -> union_comm...
  apply well_behaved_cmb...
  apply well_behaved_cmb.
Qed.

Extraction Language Haskell.
Recursive Extraction compile.