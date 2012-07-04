Require Import Packet.
Require Import Coq.Lists.List.
Require Import Coq.Arith.MinMax.
Require Import Coq.Classes.Equivalence.
Require Import Action.
Require Import CpdtTactics.
Require Import Omega.

Module Make (P : Packet).

  Include P.

  Inductive A : Type :=
    | ActionUnit : A
    | Allow : A
    | Deny : A
    | GMB : nat -> A.

  Lemma A_eq_dec : forall (a1 a2 : A), { a1 = a2 } + { a1 <> a2 }.
  Proof. repeat decide equality. Qed.

  Inductive A_equiv : A -> A -> Prop :=
  | ActionUnit_equiv : A_equiv ActionUnit ActionUnit
  | Allow_equiv : A_equiv Allow Allow
  | Deny_equiv : A_equiv Deny Deny
  | GMB_equiv : forall n, A_equiv (GMB n) (GMB n).

  Hint Constructors A_equiv.

  Lemma A_equiv_is_equivalence : Equivalence A_equiv.
  Proof with auto.
    crush.
    unfold Reflexive. intros. destruct x...
    unfold Symmetric. intros.
    destruct x; destruct y; try solve [ inversion H | auto ].
    assert ({ n = n0 } + { ~ n = n0 }).
    apply eq_nat_dec.
    destruct H0. subst... inversion H. subst.  contradiction n1...
    unfold Transitive.
    intros. destruct y; inversion H; inversion H0; subst...
  Qed.

  Open Local Scope equiv_scope.

  Definition well_behaved (f : A -> A -> A) : Prop :=
    (forall (a : A), f a ActionUnit === a) /\
    (forall (a : A), f ActionUnit a === a) /\
    (forall a a' b b', f a b === f a' b').

  Module A_as_Action <: ACTION.
    Definition A := A.
    Definition ActionUnit := ActionUnit.
    Definition equiv := A_equiv.
    Instance A_Equivalence : Equivalence equiv.
    exact A_equiv_is_equivalence.
    Qed.
  Definition well_behaved (f : A -> A -> A) : Prop :=
    (forall (a : A), f a ActionUnit === a) /\
    (forall (a : A), f ActionUnit a === a) /\
    (forall (a a' b b' : A), a === a' -> b === b' -> f a b === f a' b').
  End A_as_Action.

Definition S : Type := list (pat * A).

Inductive T : Type :=
  | Tree : S -> list T -> T.

(* We can label a tree with its height at the root, and descending naturals
   down to the leaves. *)
Inductive wf_tree : nat -> T -> Prop :=
  | wf_tree_lst : forall (s : S) (lst : list T) (n : nat),
    (forall (t' : T), In t' lst -> wf_tree n t') ->
    wf_tree (Datatypes.S n) (Tree s lst).

Definition plus_P (a1 : A_as_Action.A) (a2 : A_as_Action.A) : A_as_Action.A := match (a1, a2) with
  | (_, ActionUnit) => a1
  | (ActionUnit, _) => a2
  | (Deny, Allow) => Allow
  | (Allow, Allow) => Allow
  | (_, Deny) => Deny
  | (Deny, GMB n) => GMB n
  | (GMB m, GMB n) => GMB (max m n)
  | (Allow, GMB n) => GMB n
  | (GMB m, Allow) => GMB m
  end.

Definition plus_S (a1 : A_as_Action.A) (a2 : A_as_Action.A) : A_as_Action.A := match (a1, a2) with
  | (_, ActionUnit) => a1
  | (ActionUnit, _) => a2
  | (Deny, _) => Deny
  | (_, Deny) => Deny
  | (GMB m, GMB n) => GMB (max m n)
  | (Allow, GMB n) => GMB n
  | (GMB m, Allow) => GMB m
  | (Allow, Allow) => Allow
  end.

Lemma well_behaved_plus_P : A_as_Action.well_behaved plus_P.
Proof.
  split. intros a; destruct a; apply reflexivity.
  split. intros a; destruct a; apply reflexivity.
  intros. destruct a ; destruct b; destruct a'; destruct b';
  inversion H0; inversion H; reflexivity.
Qed.

Lemma well_behaved_plus_S : A_as_Action.well_behaved plus_S.
Proof.
  split. intros a; destruct a; apply reflexivity.
  split. intros a; destruct a; apply reflexivity.
  intros. destruct a ; destruct b; destruct a'; destruct b';
  inversion H0; inversion H; reflexivity.
Qed.

Require Classifier.
Module  Cl := Classifier.MakeClassifier (P) (A_as_Action).
Include Cl.

Fixpoint eval_S (pkt : pkt) (share : S) := match share with
  | nil => ActionUnit
  | (m, a) :: tl => match is_match pkt m with
    | true => plus_S a (eval_S pkt tl)
    | false => eval_S pkt tl
    end
  end.

Fixpoint eval_T (pkt : pkt) (t : T) := match t with
  | Tree share subtrees => 
      plus_P (eval_S pkt share)
             (fold_right plus_S ActionUnit (map (eval_T pkt) subtrees))
end.

Fixpoint lin_S (share : S) := match share with
  | nil => nil
  | (m,a)::tl => union plus_S (cons (m,a) nil) (lin_S tl)
end.

Fixpoint lin_T (tree : T) := match tree with
  | Tree share subtrees => 
      union plus_P (lin_S share) 
            (fold_right (union plus_S) nil (map lin_T subtrees))
end.

End Make.
