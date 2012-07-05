Require Import Packet.
Require Import Coq.Lists.List.
Require Import Coq.Arith.MinMax.
Require Import Action.

Module Make (P : Packet).

  Include P.

  Inductive A : Type :=
    | None : A
    | Allow : A
    | Deny : A
    | GMB : nat -> A.

  Lemma A_eq_dec : forall (a1 a2 : A), { a1 = a2 } + { a1 <> a2 }.
  Proof. repeat decide equality. Qed.

  Definition well_behaved (f : A -> A -> A) : Prop :=
    (forall (a : A), f a None = a) /\
    (forall (a : A), f None a = a).


  Module A_as_Action <: ACTION.
    Definition A := A.
    Definition None := None.
    Definition A_eq_dec := A_eq_dec.
    Definition well_behaved := well_behaved.
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
  | (_, None) => a1
  | (None, _) => a2
  | (Deny, Allow) => Allow
  | (Allow, Allow) => Allow
  | (_, Deny) => Deny
  | (Deny, GMB n) => GMB n
  | (GMB m, GMB n) => GMB (max m n)
  | (Allow, GMB n) => GMB n
  | (GMB m, Allow) => GMB m
  end.

Definition plus_S (a1 : A_as_Action.A) (a2 : A_as_Action.A) : A_as_Action.A := match (a1, a2) with
  | (_, None) => a1
  | (None, _) => a2
  | (Deny, _) => Deny
  | (_, Deny) => Deny
  | (GMB m, GMB n) => GMB (max m n)
  | (Allow, GMB n) => GMB n
  | (GMB m, Allow) => GMB m
  | (Allow, Allow) => Allow
  end.

Lemma well_behaved_plus_P : well_behaved plus_P.
Proof. split; intros a; destruct a; auto. Qed.

Lemma well_behaved_plus_S : well_behaved plus_S.
Proof. split; intros a; destruct a; auto. Qed.

Require Classifier.
Module  Cl := Classifier.MakeClassifier (P) (A_as_Action).
Include Cl.

Fixpoint eval_S (pkt : pkt) (share : S) := match share with
  | nil => None
  | (m, a) :: tl => match is_match pkt m with
    | true => plus_S a (eval_S pkt tl)
    | false => eval_S pkt tl
    end
  end.

Fixpoint eval_T (pkt : pkt) (t : T) := match t with
  | Tree share subtrees => 
      plus_P (eval_S pkt share)
             (fold_right plus_S None (map (eval_T pkt) subtrees))
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
