Require Import Packet.
Require Import Coq.Lists.List.
Require Import Coq.Arith.MinMax.

Module Make (P : Packet).

Include P.

Inductive A : Type :=
  | None : A
  | Allow : A
  | Deny : A
  | GMB : nat -> A.

Definition S : Type := list (M * A).

Inductive T : Type :=
  | Tree : S -> list T -> T.

(* We can label a tree with its height at the root, and descending naturals
   down to the leaves. *)
Inductive wf_tree : nat -> T -> Prop :=
  | wf_tree_lst : forall (s : S) (lst : list T) (n : nat),
    (forall (t' : T), In t' lst -> wf_tree n t') ->
    wf_tree (Datatypes.S n) (Tree s lst).

Definition plus_P (a1 : A) (a2 : A) : A := match (a1, a2) with
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

Definition plus_C (a1 : A) (a2 : A) : A := match (a1, a2) with
  | (_, None) => a1
  | (None, _) => a2
  | (Deny, _) => Deny
  | (_, Deny) => Deny
  | (GMB m, GMB n) => GMB (max m n)
  | (Allow, GMB n) => GMB n
  | (GMB m, Allow) => GMB m
  | (Allow, Allow) => Allow
  end.

Fixpoint eval_S (pkt : M) (share : S) := match share with
  | nil => None
  | (m, a) :: tl => match is_overlapped m pkt with
    | true => plus_C a (eval_S pkt tl)
    | false => eval_S pkt tl
    end
  end.

Fixpoint eval_T (pkt : M) (t : T) := match t with
  | Tree share subtrees => 
      plus_P (eval_S pkt share)
             (fold_right plus_C None (map (eval_T pkt) subtrees))
end.

Definition N := list (M * A).

Section NetworkFlowTables.

Variable f : A -> A -> A.

Definition inter_entry (e1 : M * A) (e2 : M * A) := match (e1, e2) with
  | ((m,a), (m',a')) => (intersect m' m, f a a')
  end.

Definition inter_helper (n : N) (ma : M * A) (r : N) :=
  (map (inter_entry ma) n) ++ r.

Definition inter (n1 : N) (n2 : N) :=
  fold_right (inter_helper n2) nil n1.

Definition union (n1 : N) (n2 : N) : N := inter n1 n2 ++ n1 ++ n2.

End NetworkFlowTables.

Fixpoint scan (pkt : M) (n : N) := match n with
  | nil => None
  | (m,a) :: tl => match is_overlapped m pkt with
    | true => a
    | false => scan pkt tl
  end
end.


Fixpoint lin_S (share : S) := match share with
  | nil => nil
  | (m,a)::tl => union plus_C (cons (m,a) nil) (lin_S tl)
end.

Fixpoint lin_T (tree : T) := match tree with
  | Tree share subtrees => 
      union plus_P (lin_S share) 
            (fold_right (union plus_C) nil (map lin_T subtrees))
end.

End Make.
