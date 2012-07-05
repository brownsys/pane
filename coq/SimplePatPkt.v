Require Import Coq.Lists.List.
Require Import Coq.Arith.MinMax.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Classes.EquivDec.
Require Import CpdtTactics.
Require Import Omega.
Require Import Coq.Bool.Bool.

Record pkt : Set := mkPkt {
  pktSrcHost : nat;
  pktDstHost : nat;
  pktSrcPort : nat;
  pktDstPort : nat
}.

Inductive patElt : Set := 
  | PatSrcHost : nat -> patElt
  | PatDstHost : nat -> patElt
  | PatSrcPort : nat -> patElt
  | PatDstPort : nat -> patElt.

Definition pat := list patElt.

Lemma pkt_eq_dec : forall (p p' : pkt), { p = p' } + { p <> p' }.
Proof. repeat decide equality. Qed.

Lemma pat_eq_dec : forall (p p' : pat), { p = p' } + { p <> p' }.
Proof. repeat decide equality. Qed.

Inductive patElt_equiv : patElt -> patElt -> Prop :=
 | SrcHostEquiv : forall n, patElt_equiv (PatSrcHost n) (PatSrcHost n)
 | DstHostEquiv : forall n, patElt_equiv (PatDstHost n) (PatDstHost n)
 | SrcPortEquiv : forall n, patElt_equiv (PatSrcPort n) (PatSrcPort n)
 | DstPortEquiv : forall n, patElt_equiv (PatDstPort n) (PatDstPort n).

Hint Constructors patElt_equiv.

Open Local Scope equiv_scope.
Instance PatElt_Equivalence : Equivalence patElt_equiv.
Proof with auto.
  crush.
  unfold Reflexive. intros. destruct x; crush.
  unfold Symmetric. intros. destruct x; destruct y; 
    try solve [ inversion H | 
 assert ({ n = n0 } + { ~ n = n0 }) by (apply eq_nat_dec); destruct H0; 
   subst; auto; inversion H; subst; contradiction n1; auto ].
  unfold Transitive.
  intros. destruct y; inversion H; inversion H0; subst...
Qed.

Definition beq_patElt p1 p2 := match (p1, p2) with
 | (PatSrcHost n, PatSrcHost n') => beq_nat n n'
 | (PatDstHost n, PatDstHost n') => beq_nat n n'
 | (PatSrcPort n, PatSrcPort n') => beq_nat n n'
 | (PatDstPort n, PatDstPort n') => beq_nat n n'
 | _ => false
end.

Fixpoint intersect (p : pat) (p' : pat) := 
  match p' with
    | nil => nil
    | (elt :: rest) =>
      if existsb (fun elt' => beq_patElt elt elt') p then
        elt :: (intersect p rest)
        else
          intersect p rest
  end.

Definition is_overlapped (p1 : pat) (p2 : pat) : bool :=
  match (intersect p1 p2) with
    | nil => false
    | _ => true
  end.

Definition exact_match (k : pkt)  :=
  cons (PatSrcHost (pktSrcHost k))
    (cons (PatDstHost (pktDstHost k))
      (cons (PatSrcPort (pktSrcPort k))
        (cons (PatDstPort (pktDstPort k)) nil))).

Definition match_elt (k : pkt) (t : patElt) := match t with
  | (PatSrcHost n) => beq_nat (pktSrcHost k) n
  | (PatDstHost n) => beq_nat (pktDstHost k) n
  | (PatSrcPort n) => beq_nat (pktSrcPort k) n
  | (PatDstPort n) => beq_nat (pktDstPort k) n
 end.

Fixpoint is_match (k : pkt) (t : pat) := match t with
  | nil => true
  | (hd :: tl) => andb (match_elt k hd) (is_match k tl)
end.

Lemma is_match_inv : forall k t,
  is_match k t = false ->
  (exists e : patElt, In e t /\ match_elt k e = false).
Proof with (auto with datatypes).
  intros.
  induction t.
  inversion H.
  simpl in H.
  apply andb_false_iff in H.
  destruct H.
  exists a...
  apply IHt in H.
  destruct H as [e [H H']].
  exists e...
Qed.

Lemma no_match_subset_r : forall k t' t,
  is_match k t = false ->
  is_match k (intersect t' t) = false.
Proof with (auto with datatypes).
  intros.
  apply is_match_inv in H.
  destruct H as [e [H H']].
(*  generalize dependent e. *)
  induction t'; intros.
Admitted.

Lemma no_match_subset_l : forall k t t',
  is_match k t = false ->
  is_match k (intersect t t') = false.
Proof.
Admitted.

Lemma pkt_match_intersect : forall k t t',
  is_match k t = true ->
  is_match k t' = true ->
  is_match k (intersect t t') = true.
Proof with auto.
  intros.
Admitted.

Lemma packet_split : forall k t t',
  is_overlapped t t' = false ->
  is_match k t = true ->
  is_match k t' = false.
Admitted.

Module ImplFragment.

  Definition pkt := pkt.
  Definition pat := pat. 
  Definition pkt_eq_dec := pkt_eq_dec.
  Definition pat_eq_dec := pat_eq_dec.
  Definition intersect := intersect.
  Definition is_overlapped := is_overlapped.
  Definition is_match := is_match.
  Definition exact_match := exact_match.
  Definition no_match_subset_l := no_match_subset_l.
  Definition no_match_subset_r := no_match_subset_r.
  Definition pkt_match_intersect := pkt_match_intersect.
  Definition packet_split := packet_split.
End ImplFragment.
