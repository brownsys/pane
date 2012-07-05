Require Import Coq.Lists.List.
Require Import Coq.Arith.MinMax.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Classes.EquivDec.
Require Import Impl.
Require Import CpdtTactics.
Require Import Omega.
Require Import Coq.Bool.Bool.
Require Classifier.

Inductive A : Type :=
| NoAction : A
| Allow : A
| Deny : A
| GMB : nat -> A.

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

Inductive A_equiv : A -> A -> Prop :=
| None_equiv : A_equiv NoAction NoAction
| Allow_equiv : A_equiv Allow Allow
| Deny_equiv : A_equiv Deny Deny
| GMB_equiv : forall n, A_equiv (GMB n) (GMB n).

Section Equiv.

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

End Equiv.

Open Local Scope equiv_scope.

Definition well_behaved (f : A -> A -> A) : Prop :=
  (forall (a : A), f a NoAction === a) /\
  (forall (a : A), f NoAction a === a) /\
  (forall a a' b b', a === a' -> b === b' -> f a b === f a' b').

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

Module HFT_Impl <: IMPL.

  Definition A := A.
  Definition pkt := pkt.
  Definition pat := pat.
  Definition ActionUnit := NoAction.
  Definition equiv := A_equiv.
  Program Instance A_Equivalence : Equivalence equiv := A_equiv_is_equivalence.
 Definition well_behaved (f : A -> A -> A) : Prop :=
    (forall (a : A), f a ActionUnit === a) /\
    (forall (a : A), f ActionUnit a === a) /\
    (forall (a a' b b' : A), a === a' -> b === b' -> f a b === f a' b').
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
End HFT_Impl.

Definition S : Type := list (pat * A).

Inductive T : Type :=
  | Tree : S -> list T -> T.

(* We can label a tree with its height at the root, and descending naturals
   down to the leaves. *)
Inductive wf_tree : nat -> T -> Prop :=
  | wf_tree_lst : forall (s : S) (lst : list T) (n : nat),
    (forall (t' : T), In t' lst -> wf_tree n t') ->
    wf_tree (Datatypes.S n) (Tree s lst).

Definition plus_P (a1 : A) (a2 : A)  := match (a1, a2) with
  | (_, NoAction) => a1
  | (NoAction, _) => a2
  | (Deny, Allow) => Allow
  | (Allow, Allow) => Allow
  | (_, Deny) => Deny
  | (Deny, GMB n) => GMB n
  | (GMB m, GMB n) => GMB (max m n)
  | (Allow, GMB n) => GMB n
  | (GMB m, Allow) => GMB m
  end.

Lemma well_behaved_plus_P : HFT_Impl.well_behaved plus_P.
Proof.
  split. intros a; destruct a; apply reflexivity.
  split. intros a; destruct a; apply reflexivity.
  intros. destruct a ; destruct b; destruct a'; destruct b';
  inversion H0; inversion H; reflexivity.
Qed.

Definition plus_S (a1 a2 : A) := match (a1, a2) with 
  | (_, NoAction) => a1
  | (NoAction, _) => a2
  | (Deny, _) => Deny
  | (_, Deny) => Deny
  | (GMB m, GMB n) => GMB (max m n)
  | (Allow, GMB n) => GMB n
  | (GMB m, Allow) => GMB m
  | (Allow, Allow) => Allow
  end.

Lemma well_behaved_plus_S : HFT_Impl.well_behaved plus_S.
Proof.
  split. intros a; destruct a; apply reflexivity.
  split. intros a; destruct a; apply reflexivity.
  intros. destruct a ; destruct b; destruct a'; destruct b';
  inversion H0; inversion H; reflexivity.
Qed.

Module Import Cl := Classifier.MakeClassifier (HFT_Impl).

Fixpoint eval_S (pkt : pkt) (share : S) := match share with
  | nil => NoAction
  | (m, a) :: tl => match is_match pkt m with
    | true => plus_S a (eval_S pkt tl)
    | false => eval_S pkt tl
    end
  end.

Fixpoint eval_T (pkt : pkt) (t : T) := match t with
  | Tree share subtrees => 
      plus_P (eval_S pkt share)
             (fold_right plus_S NoAction (map (eval_T pkt) subtrees))
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

Section Correctness.

 Hint Constructors wf_tree T A.

  Section WellFormedness.

  Fixpoint height (tree : T) : nat := match tree with
    | Tree _ children => 1 + fold_right max 0 (map height children)
  end.

  Hint Immediate le_or_lt : arith.

  Lemma max_le : forall (l m n : nat),
    max l m <= n ->
    l <= n /\ m <= n.
  Proof with (auto with arith).
  intros. 
  assert (l <= m \/ m < l) as [H0 | H1]...
  assert (max l m = m)... crush.
  assert (max l m = l)... crush.
  Qed.

  Hint Resolve max_le succ_max_distr: arith.

  Lemma split_height: forall (s : S) (a : T) (L : list T) (n : nat),
    height (Tree s (a :: L)) <= n ->
    height (Tree s L) <= n.
  Proof with auto with arith.
  intros.
  generalize dependent a.
  induction L; intros; crush.
  rewrite -> succ_max_distr in H.
  apply max_le in H.
  crush.
  Qed.

  Lemma height_list : forall (lst : list T) (n : nat) (t : T),
    fold_right max 0 (map height lst) <= n ->
    In t lst ->
    height t <= n.
  Proof with crush.
  intros.
  induction lst...
  apply max_le in H...
  apply IHlst in H1...
  apply max_le in H...
  Qed.

  Hint Resolve height_list.

  Lemma decreasing_height: forall (s : S) (lst : list T) (t : T) (n : nat),
    height (Tree s lst) <= n ->
  In t lst ->
  height t < n.
  Proof with eauto with arith.
  intros.
  generalize dependent lst.
  induction n; intros...
  Qed.

  Hint Resolve decreasing_height.

  Lemma wf_exists_aux : forall (tree : T) (n : nat),
    height tree <= n -> wf_tree n tree.
  Proof with simpl; (eauto with arith).
  intros.
  generalize dependent tree.
  induction n; intros; destruct tree.
  crush.
  (* Inductive *)
  apply wf_tree_lst.
  intros. apply IHn. crush.
  assert (height t' < Datatypes.S n)...
  Qed.

  Lemma wf_exists : forall (tree : T),
    wf_tree (height tree) tree.
  Proof. auto using wf_exists_aux. Qed.

End WellFormedness.

Hint Resolve inter_elim union_comm.

Open Local Scope equiv_scope.

Lemma flatten_eval_S : forall (share : S) (pkt : pkt),
  eval_S pkt share === scan pkt (lin_S share).
Proof with auto.
  intros share pkt.
  intros.
  induction share.
  (* Base case: empty share *)
  crush. apply reflexivity.
  (* Inductive case *)
  simpl.
  destruct a.
  remember (is_match pkt p).
  destruct b.
  (* Case: matches at head *)
  assert (scan pkt (union plus_S ((p,a)::nil) (lin_S share)) ===
          plus_S (scan pkt ((p,a)::nil)) (scan pkt (lin_S share))).
    apply union_comm... apply well_behaved_plus_S.
  rewrite -> H.
  simpl. 
  assert (is_match = HFT_Impl.is_match)...
  rewrite <- H0.
  rewrite <- Heqb.
  apply well_behaved_plus_S... apply reflexivity.
  (* Case: does not match at head *)
  unfold union. rewrite inter_elim... simpl. 
  assert (is_match = HFT_Impl.is_match)...
  rewrite <- H.
  rewrite <- Heqb...
Qed.

Notation "x +P y" := (plus_P x y) (at level 50, left associativity).
Notation "x +C y" := (plus_S x y) (at level 50, left associativity).

Lemma flatten_eval_T : forall (n : nat) (tree : T) (pkt : pkt),
  wf_tree n tree ->
  eval_T pkt tree === scan pkt (lin_T tree).
Proof with simpl; auto.
  intros n tree pkt H.
  generalize dependent tree.
  induction n.
  intros; inversion H.
(* Inductive case *)
intros.
destruct tree.
simpl.  
rewrite -> union_comm...
assert (eval_S pkt s === scan pkt (lin_S s)).
  apply flatten_eval_S...
  apply well_behaved_plus_P...
  clear H0.
  induction l... 
  apply reflexivity.
  rewrite -> union_comm...
  assert (eval_T pkt a === scan pkt (lin_T a)).
    apply IHn. inversion H. crush.
  assert (fold_right plus_S NoAction (map (eval_T pkt) l) ===
          scan pkt (fold_right (union plus_S) nil (map lin_T l))).
    apply IHl. intros. apply wf_tree_lst. inversion H; subst. crush.
  crush.
  apply well_behaved_plus_S...
apply well_behaved_plus_S.
apply well_behaved_plus_P.
Qed.

Lemma eval_scan_eq : forall (n : nat) (tree : T) (pkt : pkt),
  eval_T pkt tree === scan pkt (lin_T tree).
Proof with auto.
intros.
assert (wf_tree (height tree) tree). apply wf_exists.
apply flatten_eval_T with (n := height tree)...
Qed.

End Correctness.
