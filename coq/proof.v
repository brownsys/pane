Require Import Coq.Init.Datatypes.
Require Import Coq.Lists.List.
Require Import Coq.Arith.MinMax.
Require Import Omega.
Require Import CpdtTactics.
Require Import Packet.
Require Import HFT_Defns.
Require Import Action.
Require Import Coq.Classes.Equivalence.

Module HFT (Import P : Packet).

  Module Defns := HFT_Defns.Make P.
  Include Defns.

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
  rewrite <- Heqb.
  apply well_behaved_plus_S... apply reflexivity.
  (* Case: does not match at head *)
  unfold union. rewrite inter_elim... simpl. rewrite <- Heqb...
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
  assert (fold_right plus_S ActionUnit (map (eval_T pkt) l) ===
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

End HFT.
