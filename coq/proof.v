Require Import Coq.Init.Datatypes.
Require Import Coq.Lists.List.
Require Import Coq.Arith.MinMax.
Require Import Omega.
Require Import CpdtTactics.

Module Type Packet.

  Parameter M : Type.

  Parameter is_packet : M -> bool.
  Parameter is_overlapped : M -> M -> bool.
  Parameter intersect : M -> M -> M.

  Axiom intersect_comm : forall (m1 m2 : M),
    intersect m1 m2 = intersect m2 m1.
  Axiom overlap_intersect : forall (m1 m2 m3 : M),
    is_overlapped m1 m2 = false -> is_overlapped (intersect m1 m3) m2 = false.
  Axiom overlap_intersect_both : forall (m1 m2 m3 : M),
    is_overlapped m1 m3 = true -> 
    is_overlapped m2 m3 = true ->
    is_overlapped (intersect m1 m2) m3 = true.

  Axiom m_eq_dec : forall (m1 m2 : M), { m1 = m2 } + { m1 <> m2 }.
  Axiom packet_split : forall (pkt m1 m2 : M),
    is_packet pkt = true ->
    is_overlapped m1 m2 = false ->
    is_overlapped m1 pkt = true  ->
    is_overlapped m2 pkt = false.
  Axiom overlap_inter_1 : forall (m0 m1 m2 : M),
    is_overlapped m1 m2 = false ->
    is_overlapped (intersect m1 m0) m2 = false.

 Hint Resolve intersect_comm : packet.
 Hint Rewrite overlap_intersect overlap_intersect_both
              m_eq_dec packet_split overlap_inter_1 : packet.
End Packet.

Module HFT (P : Packet).

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

  Lemma A_eq_dec : forall (a1 a2 : A), { a1 = a2 } + { a1 <> a2 }.
  Proof.
    decide equality.
    decide equality.
  Qed.


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

Definition well_behaved (f : A -> A -> A) : Prop :=
  (forall (a : A), f a None = a) /\
  (forall (a : A), f None a = a).

Tactic Notation "prove_well_behaved" := split; intros a; destruct a; auto.
  
Lemma well_behaved_plus_P : well_behaved plus_P.
Proof. prove_well_behaved. Qed.

Lemma well_behaved_plus_C : well_behaved plus_C.
Proof. prove_well_behaved. Qed.

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

Fixpoint scan (pkt : M) (n : N) := match n with
  | nil => None
  | (m,a) :: tl => match is_overlapped m pkt with
    | true => a
    | false => scan pkt tl
  end
end.

Inductive scan_rel : M -> N -> A -> Prop :=
  | scan_empty : forall (pkt : M), scan_rel pkt nil None
  | scan_head : forall (pkt m: M) (a : A) (tl : N),
      is_overlapped m pkt = true ->
      scan_rel pkt ((m,a)::tl) a
  | scan_tail : forall (pkt m: M) (a a' : A) (tl : N),
      is_overlapped m pkt = false ->
      scan_rel pkt tl a' ->
      scan_rel pkt ((m,a)::tl) a'.

Hint Constructors scan_rel.

Lemma scan_alg : forall (pkt : M) (a : A) (n : N),
  scan pkt n = a <-> scan_rel pkt n a.
Proof with subst; eauto.
split; intros.
induction n.
(* Case 1 *)
crush.
(* Case 2 *)
destruct a0; simpl in H.
remember (is_overlapped m pkt) as B.
destruct B...
(* Case 3 *)
induction n.
inversion H; crush.
destruct a0; inversion H; subst.
crush.
crush.
Qed.

Definition scan_inv (pkt : M) (N1 : N) :=
  ((forall (m : M) (a : A), 
    In (m,a) N1 -> is_overlapped m pkt = false) /\
   scan pkt N1 = None) \/
  (exists N2 : N, exists N3 : N, exists m : M, exists a : A,
   N1 = N2 ++ (m,a)::N3 /\
   is_overlapped m pkt = true /\
   scan pkt N1 = a /\
   (forall (m' : M) (a' : A), In (m',a') N2 -> is_overlapped m' pkt = false)).

Lemma scan_off : forall (pkt : M) (N1 : N), scan_inv pkt N1.
Proof with intros; simpl; auto with datatypes.
unfold scan_inv.
intros.
induction N1.
(* Base case *)
crush.
(* Inductive case *)
destruct IHN1.
destruct a.
remember (is_overlapped m pkt) as b. destruct b.
(* Case 1 *)
right. exists nil. exists N1. exists m. exists a. crush. rewrite <- Heqb...
(* Case 2 *)
left. crush. apply H0 in H2. crush. rewrite <- Heqb...
(* Case 3 *)
destruct H as [N2 [N3 [m [a' [Neq  [Hov [Ha'eq H]]]]]]].
destruct a.
remember (is_overlapped m0 pkt) as b. destruct b.
right. exists nil. exists N1. exists m0. exists a. crush. rewrite <- Heqb...
right. exists ((m0,a) :: N2). exists N3. exists m. exists a'.
   crush. rewrite <- Heqb... apply H in H1. crush.
Qed.

Lemma elim_mismatch : forall (n1 n2 : N) (p m : M) (a : A),
  is_overlapped m p = false ->
  scan p (n1 ++ (m,a)::n2) = scan p (n1 ++ n2).
Proof. intros. induction n1; crush. Qed.

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

Hint Unfold union inter inter_helper inter_entry.

Lemma eval_elim_left : forall (N1 N2 : N) (pkt : M),
  (forall (m : M) (a : A), In (m,a) N1 -> is_overlapped m pkt = false) ->
  scan pkt (N1 ++ N2) = scan pkt N2.
Proof with eauto with arith datatypes.
intros.
induction N1. simpl...
destruct a.
assert (forall (m : M) (a : A), In (m,a) N1 -> is_overlapped m pkt = false).
 intros. apply H with (a0 := a0). crush. 
apply IHN1 in H0.
simpl.
remember (is_overlapped m pkt).
destruct b.
assert (is_overlapped m pkt = false).
  apply H with (a0 := a). simpl...
crush.
crush.
Qed.

Hint Resolve overlap_intersect.
Hint Rewrite intersect_comm.

Lemma eval_inter_step : forall (N1 N2 : N) (m pkt : M) (a : A),
  is_overlapped m pkt = false ->
  scan pkt (inter_helper N1 (m, a) N2) = scan pkt N2.
Proof with auto with packet.
intros.
induction N1.
auto.
(* Inductive case *)
unfold inter_helper in *.
destruct a0. simpl.
remember (is_overlapped (intersect m0 m) pkt).
destruct b...
assert (is_overlapped (intersect m m0) pkt = false)...
rewrite intersect_comm in H0.
crush.
Qed.

Lemma inter_nil_l : forall (n : N),
  inter nil n = nil.
Proof. intros. induction n; crush. Qed.

Lemma inter_nil_r : forall (n : N), inter n nil = nil.
Proof. intros. induction n; crush. Qed.

Hint Resolve inter_nil_l inter_nil_r.

End NetworkFlowTables.

Fixpoint lin_S (share : S) := match share with
  | nil => nil
  | (m,a)::tl => union plus_C (cons (m,a) nil) (lin_S tl)
end.

Fixpoint lin_T (tree : T) := match tree with
  | Tree share subtrees => 
      union plus_P (lin_S share) 
            (fold_right (union plus_C) nil (map lin_T subtrees))
end.

Lemma elim_scan_head : forall (N1 N2 : N) (pkt : M),
  (forall (m : M) (a : A), In (m,a) N1 -> is_overlapped m pkt = false) ->
  scan pkt (N1 ++ N2) = scan pkt N2.
Proof with simpl; auto with datatypes.
intros.
induction N1...
destruct a.
assert (forall (m : M) (a : A), In (m,a) N1 -> is_overlapped m pkt = false).
  intros. assert (In (m0,a0) ((m,a)::N1))... apply H in H1...
apply IHN1 in H0.
simpl.
remember (is_overlapped m pkt) as b. destruct b...
assert (is_overlapped m pkt = false).
  assert (In (m,a) ((m,a)::N1))... 
  apply H in H1...
crush.
Qed.

Lemma elim_scan_mid : forall (N1 N2 : N) (pkt m : M) (a : A),
  is_overlapped m pkt = false ->
  scan pkt (N1 ++ (m,a) :: N2) = scan pkt (N1 ++ N2).
Proof. intros. induction N1; crush. Qed.

Hint Resolve elim_scan_head elim_scan_mid.

Lemma elim_scan_middle : forall (N1 N2 N3 : N) (pkt : M),
  (forall (m : M) (a : A), In (m,a) N2 -> is_overlapped m pkt = false) ->
  scan pkt (N1 ++ N2 ++ N3) = scan pkt (N1 ++ N3).
Proof. intros. generalize dependent N2. induction N1; crush. Qed.

Lemma elim_inter_head : forall (N1 N2 : N) (pkt m : M) (f : A -> A -> A) 
                               (a : A),
  is_overlapped m pkt = false ->
  scan pkt (map (inter_entry f (m, a)) N1 ++ N2) = scan pkt N2.
Proof with auto with packet.
intros.
induction N1.
crush.
destruct a0.
simpl.
assert (is_overlapped (intersect m0 m) pkt = false).
  rewrite -> intersect_comm.
  autorewrite with packet using auto...
crush.
Qed.

Hint Resolve elim_inter_head.

Lemma inter_empty_aux : forall (N1 : N) (m m0 pkt : M) (a a0 : A) (f : A -> A -> A),
  is_packet pkt = true ->
  (forall (m : M) (a : A), In (m,a) N1 -> is_overlapped m pkt = false) ->
  In (m,a) (map (inter_entry f (m0,a0)) N1) ->
  is_overlapped m pkt = false.
Proof with auto with datatypes.
intros.
induction N1.
crush.
destruct a1.
simpl in H1.
destruct H1.
(* contra *)
assert (is_overlapped m1 pkt = false).
  assert (In (m1,a1) ((m1,a1)::N1))...
  apply H0 in H2...
inversion H1.
apply overlap_inter_1...
(* inductive *)
assert (forall (m : M) (a : A), In (m,a) N1 -> is_overlapped m pkt = false).
  intros.
  assert (In (m2,a2) ((m1,a1)::N1))...
  apply H0 in H3...
apply IHN1 in H2...
Qed.

Lemma inter_empty : forall (N2 : N) (f : A -> A -> A) (pkt : M),
  is_packet pkt = true ->
  (forall (m: M) (a : A), In (m,a) N2 -> is_overlapped m pkt = false) ->
  (forall (N1 : N) (m: M) (a : A), In (m,a) (inter f N1 N2) -> 
                          is_overlapped m pkt = false).
Proof with auto with datatypes.
intros N2 f pkt.
intros Hpacket.
intros Hlap.
intros.
generalize dependent N2.
induction N1.
crush.
(* Inductive *)
destruct a0.
intros.
simpl in H.
unfold inter_helper in H.
rewrite -> in_app_iff in H.
destruct H.
apply inter_empty_aux with (N1 := N2) (m0 := m0) (f := f) (a := a) (a0 := a0)...
apply IHN1 in Hlap...
Qed.

Hint Resolve inter_empty scan_off.
Hint Rewrite in_app_iff.

Lemma union_comm : forall (n1 n2 : N) (pkt : M) (f : A -> A -> A),
  is_packet pkt = true ->
  well_behaved f ->
  scan pkt (union f n1 n2) = f (scan pkt n1) (scan pkt n2).
Proof with simpl; eauto with datatypes.
intros n1 n2 pkt f Hpacket.
intros.
remember H as Hwb.
destruct H as [H H0].
induction n1.
(* Base case *)
crush.
(* Inductive case *)
destruct a.
unfold union.
remember (is_overlapped m pkt).
assert (scan_inv pkt (inter f ((m, a) :: n1) n2 ++ ((m, a) :: n1) ++ n2)).
  crush.
destruct H1 as [ [H1 H2] | H1].
  (* Case: scan falls off the table. *)
  remember (inter f ((m, a) :: n1) n2) as l1.
  remember (n1 ++ n2) as l2.
  rewrite -> H2.
  remember (l1 ++ (((m,a)::n1) ++ n2)) as L.
  assert (scan pkt n2 = None) as HscanLeft.
    assert (scan_inv pkt n2)...
    destruct H3 as [[ H3 H4] | [N2 [N3 [m' [a' [H3 [H4 [H5 H6]]]]]]]].
    trivial.
    assert (In (m',a') n2). crush.
    assert (In (m',a') L). crush.
    apply H1 in H8. rewrite -> H8 in H4. inversion H4.
  assert (scan pkt ((m,a)::n1) = None) as HscanRight.
    assert (scan_inv pkt ((m,a)::n1))...
    destruct H3 as [[ H3 H4] | [N2 [N3 [m' [a' [H3 [H4 [H5 H6]]]]]]]]...
    assert (In (m',a') ((m,a)::n1)). rewrite -> H3...
    assert (In (m',a') L).
      rewrite -> HeqL. rewrite -> H3. rewrite -> in_app_iff. right...
    apply H1 in H8.  rewrite -> H8 in H4. inversion H4. crush.
  (* Case: scan does not fall off the table. *)
  destruct H1 as [N2 [N3 [m0 [a0 [Heq [Hlap [Hresult Hscan]]]]]]].
  assert ({ is_overlapped m0 m = true }+ { ~ is_overlapped m0 m = true }).
    decide equality.
  destruct H1.
  (* Case: for (m0,a0) where a0 is the result, is_overlapped m0 m = true.
     So, inductive hypothesis does not work. *)

  Focus 2.
  (* Case: for (m0,a0) where a0 is the result, is_overlapped m0 m = false.
     So, induction should work. *)
  assert (is_overlapped m0 m = false). 
    destruct (is_overlapped m0 m).
    contradiction n. reflexivity. reflexivity.
  clear n.
  assert (is_overlapped m pkt = false).
    apply packet_split with (m1 := m0)...
  simpl. unfold inter_helper. rewrite <- app_assoc.  

  assert (scan pkt (map (inter_entry f (m, a)) n2 ++ inter f n1 n2 ++ (m, a) :: n1 ++ n2) = 
          scan pkt (inter f n1 n2 ++ (m, a) :: n1 ++ n2)).
    apply elim_inter_head. exact H2.
  rewrite -> H3.
  rewrite -> H2.
  assert (scan pkt (inter f n1 n2 ++ (m,a) :: n1 ++ n2) =
          scan pkt (inter f n1 n2 ++ n1 ++ n2)).
    apply elim_scan_mid.
    exact H2.
  rewrite -> H4.
  unfold union in IHn1.
  exact IHn1.

  (* Case: for (m0,a0) where a0 is the result, is_overlapped m0 m = true.
     So, inductive hypothesis does not work. *)
  rewrite <- app_comm_cons.
  destruct b.
  Focus 2. (* Packet is not in m, like the case above *)
  simpl.
  rewrite <- Heqb.
  assert (scan pkt (inter_helper f n2 (m, a) (inter f n1 n2) ++ (m, a) :: n1 ++ n2)
          = scan pkt (inter_helper f n2 (m, a) (inter f n1 n2) ++ n1 ++ n2)).
    apply elim_scan_mid. symmetry. exact Heqb.
  rewrite -> H1.
  unfold inter_helper.
  rewrite <- app_assoc.
  assert (scan pkt (map (inter_entry f (m, a)) n2 ++ inter f n1 n2 ++ n1 ++ n2) = 
          scan pkt (inter f n1 n2 ++ n1 ++ n2)).
    apply elim_inter_head. symmetry. exact Heqb.
  rewrite -> H2.
  unfold union in IHn1.
  exact IHn1.

  (* Case where pkt is in m *)
  assert (scan_inv pkt n2). apply scan_off...
  unfold scan_inv in H1.
  destruct H1.
  destruct H1.
  rewrite -> H2.
  assert (forall (m' : M) (a' : A), In (m',a') (inter f ((m, a) :: n1) n2) ->
                 is_overlapped m' pkt = false).
    apply inter_empty. exact Hpacket. exact H1.
  assert (scan pkt (inter f ((m, a) :: n1) n2 ++ (m, a) :: n1 ++ n2) =
          scan pkt ((m,a) :: n1 ++ n2)).
    apply elim_scan_head. exact H3.
  rewrite -> H4.
  assert ((m,a)::n1++n2 = ((m,a) :: n1) ++ n2).
    simpl. reflexivity.
  rewrite -> H5.
  assert (n2 = n2 ++ nil).
    rewrite -> app_nil_r. reflexivity.
  rewrite -> H6.
  assert (scan pkt (((m,a)::n1)++n2++nil) = scan pkt (((m,a)::n1)++nil)).
    apply elim_scan_middle. exact H1.
  rewrite -> H7.
  rewrite -> app_nil_r.
  assert (f (scan pkt ((m,a)::n1)) None = scan pkt ((m,a)::n1)).
    apply H.
  rewrite -> H8.
  reflexivity.

  (* Case where: we are in the intersection at the tip *)

  simpl.
  rewrite <- Heqb.
  unfold inter_helper.
  rewrite <- app_assoc.
  destruct H1 as [N2' [N3' [m' [a' [Heq' [Hlap' [Hscan' Hlap2']]]]]]].
  rewrite -> Hscan'.
  rewrite -> Heq'.
  rewrite -> map_app.
  rewrite <- app_assoc.
  remember ( inter f n1 (N2' ++ (m', a') :: N3') ++
      (m, a) :: n1 ++ N2' ++ (m', a') :: N3') as Trash.
  assert (forall (m5 : M) (a5 : A), 
          In (m5,a5) (map (inter_entry f (m,a)) N2') ->
          is_overlapped m5 pkt = false).
    assert (map (inter_entry f (m,a)) N2' = inter f ((m,a) ::nil) N2').
      simpl. unfold inter_helper. rewrite -> app_nil_r. reflexivity.
    rewrite -> H1.
    apply inter_empty. exact Hpacket. exact Hlap2'.
  assert (scan pkt (map (inter_entry f (m, a)) N2' ++
            ((map (inter_entry f (m, a)) ((m', a') :: N3')) ++ Trash)) =
          scan pkt (map (inter_entry f (m, a)) ((m', a') :: N3') ++ Trash)).
    apply elim_scan_head. exact H1.
  rewrite -> H2.
  simpl.
  assert (is_overlapped (intersect m' m) pkt = true).
    apply overlap_intersect_both. auto. auto.
  rewrite -> H3.
  auto.
Qed. 

Lemma inter_elim : forall (f : A -> A -> A)
  (m pkt : M) (a : A) (L1 L2 : N),
  is_overlapped m pkt = false ->
  scan pkt (inter f ((m, a) :: nil) L1 ++ L2) =
  scan pkt L2.
Proof with auto.
  intros.
  induction L1.
  crush.
  destruct a0. 
  assert (is_overlapped (intersect m0 m) pkt = false).
    rewrite <- intersect_comm.
    autorewrite with packet using auto...
  crush.
Qed.

Hint Resolve well_behaved_plus_C well_behaved_plus_P.

Lemma flatten_eval_S : forall (share : S) (pkt : M),
  is_packet pkt = true ->
  eval_S pkt share = scan pkt (lin_S share).
Proof with auto.
  intros share pkt Hpacket.
  intros.
  induction share.
  (* Base case: empty share *)
  crush.
  (* Inductive case *)
  simpl.
  destruct a.
  remember (is_overlapped m pkt).
  destruct b.
  (* Case: matches at head *)
  assert (scan pkt (union plus_C ((m,a)::nil) (lin_S share)) =
          plus_C (scan pkt ((m,a)::nil)) (scan pkt (lin_S share))).
    apply union_comm...
  rewrite -> H.
  simpl.
  rewrite <- Heqb.
  rewrite -> IHshare...
  (* Case: does not match at head *)
  unfold union. rewrite inter_elim... simpl. rewrite <- Heqb...
Qed.

Notation "x +P y" := (plus_P x y) (at level 50, left associativity).
Notation "x +C y" := (plus_C x y) (at level 50, left associativity).

Lemma flatten_eval_T : forall (n : nat) (tree : T) (pkt : M),
  is_packet pkt = true ->
  wf_tree n tree ->
  eval_T pkt tree = scan pkt (lin_T tree).
Proof with simpl; auto.
  intros n tree pkt Hpacket.
  intros.
  generalize dependent tree.
  induction n.
  intros; inversion H.
(* Inductive case *)
intros.
destruct tree.
simpl.
rewrite -> union_comm...
assert (eval_S pkt s = scan pkt (lin_S s)). apply flatten_eval_S...
rewrite -> H0. clear H0.
assert (fold_right plus_C None (map (eval_T pkt) l) =
        scan pkt (fold_right (union plus_C) nil (map lin_T l))).
  induction l... crush.
  rewrite -> union_comm...
  assert (eval_T pkt a = scan pkt (lin_T a)).
    apply IHn. inversion H. crush.
  assert (fold_right plus_C None (map (eval_T pkt) l) = 
          scan pkt (fold_right (union plus_C) nil (map lin_T l))).
    apply IHl. intros. apply wf_tree_lst. inversion H; subst. crush.
  crush.
crush.
Qed.

Lemma eval_scan_eq : forall (n : nat) (tree : T) (pkt : M),
  is_packet pkt = true ->
  eval_T pkt tree = scan pkt (lin_T tree).
Proof with auto.
intros.
assert (wf_tree (height tree) tree). apply wf_exists.
apply flatten_eval_T with (n := height tree)...
Qed.

End HFT.