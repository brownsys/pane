Require Import Coq.Init.Datatypes.
Require Import Coq.Lists.List.
Require Import Coq.Arith.MinMax.
Require Import Coq.Classes.Equivalence.
Require Import Omega.
Require Import CpdtTactics.
Require Import Impl.

Module MakeClassifier (Import TheImpl : IMPL).

  Definition N := list (pat * A).

  Fixpoint scan (pkt : pkt) (n : N) := match n with
    | nil => ActionUnit
    | (m,a) :: tl => match is_match pkt m with
      | true => a
      | false => scan pkt tl
    end
  end.

  Definition inter_entry (f : A -> A -> A) (e1 : pat * A) (e2 : pat * A) := 
    match (e1, e2) with
      | ((m,a), (m',a')) => (intersect m' m, f a a')
    end.

  Definition inter (f : A -> A -> A) (n1 : N) (n2 : N) :=
    fold_right (fun ma acc => map (inter_entry f ma) n2 ++ acc) nil n1.

  Definition union (f : A -> A -> A) (n1 : N) (n2 : N) : N := 
    inter f n1 n2 ++ n1 ++ n2.

  Section Lemmas.

  Variable f : A -> A -> A.

  Inductive scan_rel : pkt -> N -> A -> Prop :=
    | scan_empty : forall pkt, scan_rel pkt nil ActionUnit
    | scan_head : forall pkt m  (a : A) (tl : N),
        is_match pkt m = true ->
        scan_rel pkt ((m,a)::tl) a
    | scan_tail : forall pkt m (a a' : A) (tl : N),
        is_match pkt m = false ->
        scan_rel pkt tl a' ->
        scan_rel pkt ((m,a)::tl) a'.

  Hint Constructors scan_rel.

  Ltac destruct_M_A := match goal with
    | [ H : (pat * A)%type |- _ ] => destruct H; simpl in *
    | [ H : (pkt * A)%type |- _ ] => destruct H; simpl in *
    | _ => idtac "no hypotheses of the form M * A"
  end.

  Ltac destruct_is_overlapped B := match goal with
    | [ H : context[is_overlapped ?m ?pkt] |- _ ] => 
        idtac "destructing"  m pkt; remember (is_overlapped m pkt) as B; destruct B
    | [ |- context[is_overlapped ?m ?pkt] ] => 
       idtac "destructing"  m pkt;  remember (is_overlapped m pkt) as B; destruct B
    | [ H : context[is_match ?pkt ?m] |- _ ] => 
        idtac "destructing"  m pkt; remember (is_match pkt m) as B; destruct B
    | [ |- context[is_overlapped ?m ?pkt] ] => 
       idtac "destructing"  m pkt;  remember (is_match pkt m) as B; destruct B

  end.

  Ltac solve_is_overlapped := 
    let x := fresh "B" in
      destruct_M_A; destruct_is_overlapped x.

  Lemma scan_alg : forall pkt (a : A) (n : N),
    scan pkt n = a <-> scan_rel pkt n a.
  Proof with subst; eauto.
    split; intros.
    induction n.
    (* Case 1 *)
    crush.
    (* Case 2 *)
    solve_is_overlapped...
    (* Case 3 *)
    induction n.
    inversion H; crush.
    destruct a0; inversion H; subst.
    crush.
    crush.
  Qed.

Definition scan_inv (pkt : pkt) (N1 : N) :=
  ((forall (m : pat) (a : A), 
    In (m,a) N1 -> is_match pkt m = false) /\
   scan pkt N1 = ActionUnit) \/
  (exists N2 : N, exists N3 : N, exists m : pat, exists a : A,
   N1 = N2 ++ (m,a)::N3 /\
   is_match pkt m  = true /\
   scan pkt N1 = a /\
   (forall (m' : pat) (a' : A), In (m',a') N2 -> is_match pkt m' = false)).

Lemma scan_off : forall (pkt : pkt) (N1 : N), scan_inv pkt N1.
Proof with intros; simpl; auto with datatypes.
  unfold scan_inv.
  intros.
  induction N1.
  (* Base case *)
  crush.
  (* Inductive case *)
  destruct a.
  destruct IHN1.
  (* Case 1 *)
  remember (is_match pkt0 p).  destruct b.  
  right. exists nil. exists N1. exists p. exists a. crush. rewrite <- Heqb...
  (* Case 2 *)
  left. crush. apply H0 in H2. crush. rewrite <- Heqb...
  (* Case 3 *)
  destruct H as [N2 [N3 [m [a' [Neq  [Hov [Ha'eq H]]]]]]].
 remember (is_match pkt0 p) as b. destruct b. 
  right. exists nil. exists N1. exists p. exists a. crush. rewrite <- Heqb...
  right. exists ((p,a) :: N2). exists N3. exists m. exists a'.
   crush. rewrite <- Heqb... apply H in H1. crush.
Qed.

Lemma elim_mismatch : forall (n1 n2 : N) p m  (a : A),
  is_match p m = false ->
  scan p (n1 ++ (m,a)::n2) = scan p (n1 ++ n2).
Proof. intros. induction n1; crush. Qed.

Hint Unfold union inter inter_entry.

Lemma inter_nil_l : forall (n : N),
  inter f nil n = nil.
Proof. intros. induction n; crush. Qed.

Lemma inter_nil_r : forall (n : N), inter f n nil = nil.
Proof. intros. induction n; crush. Qed.

Hint Resolve inter_nil_l inter_nil_r.

Lemma elim_scan_head : forall (N1 N2 : N) pkt,
  (forall m (a : A), In (m,a) N1 -> is_match pkt m = false) ->
  scan pkt (N1 ++ N2) = scan pkt N2.
Proof with simpl; auto with datatypes.
intros.
induction N1...
destruct a.
assert (forall m a', In (m,a') N1 -> is_match pkt0 m = false).
  intros. apply H with (a0 := a')...
apply IHN1 in H0.
assert (is_match pkt0 p = false).
  assert (In (p,a) ((p,a)::N1))... 
  apply H in H1...
rewrite -> H1...
Qed.

Hint Resolve elim_scan_head.

Lemma elim_scan_middle : forall (N1 N2 N3 : N) pkt,
  (forall m (a : A), In (m,a) N2 -> is_match pkt m = false) ->
  scan pkt (N1 ++ N2 ++ N3) = scan pkt (N1 ++ N3).
Proof. intros. generalize dependent N2. induction N1; crush. Qed.

Lemma elim_scan_mid : forall (N1 N2 : N) pkt m (a : A),
  is_match pkt m = false ->
  scan pkt (N1 ++ (m,a) :: N2) = scan pkt (N1 ++ N2).
Proof. intros. induction N1; crush. Qed.


Lemma elim_inter_head : forall (N1 N2 : N) pkt m
                               (a : A),
  is_match pkt m = false ->
  scan pkt (map (inter_entry f (m, a)) N1 ++ N2) = scan pkt N2.
Proof with auto.
intros.
induction N1. crush.
solve_is_overlapped. idtac. inversion H.
autorewrite with packet using auto...
Qed.

Hint Resolve elim_inter_head.

Lemma inter_empty_aux : forall (N1 : N) m m0 pkt (a a0 : A),
  (forall m  (a : A), In (m,a) N1 -> is_match pkt m = false) ->
  In (m,a) (map (inter_entry f (m0,a0)) N1) ->
  is_match pkt m = false.
Proof with auto with datatypes.
intros.
induction N1.
crush.
destruct a1.
simpl in H0.
destruct H0.
(* contra *)
unfold inter_entry in H0. inversion H0. subst.
assert (is_match pkt0 p = false). apply H with (a := a1)...
apply no_match_subset_l...
(* inductive *)
assert (forall m' (a' : A), In (m',a') N1 -> is_match pkt0 m' = false).
  intros. 
  assert (In (m',a') ((p,a1)::N1))...
  apply H in H2...
apply IHN1 in H1...
Qed.

Lemma inter_empty : forall (N2 : N) pkt,
  (forall m (a : A), In (m,a) N2 -> is_match pkt m = false) ->
  (forall (N1 : N) m (a : A), In (m,a) (inter f N1 N2) -> 
                          is_match pkt m = false).
Proof with auto with datatypes.
intros N2 pkt.
intros Hlap.
intros.
generalize dependent N2.
induction N1.
crush.
(* Inductive *)
destruct a0.
intros.
simpl in H.
rewrite -> in_app_iff in H.
destruct H.
apply inter_empty_aux with (N1 := N2) (m0 := p)  (a := a) (a0 := a0)...
apply IHN1 in Hlap...
Qed.

Hint Resolve inter_empty scan_off.
Hint Rewrite in_app_iff.

Ltac destruct_scan' H := match goal with
  | [ H : scan_inv ?pkt ?tbl |- _ ] =>
    let HNotIn := fresh "HNotIn" in
    let HUnit := fresh "HUnit" in
    let Npre := fresh "Npre" in
    let m := fresh "m" in
    let a := fresh "a" in
    let Npost := fresh "Npost" in
    let HEq := fresh "HEq" in
    let HOverlap := fresh "HOverlap" in
    let HResult := fresh "HResult" in
    let HNotIn := fresh "HNotIn" in
    unfold scan_inv in H;
    destruct H as [[HNotIn HUnit] | 
                    [Npre [Npost [m [a [HEq [HOverlap [HResult HNotIn]]]]]]]]
end.

Ltac destruct_scan pkt tbl := match goal with
  | [ pkt : pkt, tbl : N |- _ ] =>
    let H := fresh "H" in
    assert (scan_off pkt tbl) as H by (apply scan_off);
    destruct_scan' H
end.

Open Local Scope equiv_scope.

Lemma union_comm : forall (n1 n2 : N) pkt,
  well_behaved f ->
  scan pkt (union f n1 n2) === f (scan pkt n1) (scan pkt n2).
Proof with simpl; eauto with datatypes.
intros n1 n2 pkt H.
intros.
remember H as Hwb.
destruct H as [H [H0 H0']].
induction n1.
(* Base case *)
rewrite -> H0... 
apply reflexivity.
(* Inductive case *)
unfold union.
destruct a.
rename p into m.
remember (is_match pkt m).
assert (scan_inv pkt (inter f ((m, a) :: n1) n2 ++ ((m, a) :: n1) ++ n2)).
  crush.
destruct_scan' H1.
  (* Case: scan falls off the table. *)
  remember (inter f ((m, a) :: n1) n2) as l1.
  remember (n1 ++ n2) as l2.
  rewrite -> HUnit.
  remember (l1 ++ (((m,a)::n1) ++ n2)) as L.
  assert (scan pkt n2 = ActionUnit) as HscanLeft.
    assert (scan_inv pkt n2)...
    destruct_scan' H3.
    trivial.
    assert (In (m0, a0) L). crush.
    apply HNotIn0 in H1.
    crush.
  assert (scan pkt ((m,a)::n1) = ActionUnit) as HscanRight.
    assert (scan_inv pkt ((m,a)::n1))... 
    destruct_scan' H3.
    trivial.
    assert (In (m0,a0) L).  
      rewrite -> HeqL. rewrite -> HEq. rewrite -> in_app_iff. crush.
    apply HNotIn0 in H1.
    crush.
  crush.
  (* Case: scan does not fall off the table. *)
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
  assert (is_match pkt m = false).
    apply packet_split with (m1 := m0)...
  simpl. rewrite <- app_assoc.  

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
  idtac.
  rewrite <- app_comm_cons.
  destruct b.
  Focus 2. (* Packet is not in m, like the case above *) idtac.
  simpl.
  rewrite <- Heqb.
  assert (scan pkt ((map (inter_entry f (m, a)) n2 ++  (inter f n1 n2)) ++ (m, a) :: n1 ++ n2)
          = scan pkt ((map (inter_entry f (m, a)) n2 ++ (inter f n1 n2)) ++ n1 ++ n2)).
    apply elim_scan_mid. symmetry. exact Heqb.
  rewrite -> H1.
  rewrite <- app_assoc.
  assert (scan pkt (map (inter_entry f (m, a)) n2 ++ inter f n1 n2 ++ n1 ++ n2) = 
          scan pkt (inter f n1 n2 ++ n1 ++ n2)).
    apply elim_inter_head. symmetry. exact Heqb.
  rewrite -> H2.
  unfold union in IHn1.
  exact IHn1.

  (* Case where pkt is in m *)
  idtac.
  assert (scan_inv pkt n2). apply scan_off...
  unfold scan_inv in H1.
  destruct H1.
  destruct H1.
  rewrite -> H2.
  assert (forall m'  (a' : A), In (m',a') (inter f ((m, a) :: n1) n2) ->
                 is_match pkt m' = false).
    apply inter_empty. exact H1.
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
  assert (f (scan pkt ((m,a)::n1)) ActionUnit === scan pkt ((m,a)::n1)).
    apply H.
  rewrite -> H8.
  reflexivity.

  (* Case where: we are in the intersection at the tip *)
  simpl.
  rewrite <- Heqb.
  rewrite <- app_assoc.
  destruct H1 as [N2' [N3' [m' [a' [Heq' [Hlap' [Hscan' Hlap2']]]]]]].
  rewrite -> Hscan'.
  rewrite -> Heq'.
  rewrite -> map_app.
  rewrite <- app_assoc.
  remember ( inter f n1 (N2' ++ (m', a') :: N3') ++
      (m, a) :: n1 ++ N2' ++ (m', a') :: N3') as Trash.
  assert (forall m5 (a5 : A), 
          In (m5,a5) (map (inter_entry f (m,a)) N2') ->
          is_match pkt m5  = false).
    assert (map (inter_entry f (m,a)) N2' = inter f ((m,a) ::nil) N2').
      simpl. rewrite -> app_nil_r. reflexivity.
  
  rewrite -> H1.
    apply inter_empty.  exact Hlap2'.
  assert (scan pkt (map (inter_entry f (m, a)) N2' ++
            ((map (inter_entry f (m, a)) ((m', a') :: N3')) ++ Trash)) =
           scan pkt (map (inter_entry f (m, a)) ((m', a') :: N3') ++ Trash)).
     apply elim_scan_head. exact H1.
    rewrite -> H2.
  simpl.
  assert (is_match pkt (intersect m' m) = true).
    apply pkt_match_intersect...
  rewrite -> H3...
  apply reflexivity.
Qed. 

Lemma inter_elim : forall 
  m pkt (a : A) (L1 L2 : N),
  is_match pkt m = false ->
  scan pkt (inter f ((m, a) :: nil) L1 ++ L2) =
  scan pkt L2.
Proof with auto.
intros.
induction L1.
crush.
destruct a0. 
assert (is_match pkt0 (intersect p m) = false).
  apply no_match_subset_r...
simpl. rewrite -> H0. crush.
Qed.

End Lemmas.

End MakeClassifier.
