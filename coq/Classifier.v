Require Import Coq.Init.Datatypes.
Require Import Coq.Lists.List.
Require Import Coq.Arith.MinMax.
Require Import Omega.
Require Import CpdtTactics.
Require Import Packet.
Require Import Action.

Module MakeClassifier (Import P : Packet) (Import Act : ACTION).

  Definition N := list (M * A).

  Fixpoint scan (pkt : M) (n : N) := match n with
    | nil => None
    | (m,a) :: tl => match is_overlapped m pkt with
      | true => a
      | false => scan pkt tl
    end
  end.



  Definition inter_entry (f : A -> A -> A) (e1 : M * A) (e2 : M * A) := match (e1, e2) with
    | ((m,a), (m',a')) => (intersect m' m, f a a')
   end.

  Definition inter_helper (f : A -> A -> A) (n : N) (ma : M * A) (r : N) :=
    (map (inter_entry f ma) n) ++ r.

  Definition inter (f : A -> A -> A) (n1 : N) (n2 : N) :=
    fold_right (inter_helper f n2) nil n1.

  Definition union (f : A -> A -> A) (n1 : N) (n2 : N) : N := inter f n1 n2 ++ n1 ++ n2.

  Section Lemmas.

  Variable f : A -> A -> A.

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

  Ltac destruct_M_A := match goal with
    | [ H : (M * A)%type |- _ ] => destruct H; simpl in *
    | _ => idtac "no hypotheses of the form M * A"
  end.

  Ltac destruct_is_overlapped B := match goal with
    | [ H : context[is_overlapped ?m ?pkt] |- _ ] => 
        idtac "destructing"  m pkt; remember (is_overlapped m pkt) as B; destruct B
    | [ |- context[is_overlapped ?m ?pkt] ] => 
       idtac "destructing"  m pkt;  remember (is_overlapped m pkt) as B; destruct B
  end.

  Ltac solve_is_overlapped := 
    let x := fresh "B" in
      destruct_M_A; destruct_is_overlapped x.

  Lemma scan_alg : forall (pkt : M) (a : A) (n : N),
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
solve_is_overlapped.
(* Case 1 *)
right. exists nil. exists N1. exists m. exists a. crush.
(* Case 2 *)
left. crush. apply H0 in H2. crush.
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
solve_is_overlapped.
idtac.
assert (is_overlapped m pkt = false). eapply H...
crush.
crush.
Qed.

Hint Resolve overlap_intersect.
Hint Rewrite intersect_comm.

Lemma eval_inter_step : forall (N1 N2 : N) (m pkt : M) (a : A),
  is_overlapped m pkt = false ->
  scan pkt (inter_helper f N1 (m, a) N2) = scan pkt N2.
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
  inter f nil n = nil.
Proof. intros. induction n; crush. Qed.

Lemma inter_nil_r : forall (n : N), inter f n nil = nil.
Proof. intros. induction n; crush. Qed.

Hint Resolve inter_nil_l inter_nil_r.

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
solve_is_overlapped...
idtac.
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

Lemma elim_inter_head : forall (N1 N2 : N) (pkt m : M)
                               (a : A),
  is_overlapped m pkt = false ->
  scan pkt (map (inter_entry f (m, a)) N1 ++ N2) = scan pkt N2.
Proof with auto with packet.
intros.
induction N1. crush.
solve_is_overlapped. inversion H.
rewrite -> intersect_comm.
autorewrite with packet using auto...
Qed.

Hint Resolve elim_inter_head.

Lemma inter_empty_aux : forall (N1 : N) (m m0 pkt : M) (a a0 : A),
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

Lemma inter_empty : forall (N2 : N) (pkt : M),
  is_packet pkt = true ->
  (forall (m: M) (a : A), In (m,a) N2 -> is_overlapped m pkt = false) ->
  (forall (N1 : N) (m: M) (a : A), In (m,a) (inter f N1 N2) -> 
                          is_overlapped m pkt = false).
Proof with auto with datatypes.
intros N2 pkt.
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
apply inter_empty_aux with (N1 := N2) (m0 := m0)  (a := a) (a0 := a0)...
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
  | [ pkt : M, tbl : N |- _ ] =>
    let H := fresh "H" in
    assert (scan_off pkt tbl) as H by (apply scan_off);
    destruct_scan' H
end.

Lemma union_comm : forall (n1 n2 : N) (pkt : M),
  is_packet pkt = true ->
  well_behaved f ->
  scan pkt (union f n1 n2) = f (scan pkt n1) (scan pkt n2).
Proof with simpl; eauto with datatypes.
intros n1 n2 pkt Hpacket H.
intros.
remember H as Hwb.
destruct H as [H H0].
induction n1.
(* Base case *)
crush.
(* Inductive case *)
unfold union.
destruct a.
remember (is_overlapped m pkt).
assert (scan_inv pkt (inter f ((m, a) :: n1) n2 ++ ((m, a) :: n1) ++ n2)).
  crush.
destruct_scan' H1.
  (* Case: scan falls off the table. *)
  remember (inter f ((m, a) :: n1) n2) as l1.
  remember (n1 ++ n2) as l2.
  rewrite -> HUnit.
  remember (l1 ++ (((m,a)::n1) ++ n2)) as L.
  assert (scan pkt n2 = None) as HscanLeft.
    assert (scan_inv pkt n2)...
    destruct_scan' H3.
    trivial.
    assert (In (m0, a0) L). crush.
    apply HNotIn0 in H1.
    crush.
  assert (scan pkt ((m,a)::n1) = None) as HscanRight.
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

Lemma inter_elim : forall 
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
simpl. rewrite -> H0. crush.
Qed.

End Lemmas.

End MakeClassifier.
