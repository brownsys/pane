Require Import Coq.Lists.List.
Require Import Coq.Arith.MinMax.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Classes.EquivDec.
Require Import CpdtTactics.
Require Import Omega.
Require Import Coq.Bool.Bool.
Require Import Impl.

Record pkt : Set := mkPkt {
  pktSrcHost : nat;
  pktDstHost : nat;
  pktSrcPort : nat;
  pktDstPort : nat
}.

Record pat' : Set := mkPat' {
  patInPort : option port;
  patSrcHost : option nat;
  patDstHost : option nat;
  patSrcPort : option nat;
  patDstPort : option nat
}.

Inductive pat : Set :=
  | PatMatch : pat' -> pat
  | PatEmpty : pat.

Lemma pkt_eq_dec : forall (p p' : pkt), { p = p' } + { p <> p' }.
Proof. repeat decide equality. Qed.

Lemma pat_eq_dec : forall (p p' : pat), { p = p' } + { p <> p' }.
Proof. repeat decide equality. Qed.


Definition interAux (m : option nat) (n : option nat) := match (m, n) with
  | (Some m, Some n) => if beq_nat m n then Some (Some m) else None
  | (Some m, None) => Some (Some m)
  | (None, Some n) => Some (Some n)
  | (None, None) => Some None
end.

Definition intersect' (p p' : pat') := 
  match (interAux (patInPort p) (patInPort p'),
    interAux (patSrcHost p) (patSrcHost p'),
    interAux (patDstHost p) (patDstHost p'),
    interAux (patSrcPort p) (patSrcPort p'),
    interAux (patDstPort p) (patDstPort p')) with
    | (Some v, Some x, Some y, Some z, Some w) => Some (mkPat' v x y z w)
    | _ => None
  end.

Definition intersect (p p' : pat) := 
  match (p, p') with
    | (PatMatch q, PatMatch q') => 
      match intersect' q q' with
        | Some r => PatMatch r
        | None => PatEmpty
      end
    | _ => PatEmpty
  end.

Definition is_overlapped (p1 : pat) (p2 : pat) : bool :=
  match (intersect p1 p2) with
    | PatEmpty => false
    | PatMatch p3 => true
  end.

Definition exact_match (k : pkt) (n : port) := 
  PatMatch (mkPat' 
    (Some n)
    (Some (pktSrcHost k))
    (Some (pktDstHost k))
    (Some (pktSrcPort k))
    (Some (pktDstPort k))).

Definition is_match (k : pkt) (n : port) (t : pat) := 
  is_overlapped (exact_match k n) t.

Section Proofs.


  Lemma inter_Empty : forall t, intersect t PatEmpty = PatEmpty.
  Proof with auto.
    intros.
    destruct t...
  Qed.

  Lemma inter_empty_l : forall t, intersect PatEmpty t = PatEmpty.
  Proof with auto.
    intros.
    destruct t...
  Qed.

  Lemma interAux_comm' : forall x y z,
    Some z = interAux x y ->
    Some z = interAux y x.
  Proof with (auto with arith).
    intros.
    unfold interAux in *.
    repeat match goal with
    | [ H : option nat |- _ ] => destruct H
    | [ H : context [if beq_nat ?x ?y then _ else _] |- _ ] =>
      let Hx := fresh "Hx" in
        remember (beq_nat x y) as Hx;
        destruct Hx 
    | [ H : true = beq_nat ?x ?y |- _ ] =>
        symmetry in H; apply beq_nat_true in H
    end; crush.
    rewrite <- beq_nat_refl...
  Qed.

  
  Lemma interAux_comm : forall x y m n,
    interAux x y = Some m ->
    interAux y x = Some n ->
    m = n.
  Proof with (auto with arith).
    intros.
    unfold interAux in *.
    repeat match goal with
    | [ H : option nat |- _ ] => destruct H
    | [ H : context [if beq_nat ?x ?y then _ else _] |- _ ] =>
      let Hx := fresh "Hx" in
        remember (beq_nat x y) as Hx;
        destruct Hx 
    | [ H : true = beq_nat ?x ?y |- _ ] =>
        symmetry in H; apply beq_nat_true in H
    end; crush.
  Qed.


  Lemma inter_comm : forall t t', intersect t t' = intersect t' t.
  Proof with auto.
    intros.
    destruct t. destruct t'.
    unfold intersect.
    unfold intersect'.
    repeat match goal with
      | [ |- context [interAux ?x ?y] ] =>
        let Hx := fresh "Hx" in
        let Hy := fresh "Hy" in
        remember (interAux x y) as Hx;
        remember (interAux y x) as Hy;
        destruct Hx; destruct Hy
      | [ H1 : Some ?o1 = interAux ?a ?b,
          H2 : Some ?o2 = interAux ?b ?a
          |- _ ] => 
      let H := fresh "H" in
      assert (o1 = o2) as H by (apply interAux_comm with (x := a) (y := b); auto); clear H1; clear H2; rewrite -> H
      | [ H1 : Some _ = interAux ?x ?y,
          H2 : None = interAux ?y ?x
          |- _ ] => apply interAux_comm' in H1; rewrite <- H1 in H2; inversion H2 
    end; try reflexivity.
   apply inter_Empty.
   unfold intersect. destruct t'...
 Qed.

 Lemma interAux_assoc : forall x y z a b,
   Some a = interAux y z ->
   Some b = interAux x y ->
   interAux x a = interAux b z.
 Proof with auto.
   intros.
   unfold interAux in *.
    repeat match goal with
    | [ H : option nat |- _ ] => destruct H
    | [ H : context [if beq_nat ?x ?y then _ else _] |- _ ] =>
      let Hx := fresh "Hx" in
        remember (beq_nat x y) as Hx;
        destruct Hx 
    | [ H : true = beq_nat ?x ?y |- _ ] =>
        symmetry in H; apply beq_nat_true in H
    end; crush.
    rewrite <- beq_nat_refl...
    rewrite <- beq_nat_refl...
  Qed.

  Lemma interAux_assoc_contra : forall x y z a b,
    Some a = interAux y z ->
    Some b = interAux x a ->
    exists c, Some c = interAux x y.
  Proof with eauto.
   intros.
   unfold interAux in *.
    repeat match goal with
    | [ H : option nat |- _ ] => destruct H
    | [ H : context [if beq_nat ?x ?y then _ else _] |- _ ] =>
      let Hx := fresh "Hx" in
        remember (beq_nat x y) as Hx;
        destruct Hx 
    | [ H : true = beq_nat ?x ?y |- _ ] =>
        symmetry in H; apply beq_nat_true in H
    end; crush...
    rewrite <- beq_nat_refl...
    rewrite <- beq_nat_refl...
  Qed.

  Lemma interAux_assoc_contra2 : forall x y z a b,
    Some a = interAux x y ->
    Some b = interAux a z ->
    exists w, Some w = interAux y z.
  Proof with eauto.
   intros.
   unfold interAux in *.
    repeat match goal with
    | [ H : option nat |- _ ] => destruct H
    | [ H : context [if beq_nat ?x ?y then _ else _] |- _ ] =>
      let Hx := fresh "Hx" in
        remember (beq_nat x y) as Hx;
        destruct Hx 
    | [ H : true = beq_nat ?x ?y |- _ ] =>
        symmetry in H; apply beq_nat_true in H
    end; crush...
    rewrite <- beq_nat_refl...
    rewrite <- beq_nat_refl...
  Qed.

  Ltac contra_inter_assoc := match goal with
      | [ H1 : Some ?a' = interAux ?y ?z',
          H2 : Some ?b' = interAux ?x ?a',
          H3 : None = interAux ?x ?y |- _ ]
        => let H := fresh "H" in
           assert (exists w, Some w = interAux x y) as H by (apply interAux_assoc_contra with (z := z') (a := a') (b := b'); auto); destruct H; rewrite <- H in H3; inversion H3
      | [ H2 : None = interAux ?y' ?z',
          H1 : Some ?a' = interAux ?x' ?y',
          H3 : Some ?b' = interAux ?a' ?z' |- _ ]
        => let H := fresh "H" in
           assert (exists w, Some w = interAux y' z') as H by (apply interAux_assoc_contra2 with (x := x') (a := a') (b := b'); auto); destruct H; rewrite <- H in H2; inversion H2
             end.

  Lemma inter_assoc : forall t1 t2 t3, 
    intersect t1 (intersect t2 t3) = intersect (intersect t1 t2) t3.
  Proof with auto.
    intros.
    unfold intersect.
    unfold intersect'.   
    destruct t1; destruct t2; destruct t3; auto;
    simpl;
    repeat match goal with
      | [ |- context [interAux ?x ?y] ] =>
        let H := fresh "H" in
        remember (interAux x y) as H;
        destruct H; simpl
      | [ H1 : Some ?a = interAux ?y' ?z,
          H2 : Some ?b = interAux ?x ?y' |- _ ] 
        => assert (interAux x a = interAux b z) by (apply interAux_assoc with (y := y'); auto); clear H1; clear H2
      | [ H1 : interAux _ _  = interAux _ _  |- _ ]
        => rewrite <- H1 in *; rewrite <- H1; clear H1
      | [ H1: (interAux _ _) = (interAux _ _)  |- _ ]
        => rewrite -> H1 in *; try (rewrite -> H1); clear H1
      | [ H1 : Some ?x = ?e,
          H2 : Some ?y = ?e |- _ ] =>
        rewrite <- H1 in H2; clear H1; inversion H2; clear H2; subst
      | [ H1 : Some ?x = ?e,
          H2 : None = ?e |- _ ] =>
        rewrite <- H1 in H2; inversion H2
    end; try reflexivity.
    contra_inter_assoc.
    contra_inter_assoc.
    contra_inter_assoc.
    contra_inter_assoc.
    contra_inter_assoc.
    contra_inter_assoc.
    contra_inter_assoc.
    contra_inter_assoc.
    contra_inter_assoc.
    contra_inter_assoc.
  Qed.

  Lemma no_match_subset_r : forall k n t t',
    is_match k n t = false ->
    is_match k n (intersect t' t) = false.
  Proof with (auto with datatypes).
    intros.
    unfold is_match in *.
    unfold is_overlapped in *.
    assert (intersect (exact_match k n) t = PatEmpty).
      remember (intersect (exact_match k n) t).
      destruct p. inversion H. trivial.
    clear H.
    assert (intersect t' t = intersect t t') by apply inter_comm.
    rewrite -> H.
    rewrite -> inter_assoc.
    rewrite -> H0.
    rewrite -> inter_comm.
    rewrite -> inter_Empty...
  Qed.

  Lemma no_match_subset_l : forall k n t t',
    is_match k n t = false ->
    is_match k n (intersect t t') = false.
  Proof with auto.
    intros.
    unfold is_match in *.
    unfold is_overlapped in *.
    assert (intersect (exact_match k n) t = PatEmpty).
      remember (intersect (exact_match k n) t).
      destruct p. inversion H. trivial.
    clear H.
    rewrite -> inter_assoc.
    rewrite -> H0.
    rewrite -> inter_empty_l...
  Qed.

  Lemma exact_intersect : forall k n t,
    is_match k n t = true ->
    intersect (exact_match k n) t = exact_match k n.
  Proof with eauto.
    intros.
    unfold is_match in H.
    unfold is_overlapped in H.
    assert (exists z, intersect (exact_match k n) t = PatMatch z).
      remember (intersect (exact_match k n) t).
      destruct p... inversion H.
    clear H.
    destruct H0 as [z H].
    assert (exists z', t = PatMatch z').
      unfold intersect in H.
      destruct (exact_match k n)...
      destruct t... inversion H.
    destruct H0 as [z' H0].
    subst.
    unfold exact_match in H.
    destruct z.
    destruct z'.
    unfold intersect in H.
    unfold intersect' in H.
    simpl in H.
    assert (exists v, interAux (Some n) patInPort1 = Some v).
    destruct (interAux (Some n) patInPort1)... inversion H.
    destruct H0 as [patInPort1v H0].
    rewrite -> H0  in H.

    assert (exists v, interAux (Some (pktSrcHost k)) patSrcHost1 = Some v).
    destruct (interAux (Some (pktSrcHost k)) patSrcHost1)...
    inversion H.
    destruct H1 as [pktSrcHostv H1].
    rewrite -> H1 in H.

    assert (exists v, interAux (Some (pktDstHost k)) patDstHost1 = Some v).
    destruct (interAux (Some (pktDstHost k)) patDstHost1)...
    inversion H.
    destruct H2 as [pktDstHostv H2].
    rewrite -> H2 in H.

    assert (exists v, interAux (Some (pktSrcPort k)) patSrcPort1 = Some v).
    destruct (interAux (Some (pktSrcPort k)) patSrcPort1)...
    inversion H.
    destruct H3 as [pktSrcPortv H3].
    rewrite -> H3 in H.

    assert (exists v, interAux (Some (pktDstPort k)) patDstPort1 = Some v).
    destruct (interAux (Some (pktDstPort k)) patDstPort1)...
    inversion H.
    destruct H4 as [pktDstPortv H4].
    rewrite -> H4 in H.
    inversion H.
    subst.
    clear H. (* H0 H1 H2 H3 H4.  *)
    unfold exact_match.
    unfold intersect.
    unfold intersect'.
    simpl.
    rewrite -> H0.
    rewrite -> H1.
    rewrite -> H2.
    rewrite -> H3.
    rewrite -> H4.
    unfold interAux in *.
    repeat match goal with
      | [ H : (match ?v with | Some _ => _ | None => _ end) = Some _ |- _ ] =>
        destruct v
      | [ H :  (if beq_nat ?x ?y then Some _ else None) = Some _ |- _ ] =>
        destruct (beq_nat x y)
      | [ H : None = Some _ |- _ ] =>
        inversion H
      | [ H : Some (Some ?x) = Some ?y |- _ ] =>
        inversion H; clear H
           end; reflexivity.
  Qed.

  Lemma pkt_match_intersect : forall k n t t',
    is_match k n t = true ->
    is_match k n t' = true ->
      is_match k n (intersect t t') = true.
  Proof with auto.
    intros.
    apply exact_intersect in H.
    apply exact_intersect in H0.
    unfold is_match.
    unfold is_overlapped.
    rewrite -> inter_assoc.
    rewrite -> H.
    rewrite -> H0.
    unfold exact_match...
  Qed.

  Lemma packet_split : forall k n t t',
    is_overlapped t t' = false ->
    is_match k n t = true ->
    is_match k n t' = false.
  Proof with eauto.
    intros.
(*    apply exact_intersect in H0. *)
    unfold is_match.
    unfold is_overlapped.
    remember (intersect (exact_match k n) t').
      destruct p...
    assert (is_match k n t' = true).
      unfold is_match.
      unfold is_overlapped.
      rewrite <- Heqp...
    assert (is_match k n (intersect t t') = true).
      apply pkt_match_intersect...
    (* have contradiction here *)
    unfold is_overlapped in H.
    assert (intersect t t' = PatEmpty).
      remember (intersect t t').
      destruct p0... inversion H.
    rewrite -> H3 in H2.
    unfold is_match in H2.
    unfold is_overlapped in H2.
    simpl in H2...
  Qed.

End Proofs.

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

