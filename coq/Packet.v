Module Type Packet.

  Parameter pkt : Set.
  Parameter pat : Set.

  Axiom pkt_eq_dec : forall (m1 m2 : pkt), { m1 = m2 } + { m1 <> m2 }.
  Axiom pat_eq_dec : forall (m1 m2 : pat), { m1 = m2 } + { m1 <> m2 }.

  Parameter intersect : pat -> pat -> pat.
  Parameter is_overlapped : pat -> pat -> bool.
  Parameter is_match : pkt -> pat -> bool.
  Parameter exact_match : pkt -> pat.

  Axiom intersect_comm : forall m1 m2,
    intersect m1 m2 = intersect m2 m1.
  Axiom overlap_intersect : forall m1 m2 m3,
    is_overlapped m1 m2 = false -> is_overlapped (intersect m1 m3) m2 = false.
  Axiom overlap_intersect_both : forall m1 m2 m3,
    is_overlapped m1 m3 = true -> 
    is_overlapped m2 m3 = true ->
    is_overlapped (intersect m1 m2) m3 = true.

  Axiom no_match_subset_l : forall pkt m1 m2,
    is_match pkt m1 = false ->
    is_match pkt (intersect m1 m2) = false.
  
  Axiom no_match_subset_r : forall pkt m1 m2,
    is_match pkt m2 = false ->
    is_match pkt (intersect m1 m2) = false.
  
  Axiom pkt_match_intersect : forall pkt m m',
    is_match pkt m = true -> 
    is_match pkt m' = true ->
    is_match pkt (intersect m m') = true.

  Axiom packet_split : forall pkt m1 m2,
    is_overlapped m1 m2 = false ->
    is_match pkt m1 = true  ->
    is_match pkt m2 = false.
  Axiom overlap_inter_1 : forall m0 m1 m2,
    is_overlapped m1 m2 = false ->
    is_overlapped (intersect m1 m0) m2 = false.

 Hint Resolve intersect_comm : packet.
 Hint Rewrite overlap_intersect overlap_intersect_both
              pkt_eq_dec pat_eq_dec packet_split overlap_inter_1 
              no_match_subset_l no_match_subset_r : packet.
End Packet.

