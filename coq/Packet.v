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

