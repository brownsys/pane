Require Import NetCore.

Extraction Language Haskell.

Extract Inductive bool => "Prelude.Bool" [ "Prelude.True" "Prelude.False" ].
Extract Inductive nat => "Prelude.Integer" [ "0" "Prelude.succ" ]
"(\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))".
Extract Inductive comparison => "Prelude.Ordering" [ "Prelude.EQ" "Prelude.LT" "Prelude.GT" ].
Extract Inductive prod => "(,)"  [ "(,)" ].
Extract Inductive option => "Prelude.Maybe" [ "Prelude.Just" "Prelude.Nothing" ].
Extract Inductive list => "[]" [ "[]" "(:)" ]. 
Recursive Extraction Library NetCore.
