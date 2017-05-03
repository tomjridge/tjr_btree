(** Typical pickling parameters used by the B-tree. Includes
   information on the length (in bytes) of a pickled key and value. *)
open Pickle

(** Pickling parameters including length information. *)
type ('k,'v) pp = {
  p_k: 'k -> P.m;
  u_k: 'k U.m;
  k_len: int;
  p_v: 'v -> P.m;
  u_v: 'v U.m;
  v_len: int      
}
