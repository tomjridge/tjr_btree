(* a map from int to int, backed by file ------------------------------- *)
open Prelude
open Btree_api

open Example_keys_and_values

module S = struct
  type k = int
  type v = int
  let pp = int_int_pp
  let sz = Default.default_size
  let compare_k = Int.compare
end

include S

module M = Map_on_fd.Make(S)  

include M  
