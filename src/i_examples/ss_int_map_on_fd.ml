(* a map from small string to int, backed by file -------------------------- *)

open Prelude
open Btree_api
open Small_string
open Example_keys_and_values

module S = struct
  type k = Small_string.t
  type v = int
  let pp = ss_int_pp
  let sz = 4096
  let compare_k = Small_string.compare
end

module M = Map_on_fd.Make(S)  

include M  
