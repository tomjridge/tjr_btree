(* map from int to int *)

open Btree_util

(* assumptions ---------------------------------------- *)

let int_size = 4  (* bytes *)


(* KV, C, STORE, FT --------------------------------------- *)

(* for ints *)
module KV = struct
  type key = int[@@deriving yojson]
  type value = int[@@deriving yojson]
  let key_ord (x:int) y = Pervasives.compare x y
  let equal_value : value -> value -> bool = (=)
end

let _ = (module KV : Internal_api.KEY_VALUE)


(* NB page=string *)
module type STORE = Internal_api.Simple.STORE


module Make = functor (ST:STORE) -> struct
  module ST = ST
  module Btree_simple_internal = Btree_simple_internal.Make(struct
    module KV=KV
    module ST=ST
    open KV
    open Internal_api.Pickle_params
    let pp = Pickle.Examples.{
        p_k = p_int;
        u_k = u_int;
        k_len = 4;
        p_v = p_int;
        u_v = u_int;
        v_len = 4;
      }
  end)
  let _ = (module Btree_simple_internal.Btree.Raw_map : Internal_api.RAW_MAP)
end


