(* a map from int to int, backed by file ------------------------------- *)
open Prelude
open Btree_api

open Example_keys_and_values

module G = Generic_kv_store

(* FIXME put elsewhere? factor into mk_ps1? *)
(* sz is page_size *)
let cs sz = Constants.make_constants sz 4 4 4

let mk_ps1 sz = G.mk_ps1 (cs sz) Int.compare Example_keys_and_values.int_int_pp

let mk_unchecked_map_ops sz = G.mk_unchecked_map_ops (mk_ps1 sz)  (* FIXME unchecked *)
