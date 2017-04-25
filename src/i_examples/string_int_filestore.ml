(* a map from int to int, backed by file ------------------------------- *)
open Prelude
open Btree_api
open Small_string
open Example_keys_and_values

module G = Generic_kv_store

module Uncached = G.Make_uncached (struct 
    open G
    type k = SS_.t
    type v = int
    let ps = {
      pp=ss_int_pp;
      kv_ops={
        compare_k=Pervasives.compare;
        equal_v=(=);
      }
    }
  end)

