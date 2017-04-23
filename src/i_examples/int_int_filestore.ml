(* a map from int to int, backed by file ------------------------------- *)
open Prelude
open Btree_api

open Example_keys_and_values

module G = Generic_kv_store

module Uncached = G.Make_uncached (struct 
    open G
    type k = int 
    type v = int
    let ps = {
      pp=int_int_pp;
      kv_ops={
        compare_k=Pervasives.compare;
        equal_v=(=);
      }
    }
  end)

