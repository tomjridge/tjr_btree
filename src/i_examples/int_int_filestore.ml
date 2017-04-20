(* a map from int to int, backed by file ------------------------------- *)
open Prelude
open Btree_api

module G = Generic_kv_store

module Uncached = G.Make_uncached (struct 
    open G
    type k = int 
    type v = int
    let ps = {
      pp=Map_int_int.pp;
      compare_k=Pervasives.compare;
      equal_v=(=);
    }
  end)

