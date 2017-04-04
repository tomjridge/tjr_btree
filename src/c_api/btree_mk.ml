(* construct map from store ---------------------------------------- *)

open Btree_api

module type S = sig
  type k 
  type v 
  val ps0: (k,v) params

  type t
  val store: (k,v,t) store
end

module M = functor (S:S) -> struct
  module S = S
  module P = struct
    type k = S.k
    type v = S.v
    let compare_k = S.ps0.compare_k
    let equal_v = S.ps0.equal_v
    type store = S.t
                   
  end
  module IU = Isa_util.Make(S)
end
