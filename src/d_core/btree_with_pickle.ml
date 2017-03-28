(* btree_with_pickle ---------------------------------------- *)

(* like btree, but with pickling rather than some other frame -> page
   mapping *)

open Prelude
open Btree_api


module Pickle_params = struct
  open Pickle
  type ('k,'v) t = {
    p_k: 'k -> P.m;
    u_k: 'k U.m;
    k_len: int;
    p_v: 'v -> P.m;
    u_v: 'v U.m;
    v_len: int      
  }
end  

module type CONSTANTS = Btree.CONSTANTS

(* a refinement of Btree.STORE *)
module type STORE = sig
  module Page: BLK_LIKE with type t = string
  open Page
  type t
  type 'a m = ('a,t) Sem.m
  val free: r list -> unit m  (* free list *)
  val alloc: Page.t -> r m
  val page_ref_to_page: r -> Page.t m
  val store_sync: unit m
  include MONAD with type 'a m := 'a m
end

(* check STORE is a refinement of Btree.STORE *)
module X_ = functor (S:STORE) -> struct
  module ST_ : Btree.STORE = S
end

module type S = sig
  module KV: KEY_VALUE
  module ST: STORE    
  open KV
  val pp : (key,value) Pickle_params.t
end

module Make = functor (S:S) -> (struct

    module S = S

    module S_ = (struct
      module KV = S.KV
      module ST = struct 
        include S.ST
        type page = Page.t
        type page_ref = Page.r [@@deriving yojson]
        type store = t
        let dest_Store: store -> page_ref -> page = failwith "FIXME"
      end

      module C : CONSTANTS = struct
        open S
        module Page = ST.Page
        let pp = S.pp
        let max_leaf_size = 
          (Page.sz - 4 - 4) (* for tag and length *)
          / (pp.k_len+pp.v_len)
        let max_node_keys =
          (Page.sz - 4 - 4 - 4 (* tag, length x 2 *)
           - pp.v_len) (* always one val more than # keys *)
          / (pp.k_len + pp.v_len)
        let min_leaf_size = 2
        let min_node_keys = 2
      end


      module FT = struct
        open KV
        open ST
        module Page = ST.Page
        type pframe =  
            Node_frame of (key list * Page.r list) |
            Leaf_frame of (key * value) list[@@deriving yojson]

        open Btree_util
        open S

        (* following assumes tags marshall to single int32 *)
        let node_tag = 0
        let leaf_tag = 1

        (* generic marshalling; format: int node_or_leaf; int number
           of entries; entries *)
        let frame_to_page' : pframe -> Page.t = Pickle.P.(
            fun p ->
              let is = Pickle.Examples.(
                  match p with
                  | Node_frame(ks,rs) -> (
                      p_int node_tag |> bind (fun () -> 
                          p_list pp.p_k ks |> bind (fun () -> 
                              p_list p_int rs)))
                  | Leaf_frame(kvs) -> (
                      p_int leaf_tag |> bind (fun () -> 
                          let p = fun (k,v) -> p_pair (pp.p_k k) (pp.p_v v) in
                          p_list p kvs))
                )
              in
              let s = is |> Pickle.P.run_w_exception "" in 
              let _ = Test.test (fun _ ->
                  let (l1,l2) = String.length s , Page.sz in
                  let b =  l1 <= l2 in
                  (if (not b) then Printf.printf "%d %d" l1 l2);
                  assert b)
              in
              s ^ (String.make (Page.sz - String.length s) (Char.chr 0))
          )

        let page_to_frame' : Page.t -> pframe = Pickle.U.(
            fun buf -> 
              let x = Pickle.Examples.(
                  u_int |> bind (fun tag -> 
                      match tag with 
                      | _ when tag = node_tag -> (
                          u_list pp.u_k |> bind (fun ks ->
                              u_list u_int |> bind (fun rs ->
                                  ret (Node_frame(ks,rs)))))
                      | _ when tag = leaf_tag -> (
                          let u = u_pair pp.u_k (fun k -> pp.u_v) in
                          u_list u |> bind (fun kvs -> 
                              ret (Leaf_frame(kvs)))))
                )
              in
              let (_,r) = x |> Pickle.U.run_w_exception buf in
              r)

        (* FIXME can remove these once code is trusted *)

        (* FIXME move test config to config *)
        let frame_to_page = fun f -> 
          let p = frame_to_page' f in
          let _ = Test.test (fun _ -> 
              let f' = page_to_frame' p in
              assert (f' = f)) 
          in
          p

        let page_to_frame = fun p -> 
          let f = page_to_frame' p in
          let _ = Test.test (fun _ -> 
              let p' = frame_to_page' f in
              assert Bytes.(to_string p = to_string p')) 
          in
          f

      end (* FT *)

    end) (* S_ *)


    let _ = (module S_ : Btree.S)

    module Btree = Btree.Make(S_)


  end) (* Make *)
