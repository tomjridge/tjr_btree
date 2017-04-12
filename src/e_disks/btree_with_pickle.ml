(* btree_with_pickle ---------------------------------------- *)

(* given a block-like store, we want to produce a Btree_make.Poly.t *)

(* this is really somehow altering a set of operations so that they
   use a (k,v) frame type rather than a raw bytes type *)


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

type ('k,'v) pp = ('k,'v) Pickle_params.t

module Page = struct
  type t = string
  type r = page_ref
  type sz = int  (* in bytes *)
end

type ('k,'v) frame = ('k,'v,page_ref) Frame.t

(* following assumes tags marshall to single int32 *)
let node_tag = 0
let leaf_tag = 1
let tag_len = 4 (* bytes *)

open Frame



(* generic marshalling; format: int node_or_leaf; int number
   of entries; entries *)
let frame_to_page':
  Page.sz -> ('k,'v) pp -> ('k,'v) frame -> Page.t = Pickle.P.(
    fun sz pp p ->
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
          let (l1,l2) = String.length s , sz in
          let b =  l1 <= l2 in
          (if (not b) then Printf.printf "%d %d" l1 l2);
          assert b)
      in
      s ^ (String.make (sz - String.length s) (Char.chr 0))
  )

let page_to_frame' : ('k,'v) pp -> Page.t -> ('k,'v)frame = Pickle.U.(
    fun pp buf -> 
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
let frame_to_page sz pp f = 
  let p = frame_to_page' sz pp f in
  let _ = Test.test (fun _ -> 
      let f' = page_to_frame' pp p in
      assert (f' = f)) 
  in
  p

let page_to_frame sz pp p = 
  (* sz only for testing *)
  let f = page_to_frame' pp p in
  let _ = Test.test (fun _ -> 
      let p' = frame_to_page' sz pp f in
      assert Bytes.(to_string p = to_string p'))
  in
  f
