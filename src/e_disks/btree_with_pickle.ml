(* btree_with_pickle ---------------------------------------- *)

(* like btree, but with pickling rather than some other frame -> page
   mapping *)

open Prelude
open Btree_api
open Page_ref_int
open Default
open Pickle

module PE = Pickle.Examples

type page = BLK.t

(* following assumes tags marshall to single int32 *)
let node_tag = 0
let leaf_tag = 1
let tag_len = 4 (* bytes *)

(* generic marshalling; format: int node_or_leaf; int number
   of entries; entries *)
let frame_to_page':
  BLK.sz -> ('k,'v) pp -> ('k,'v) frame -> page = Pickle.P.(
    fun sz pp p ->
      let is = PE.(
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
      let s = is |> P.run_w_exception "" in 
      let _ = test (fun _ ->
          let (l1,l2) = String.length s , sz in
          let b =  l1 <= l2 in
          (if (not b) then Printf.printf "%d %d" l1 l2);
          assert b)
      in
      BLK.of_string sz s
  )

let page_to_frame' : ('k,'v) pp -> page -> ('k,'v)frame = Pickle.U.(
    fun pp buf -> 
      let x = PE.(
          u_int |> bind (fun tag -> 
              match tag with 
              | _ when tag = node_tag -> (
                  u_list pp.u_k |> bind (fun ks ->
                      u_list u_int |> bind (fun rs ->
                          ret (Frame.Node_frame(ks,rs)))))
              | _ when tag = leaf_tag -> (
                  let u = u_pair pp.u_k (fun k -> pp.u_v) in
                  u_list u |> bind (fun kvs -> 
                      ret (Frame.Leaf_frame(kvs)))))
        )
      in
      let (_,r) = x |> U.run_w_exception (BLK.to_string buf) in
      r)

module O = struct
  let tag_len = tag_len

  (* FIXME can remove these once code is trusted *)
  (* FIXME move test config to config *)
  let frame_to_page sz pp f = 
    let p = frame_to_page' sz pp f in
    let _ = test (fun _ -> 
        let f' = page_to_frame' pp p in
        assert (f' = f)) 
    in
    p

  let page_to_frame sz pp p = 
    (* sz only for testing *)
    let f = page_to_frame' pp p in
    let _ = test (fun _ -> 
        let p' = frame_to_page' sz pp f in
        assert (p = p'))
    in
    f

end
