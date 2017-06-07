(** Convert frames to blocks using pickling *)

(* like btree, but with pickling rather than some other frame -> page
   mapping *)

open Prelude
open Btree_api
open Page_ref_int
open Pickle
open Pickle_params
open Block

module PE = Pickle.Examples

type page = blk

(* following assumes tags marshall to single int32 *)
let node_tag = 0
let leaf_tag = 1
let tag_len = 4 (* bytes *)

module X = Profile

(* generic marshalling; format: int node_or_leaf; int number
   of entries; entries *)
let frame_to_page':
  blk_sz -> ('k,'v) pp -> ('k,'v) frame -> page = Pickle.P.(
    fun blk_sz pp p ->
      assert(X.log X.P.cd);
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
          let (l1,l2) = String.length s , blk_sz in
          let b =  l1 <= l2 in
          (if (not b) then Printf.printf "%d %d" l1 l2);
          assert b)
      in
      assert(X.log X.P.de);
      BlkN.of_string blk_sz s
  )

let page_to_frame' : ('k,'v) pp -> page -> ('k,'v)frame = Pickle.U.(
    fun pp buf -> 
      assert(X.log X.P.ef);      
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
      let (_,r) = x |> U.run_w_exception (BlkN.to_string buf) in
      (* basic size checks *)
      test(fun _ -> (match r with
      | Leaf_frame ks -> (* could be root *) ()
      | Node_frame (ks,rs) -> assert(List.length ks +1 = List.length rs)));
      assert(X.log X.P.fg);      
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
