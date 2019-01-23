(** The essential B-tree functionality: implement a map on top of a store. *)


(* convert store to map ---------------------------------------- *)

open Base_types
open Page_ref_int  (* TODO generalize? *)
open Store_ops

(** The B-tree code exports a [pre_map_ops] version of a map, with
   explicit passing of the reference to the root of the B-tree. In
   order to implement the [map_ops] interface, we need to store the
   "current" reference to the B-tree root in the global state
   somehow. A value of type ['t page_ref_ops] reveals how to read and
   write this reference in the global state.  

NOTE because page_ref_ops get and set are not atomic, this interface is
not concurrent safe.

FIXME move this type elsewhere?
*)
(* for all operations, we need to be able to retrieve the root; *)
type 't btree_root_ops = (page_ref,'t) mref

open Pre_map_ops_type
open Map_ops



module Internal = struct
  (* produce a map, with page_ref state set/get via monad_ops *)
  let make_map_ops' (* (type k v r t)*) ~monad_ops ~pre_map_ops ~page_ref_ops:root_ops = 
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let { find_leaf; find; insert; insert_many; delete } = pre_map_ops in
    let _find_leaf = fun k ->
      root_ops.get () >>= fun r ->
      find_leaf k r >>= fun kvs ->               
      return kvs
    in
    let find = fun k ->
      root_ops.get () >>= fun r ->
      find k r >>= fun (_r',kvs) -> 
      (* page_ref_ops.set_page_ref r' >>= (fun () -> 
         NO! the r is the pointer to the leaf *)
      return (try Some(List.assoc k kvs) with _ -> None)
    in
    let insert = fun k v ->
      root_ops.get () >>= fun r ->
      insert k v r >>= fun r' -> 
      root_ops.set r'
    in
    let insert_many = fun k v kvs -> 
      root_ops.get () >>= fun r ->
      insert_many k v kvs r >>= fun (r',kvs') ->
      root_ops.set r' >>= fun () ->
      return kvs'
    in
    let delete = fun k ->
      root_ops.get () >>= fun r -> 
      delete k r >>= fun r' ->
      root_ops.set r'
    in
    (* assert(wf_map_ops ~find ~insert ~delete ~insert_many); *)
    { find; insert; delete; insert_many }
end
open Internal


(** Make [map_ops], given a [page_ref_ops]. *)
let store_ops_to_map_ops 
    ~monad_ops ~constants ~cmp ~page_ref_ops ~(store_ops:('k,'v,'r,'t)store_ops)
  : ('k,'v,'t) Map_ops.map_ops
  =
  Big_step.make_pre_map_ops ~monad_ops ~store_ops ~constants ~cmp @@ 
  fun ~pre_map_ops ~ls_ops:_ -> 
  let map_ops = make_map_ops' ~monad_ops ~pre_map_ops ~page_ref_ops in
  map_ops


let _ = store_ops_to_map_ops

(** Make [ls_ops], given a [store_ops]. *)
let store_ops_to_ls_ops 
    ~monad_ops ~constants ~cmp ~(store_ops:('k,'v,'r,'t)store_ops)
    : ('k,'v,'r,'t) Leaf_stream_ops.leaf_stream_ops
  =
  Big_step.make_pre_map_ops ~monad_ops ~store_ops ~constants ~cmp @@ 
  fun ~pre_map_ops:_ ~ls_ops -> 
  ls_ops

