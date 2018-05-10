(** The essential B-tree functionality: implement a map on top of a store. *)


(* convert store to map ---------------------------------------- *)

(* instead of btree_make, use records; we want to get versions which
   use records rather than functors *)

open Base_types
open Page_ref_int  (* TODO generalize? *)

(** The B-tree code exports a [pre_map_ops] version of a map, with
   explicit passing of the reference to the root of the B-tree. In
   order to implement the [map_ops] interface, we need to store the
   "current" reference to the B-tree root in the global state
   somehow. A value of type ['t page_ref_ops] reveals how to read and
   write this reference in the global state. *)
type 't page_ref_ops = (page_ref,'t) mref

open Pre_map_ops
open Map_ops

(* produce a map, with page_ref state set/get via monad_ops *)
let make_map_ops' (type k v r t) ~monad_ops ~pre_map_ops ~page_ref_ops = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  dest_pre_map_ops pre_map_ops @@ 
  fun ~find_leaf ~find ~insert ~insert_many ~delete -> 
  let find_leaf = fun k ->
    page_ref_ops.get () >>= fun r ->
    find_leaf k r >>= fun kvs ->               
    return kvs
  in
  let find = fun k ->
    page_ref_ops.get () >>= fun r ->
    find k r >>= fun (r',kvs) -> 
    (* page_ref_ops.set_page_ref r' >>= (fun () -> 
       NO! the r is the pointer to the leaf *)
    return (try Some(List.assoc k kvs) with _ -> None)
  in
  let insert = fun k v ->
    page_ref_ops.get () >>= fun r ->
    insert k v r >>= fun r' -> 
    page_ref_ops.set r'
  in
  let insert_many = fun k v kvs -> 
    page_ref_ops.get () >>= fun r ->
    insert_many k v kvs r >>= fun (r',kvs') ->
    page_ref_ops.set r' >>= fun () ->
    return kvs'
  in
  let delete = fun k ->
    page_ref_ops.get () >>= fun r -> 
    delete k r >>= fun r' ->
    page_ref_ops.set r'
  in
  assert(wf_map_ops ~find ~insert ~delete ~insert_many);
  mk_map_ops ~find ~insert ~delete ~insert_many


(** Make [map_ops], given a [page_ref_ops]. *)
let store_ops_to_map_ops 
    ~monad_ops ~constants ~cmp ~page_ref_ops ~store_ops 
  : ('k,'v,'t) Map_ops.map_ops
  =
  Big_step.make_pre_map_ops ~monad_ops ~store_ops ~constants ~cmp @@ 
  fun ~pre_map_ops ~ils_mk -> 
  make_map_ops' ~monad_ops ~pre_map_ops ~page_ref_ops

(*

let ils_mk = Iter_leaf_stream.ils_mk

(** Make [ls_ops], given a [page_ref_ops]. We only read from
    page_ref_ops. TODO ditto *)
let make_ls_ops ~constants ~cmp ~monad_ops ~store_ops ~page_ref_ops ~ls_step : ('k,'v,'r,'t) Leaf_stream_ops.leaf_stream_ops =
  ils_mk ~constants ~cmp ~monad_ops ~store_ops ~ls_step @@ fun ~mk_leaf_stream ~ls_kvs ~ls_step ->
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let mk_leaf_stream = fun () ->
    page_ref_ops.get () >>= fun r -> 
    mk_leaf_stream r
  in
  Leaf_stream_ops.mk_ls_ops ~mk_leaf_stream ~ls_step ~ls_kvs

let _ = make_ls_ops
*)
