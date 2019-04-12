(** The essential B-tree functionality: implement a map on top of a store. *)
open Tjr_monad.Mref
open Tjr_monad.Types
open Isa_btree
open Isa_export_wrapper
open Map_ops_type

(* convert store to map ---------------------------------------- *)

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
type ('r,'t) root_ops = ('r,'t) mref

module Internal = struct
  (* produce a map, with page_ref state set/get via monad_ops *)
  let _make_map_ops ~monad_ops ~pre_map_ops ~root_ops = 
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let { leaf_lookup; find; insert; delete } = pre_map_ops in
    let find ~k = 
      root_ops.get () >>= fun r ->
      find ~r ~k >>= fun (_,leaf,_) -> 
      (* page_ref_ops.set_page_ref r' >>= (fun () -> 
         NO! the r is the pointer to the leaf *)
      return (leaf_lookup k leaf)
    in
    let insert ~k ~v =
      root_ops.get () >>= fun r ->
      insert ~r ~k ~v >>= fun r' -> 
      match r' with
      | None -> return ()
      | Some r' -> root_ops.set r'
    in
    (*
    let insert_many ~k ~v ~kvs =
      root_ops.get () >>= fun r ->
      failwith "FIXME uinimplemented insert_many k v kvs r" >>= fun (r',kvs') ->
      root_ops.set r' >>= fun () ->
      return kvs'
    in
*)
    let delete ~k =
      root_ops.get () >>= fun r -> 
      delete ~r ~k >>= fun r' ->
      root_ops.set r'
    in
    (`Map_ops { find; insert; delete }),(`Insert_many ())
end
open Internal

(* type ('k,'r) node_impl = ('k,'r) Isa_btree.Isa_export_wrapper.node_impl *)

(** This defn serves to abbreviate types in what follows *)
type ('k,'v,'r) dnode_impl = (('k,'r)node_impl,('k,'v)leaf_impl) dnode

(** Make [map_ops], given a [page_ref_ops]. *)
let store_ops_to_map_ops ~(monad_ops:'t monad_ops) ~cs ~(k_cmp:'k -> 'k -> int) 
    ~(store_ops:('r,('k,'v,'r)dnode_impl,'t)store_ops) =
  let return = monad_ops.return in
  let dbg_tree_at_r = fun _ -> return () in
  let pre_map_ops = 
    (make_pre_map_ops_etc ~monad_ops ~cs ~k_cmp ~store_ops ~dbg_tree_at_r)
    |> pre_map_ops
  in
  fun ~root_ops -> 
    let ops = _make_map_ops ~monad_ops ~pre_map_ops ~root_ops in
    ops

let _ = store_ops_to_map_ops

(*
(** Make [ls_ops], given a [store_ops]. *)
let store_ops_to_ls_ops 
    ~monad_ops ~constants ~cmp ~(store_ops:('k,'v,'r,'t)store_ops)
    : ('k,'v,'r,'t) Leaf_stream_ops.leaf_stream_ops
  =
  Big_step.make_pre_map_ops ~monad_ops ~store_ops ~constants ~cmp @@ 
  fun ~pre_map_ops:_ ~ls_ops -> 
  ls_ops
*)
