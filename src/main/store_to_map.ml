(** The essential B-tree functionality: implement a map on top of a store. *)
(* open Tjr_monad.Mref *)
(* open Tjr_monad.Types *)
(* open Isa_btree *)
(* open Isa_export_wrapper *)
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
type ('r,'t) btree_root_ops = ('r,'t) with_state
(* for all operations, we need to be able to retrieve the root; *)

module Internal = struct
  (* produce a map, with page_ref state set/get via monad_ops *)
  let _make_map_ops ~monad_ops ~pre_map_ops ~pre_insert_many_op ~leaf_stream_ops ~root_ops = 
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let { leaf_lookup; find; insert; delete } = pre_map_ops in
    let Isa_btree.{ insert_many } = pre_insert_many_op in
    let Isa_btree.{ make_leaf_stream; ls_step; ls_leaf } = leaf_stream_ops in
    let { with_state } = root_ops in
    let find ~k = 
      with_state (fun ~state:r ~set_state:_ -> 
          find ~r ~k >>= fun (_,leaf,_) -> 
          (* page_ref_ops.set_page_ref r' >>= (fun () -> 
             NO! the r is the pointer to the leaf *)
          return (leaf_lookup k leaf))
    in
    let insert ~k ~v =
      with_state (fun ~state:r ~set_state -> 
          insert ~r ~k ~v >>= fun r' -> 
          match r' with
          | None -> return ()
          | Some r' -> set_state r')
    in
    let insert_many ~kvs =
      with_state (fun ~state:r ~set_state -> 
        insert_many ~r ~kvs >>= fun (kvs,ropt) -> 
        match ropt with
        | None -> return kvs
        | Some r -> set_state r >>= fun () -> return kvs)
    in
    let iter_m = iter_m ~monad_ops in
    let insert_many ~kvs = 
      kvs |> iter_m (fun kvs -> insert_many ~kvs >>= function
      | [] -> return None
      | kvs -> return (Some kvs)) >>= function
      | [] -> return ()
      | _ -> failwith "impossible"
    in
    let delete ~k =
      with_state (fun ~state:r ~set_state -> 
          delete ~r ~k >>= fun r' ->
          set_state r')
    in
    (`Map_ops { find; insert; delete }),(`Insert_many insert_many),(`Leaf_stream_ops leaf_stream_ops)
end
open Internal

(* type ('k,'r) node_impl = ('k,'r) Isa_btree.Isa_export_wrapper.node_impl *)

(** This defn serves to abbreviate types in what follows *)
(* type ('k,'v,'r) dnode_impl = (('k,'r)node_impl,('k,'v)leaf_impl) dnode *)

(** Make [map_ops], given a [page_ref_ops]. *)
let store_ops_to_map_ops ~(monad_ops:'t monad_ops) ~cs ~(k_cmp:'k -> 'k -> int) 
    ~(store_ops:('r,('k,'v,'r)dnode_impl,'t)store_ops) =
  let return = monad_ops.return in
  let dbg_tree_at_r = fun _ -> return () in
  let isa_btree = make_isa_btree ~monad_ops ~cs ~k_cmp ~store_ops ~dbg_tree_at_r in
  let pre_map_ops = isa_btree |> pre_map_ops in
  let pre_insert_many_op = isa_btree |> pre_insert_many_op in
  let leaf_stream_ops = isa_btree |> leaf_stream_ops in
  fun ~root_ops -> 
    let ops = _make_map_ops ~monad_ops ~pre_map_ops ~pre_insert_many_op ~leaf_stream_ops ~root_ops in
    ops

let _ = store_ops_to_map_ops

(** Prettier type: {%html:<pre>
monad_ops:'t monad_ops ->
cs:constants ->
k_cmp:('k -> 'k -> int) ->
store_ops:('r, ('k, 'v, 'r) dnode_impl, 't) store_ops ->
root_ops:('r, 't) btree_root_ops ->
[> `Map_ops of ('k, 'v, 't) map_ops ] *
[> `Insert_many of kvs:('k * 'v) list -> (unit, 't) m ] *
[> `Leaf_stream_ops of
     ('r, ('k, 'v) leaf_impl, ('k, 'v, 'r) leaf_stream_impl, 't)
     leaf_stream_ops ]
</pre> %}
*)

