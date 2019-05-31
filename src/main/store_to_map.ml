(** The essential B-tree functionality: implement a map on top of a store. *)

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
  (* let _make_map_ops ~monad_ops ~pre_map_ops ~pre_insert_many_op ~leaf_stream_ops ~root_ops =  *)
  let _make_map_ops ~monad_ops ~isa_btree ~root_ops =
    let xx = isa_btree in
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let { leaf_lookup; find; insert; delete } = pre_map_ops xx in
    let Isa_btree.{ insert_many } = insert_many xx in
    let Isa_btree.{ insert_all } = insert_all xx in
    let Isa_btree.{ make_leaf_stream; ls_step; ls_kvs } = leaf_stream_ops xx in
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
    let delete ~k =
      with_state (fun ~state:r ~set_state -> 
          delete ~r ~k >>= fun r' ->
          set_state r')
    in
    let insert_many ~k ~v ~kvs =
      with_state (fun ~state:r ~set_state -> 
        insert_many ~r ~k ~v ~kvs >>= fun (kvs,ropt) -> 
        (match ropt with
        | None -> return ()
        | Some r -> set_state r) >>= fun () -> 
        return kvs)
    in
    let insert_all ~kvs = 
      with_state (fun ~state:r ~set_state -> 
        insert_all ~r ~kvs >>= fun r -> 
        set_state r)
    in
    let leaf_stream_ops = { make_leaf_stream; ls_step; ls_kvs } in
    { find; insert; delete },{ insert_many; insert_all; leaf_stream_ops }

  let _ : 
monad_ops:'a monad_ops ->
isa_btree:('b, 'c, 'd, 'a) isa_btree ->
root_ops:('d, 'a) btree_root_ops ->
('b, 'c, 'a) map_ops *
(k:'b -> v:'c -> kvs:('b * 'c) list -> (('b * 'c) list, 'a) m,
 kvs:('b * 'c) list -> (unit, 'a) m,
 ('b, 'c, 'd, ('b, 'c, 'd) leaf_stream_impl, 'a) leaf_stream_ops)
extra_map_ops
 = _make_map_ops
end
open Internal

(** Make [map_ops], given a [page_ref_ops]. *)
let store_ops_to_map_ops ~(monad_ops:'t monad_ops) ~cs ~(k_cmp:'k -> 'k -> int) 
    ~(store_ops:('r,('k,'v,'r)dnode_impl,'t)store_ops) =
  let return = monad_ops.return in
  let dbg_tree_at_r = fun _ -> return () in
  let isa_btree = make_isa_btree ~monad_ops ~cs ~k_cmp ~store_ops ~dbg_tree_at_r in
  fun ~root_ops -> 
    let ops = _make_map_ops ~monad_ops ~isa_btree ~root_ops in
    ops

let _ : 
monad_ops:'t monad_ops ->
cs:constants ->
k_cmp:('k -> 'k -> int) ->
store_ops:('r, ('k, 'v, 'r) dnode_impl, 't) store_ops ->
root_ops:('r, 't) btree_root_ops ->
('k, 'v, 't) map_ops *
(k:'k -> v:'v -> kvs:('k * 'v) list -> (('k * 'v) list, 't) m,
 kvs:('k * 'v) list -> (unit, 't) m,
 ('k, 'v, 'r, ('k, 'v, 'r) leaf_stream_impl, 't) leaf_stream_ops)
extra_map_ops
= 
store_ops_to_map_ops

(** Prettier type: {%html:<pre>
monad_ops:'t monad_ops ->
cs:constants ->
k_cmp:('k -> 'k -> int) ->
store_ops:('r, ('k, 'v, 'r) dnode_impl, 't) store_ops ->
root_ops:('r, 't) btree_root_ops ->
('k, 'v, 't) map_ops *
(k:'k -> v:'v -> kvs:('k * 'v) list -> (('k * 'v) list, 't) m,
 kvs:('k * 'v) list -> (unit, 't) m,
 ('k, 'v, 'r, ('k, 'v, 'r) leaf_stream_impl, 't) leaf_stream_ops)
extra_map_ops
</pre> %}
*)

