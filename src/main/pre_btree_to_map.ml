(** Convert a pre-btree (with explicit root passing) to a real map. *)

open Btree_intf

(* convert store to map ---------------------------------------- *)

(* produce a map, with page_ref state set/get via monad_ops *)
let pre_btree_to_map ~monad_ops ~pre_btree_ops ~root_ops =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let Isa_btree_intf.{ find; insert; delete; insert_many; insert_all; leaf_ops; leaf_stream_ops; _ } = pre_btree_ops in
  let Isa_btree_intf.Insert_many_type.{ insert_many } = insert_many in
  let Isa_btree_intf.Insert_all_type.{ insert_all } = insert_all in
  let { with_state } = root_ops.root_ops in
  let find ~k = 
    with_state (fun ~state:r ~set_state:_ -> 
        find ~r ~k >>= fun (_,leaf,_) -> 
        (* page_ref_ops.set_page_ref r' >>= (fun () -> 
           NO! the r is the pointer to the leaf *)
        return (leaf_ops.leaf_lookup k leaf))
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
  Map_ops_etc_type.{ find; insert; delete; insert_many; insert_all; leaf_stream_ops }

let _ : 
monad_ops:'a monad_ops ->
pre_btree_ops:('b, 'c, 'd, 'a, 'e, 'f, 'g) pre_btree_ops ->
root_ops:('d, 'a) btree_root_ops ->
('b, 'c, 'd, 'g, 'a) Map_ops_etc_type.map_ops_etc
  = pre_btree_to_map

(*
(** Make [map_ops], given a [page_ref_ops]. The question is what interface to provide for store_ops to access node and leaf. *)
module Make_store_ops_to_map_ops(S:S) = struct
  open S

  module X = Isa_btree.Make(S)
  open X

  let store_ops_to_map_ops (type k v r) ~(monad_ops:'t monad_ops) ~cs ~store_ops = 
    let pre_btree_ops = X.make_btree_ops ~monad_ops ~cs ~store_ops in
    fun ~root_ops -> 
      let map_ops = Internal.make_map_ops ~monad_ops ~pre_btree_ops ~root_ops in
      map_ops

  let _ :
    monad_ops:'t monad_ops ->
cs:constants ->
store_ops:(r, (node, leaf) dnode, 't) store_ops ->
root_ops:(r, 't) with_state ->
(k, v, 't) map_ops *
(k:k -> v:v -> kvs:(k * v) list -> ((k * v) list, 't) m,
 kvs:(k * v) list -> (unit, 't) m,
 (k, v, r, leaf_stream, 't) leaf_stream_ops)
extra_map_ops
    = store_ops_to_map_ops
    
end
*)

(* FIXME removed because we can probably just use pre_btree_to_map_ops
let store_to_map ~(monad_ops:'t monad_ops) ~cs ~k_args ~(store_ops:('r,('node,'leaf)dnode,'t)store_ops) =
  let open Make_with_kargs in
  let pre_btree_ops = make_with_kargs ~monad_ops ~cs ~k_args ~store_ops in
  fun ~root_ops -> 
    let ops = make_map_ops ~monad_ops ~pre_btree_ops ~root_ops in
    ops

let _ : 
monad_ops:'t monad_ops ->
cs:Isa_btree__Constants_type.constants ->
k_args:('a -> 'a -> int, ('a, 'b, 'leaf) Tjr_map.With_base_as_record.map_ops,
        ('a option, 'r, 'node) Tjr_map.With_base_as_record.map_ops)
       Make_with_kargs.k_args ->
store_ops:('r, ('node, 'leaf) dnode, 't) store_ops ->
root_ops:('r, 't) btree_root_ops ->
('a, 'b, 't) Map_ops_etc_type.map_ops_etc *
('a, 'b, 'r,
 ('r, 'leaf, ('a, 'r, 'node) Isa_btree_intf.Frame_type.frame)
 Isa_btree__Isa_export_wrapper.Internal_leaf_stream_impl._t, 't)
leaf_stream_ops
= 
store_to_map


(** Prettier type: {%html:<pre>
monad_ops:'t monad_ops ->
cs:Isa_btree__Constants_type.constants ->
k_args:('a -> 'a -> int, ('a, 'b, 'leaf) Tjr_map.With_base_as_record.map_ops,
        ('a option, 'r, 'node) Tjr_map.With_base_as_record.map_ops)
       Make_with_kargs.k_args ->
store_ops:('r, ('node, 'leaf) dnode, 't) store_ops ->
root_ops:('r, 't) btree_root_ops ->
('a, 'b, 't) Map_ops_etc_type.map_ops_etc *
('a, 'b, 'r,
 'xxx, 't)
leaf_stream_ops


</pre> %}
*)



(*
module type Leaf_stream_ops = sig
  type k 
  type v
  type r
  type t

  type leaf_stream_state

  val leaf_stream_ops: (k, v, r, leaf_stream_state, t) leaf_stream_ops
end
*)

*)
