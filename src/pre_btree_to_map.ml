(** Convert a pre-btree (with explicit root passing) to a real map. *)

open Btree_intf

(* produce a map, with page_ref state set/get via monad_ops *)
let pre_btree_to_map ~monad_ops 
    ~(pre_btree_ops:('k,'v,'r,'t,'leaf,'node,'leaf_stream)pre_btree_ops)
    ~(root_ops:('r,'t) btree_root_ops) 
  =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let Isa_btree_intf.{ find; insert; delete; insert_many; insert_all; leaf_ops; leaf_stream_ops; _ } = pre_btree_ops in
  let Isa_btree_intf.Insert_many_type.{ insert_many } = insert_many in
  let Isa_btree_intf.Insert_all_type.{ insert_all } = insert_all in
  let { with_state } = root_ops in
  let find ~k = 
    with_state (fun ~state:r ~set_state:_ -> 
        let r = r.btree_root in
        find ~r ~k >>= fun (_,leaf,_) -> 
        (* page_ref_ops.set_page_ref r' >>= (fun () -> 
           NO! the r is the pointer to the leaf *)
        return (leaf_ops.leaf_lookup k leaf))
  in
  let insert ~k ~v =
    with_state (fun ~state:r ~set_state -> 
        let r = r.btree_root in
        insert ~r ~k ~v >>= fun r' -> 
        match r' with
        | None -> return ()
        | Some r' -> set_state {btree_root=r'})
  in
  let delete ~k =
    with_state (fun ~state:r ~set_state -> 
        let r = r.btree_root in
        delete ~r ~k >>= fun r' ->
        set_state {btree_root=r'})
  in
  let insert_many ~k ~v ~kvs =
    with_state (fun ~state:r ~set_state -> 
        let r = r.btree_root in
        insert_many ~r ~k ~v ~kvs >>= fun (kvs,ropt) -> 
        (match ropt with
         | None -> return ()
         | Some r -> set_state {btree_root=r}) >>= fun () -> 
        return kvs)
  in
  let insert_all ~kvs = 
    with_state (fun ~state:r ~set_state -> 
        let r = r.btree_root in
        insert_all ~r ~kvs >>= fun r -> 
        set_state {btree_root=r})
  in
  Map_ops_with_ls.{ find; insert; delete; insert_many; insert_all; leaf_stream_ops }

let _ : 
monad_ops:'a monad_ops ->
pre_btree_ops:('b, 'c, 'd, 'a, 'e, 'f, 'g) pre_btree_ops ->
root_ops:('d, 'a) btree_root_ops ->
('b, 'c, 'd, 'g, 'a) Map_ops_with_ls.map_ops_with_ls
  = pre_btree_to_map
