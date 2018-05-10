open Tjr_monad.Monad
open Isa_btree

(** Iterate small-step map operations to provide big-step map
    operations. The important functions in this module are [find],
    [insert], [insert_many] and [delete] (together with the leaf stream
    big-step operations). *)


(* used by store_to_map *)

(* FIXME separate out wellformedness checking *)

(* iterate small-step operations till finished ---------------------- *)

(* FIXME in the following find_step etc should be wrt the generic monad  *)

let make_pre_map_ops (type k v r t) 
    ~(monad_ops:t monad_ops) ~store_ops ~constants ~cmp 

(*    ~find_step ~dest_f_finished 
    ~insert_step ~dest_i_finished
    ~im_step ~dest_im_finished
    ~delete_step ~dest_d_finished *)
  = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in

  let module Monad = struct
    type ('a,'t) mm = ('a,t) Tjr_monad.Monad.m  (* FIXME *)
    let bind ab a = monad_ops.bind a ab
    let return a = monad_ops.return a
    let dummy = ()
    let fmap f a = a |> bind (fun a -> return (f a))
  end
  in
  let module M = Isa_btree.Make(Monad) in

  let find_step = M.Small_step.find_step in
  let insert_step = M.Small_step.insert_step in
  let im_step = M.Small_step.im_step in
  let delete_step = M.Small_step.delete_step in
  let ls_step = M.Small_step.ls_step in

  let fmap f a = a >>= fun a -> return (f a) in


  let step ~small_step ~dest fs =
    let rec step fs =
      match dest fs with
      | Some x -> return x
      | None -> small_step fs >>= (fun fs -> step fs)
    in
    step fs
  in

  (* find leaf -------------------------------------------------------- *)

  let find_leaf' ~(k:k) ~(r:r) = 
    let small_step = find_step ~constants ~cmp ~store_ops in
    let dest = dest_f_finished in
    (step ~small_step ~dest) (mk_find_state k r)
  in
  let _ = find_leaf' in

  let find_leaf ~k ~r = 
    find_leaf' ~k ~r 
    |> fmap (fun ((r1:r),(k:k),(r2:r),kvs,stk) -> (r2,kvs,stk))
  in
  let _ = find_leaf in


  (* find ---------------------------------------- *)

  let find' ~(k:k) ~(r:r) = 
    find_leaf' ~k ~r
  in

  (** Find. Take a key, a pointer to the root of a B-tree, and the
      global state. Return the updated state, a reference to the leaf of
      the B-tree that possibly contains the key (or alternatively return
      an error). *)
  let find ~k ~r =
    find' ~k ~r 
    |> fmap (fun (_,_,r,kvs,_) -> (r,kvs))
  in


  (* insert ---------------------------------------- *)

  let insert' ~k ~v ~r = 
    let small_step = insert_step ~constants ~cmp ~store_ops in
    let dest = dest_i_finished in
    (step ~small_step ~dest) (mk_insert_state k v r)
  in


  (** Insert. Take a key, a value, a reference (to the B-tree root) and
      a state and return an updated state, with a new reference to the
      root of the updated B-tree. *)
  let insert ~k ~v ~r = 
    insert' ~k ~v ~r  
  in

  (* insert many ---------------------------------------- *)

  let im' ~k ~v ~kvs ~r =
    let small_step = im_step ~constants ~cmp ~store_ops in
    let dest = dest_im_finished in
    (step ~small_step ~dest) (mk_im_state k v kvs r)
  in

  (** Insert many. Take a key, value, and list of further key/values, a
      pointer to the B-tree root and a global state. Return the updated
      state, a new pointer to a B-tree root, and a list of key/values
      which could not be inserted into the same leaf as the first
      key/value. Typically this function is called in a loop till all kvs
      have been inserted. It is assumed faster than multiple inserts,
      although caching may invalidate this assumption. *)
  let insert_many ~(k:k) ~(v:v) ~kvs ~(r:r) = im' ~k ~v ~kvs ~r in
  let _ = insert_many in


  (* delete ---------------------------------------- *)

  let delete' ~(k:k) ~(r:r) = 
    let small_step = delete_step ~constants ~cmp ~store_ops in
    let dest = dest_d_finished in
    (step ~small_step ~dest) (mk_delete_state k r)
  in


  (** Delete. Take a key and a reference to a B-tree root, and a global
      state. Return the updated state and a reference to the new root. *)
  let delete ~(k:k) ~(r:r) = 
    delete' ~k ~r 
  in



  (* leaf stream ---------------------------------------------------- *)

  let ils_mk = 
    Iter_leaf_stream.ils_mk ~monad_ops ~constants ~cmp ~store_ops ~ls_step in

  (* make pre_map_ops ---------------------------------------- *)

  (** Construct [pre_map_ops] using functions above. NOTE also
      produces a leaf stream. *)
  let pre_map_ops  = 
    let find_leaf=(fun (k:k) (r:r) -> find_leaf ~k ~r) in
    let find=(fun k r -> find ~k ~r) in
    let insert=(fun k (v:v) r -> insert ~k ~v ~r) in
    let insert_many=(fun k v kvs r -> insert_many ~k ~v ~kvs ~r) in
    let delete=(fun k r -> delete ~k ~r) in
    let pre_map_ops = Pre_map_ops.mk_pre_map_ops ~find_leaf ~find ~insert ~insert_many ~delete in
    pre_map_ops
  in

  fun f -> f ~pre_map_ops ~ils_mk

(*

  let open Store_ops in


Pre_map_ops.mk_pre_map_ops ~find_leaf ~find ~insert ~insert_many ~delete

let _ = store_ops_to_pre_map_ops
*)

(** The main functionality exported by the B-tree code: implement a
    map on top of a store *)
(* NOTE we phrase it in this form so that we avoid types like
   store_ops that are dependent on the monad *)
(*
let store_ops_to_pre_map_ops ~ps ~store_free ~store_read ~store_alloc = 
  mk_store_ops ~store_free ~store_read ~store_alloc |> fun store_ops ->
  Big_step.store_ops_to_pre_map_ops ~ps ~store_ops |> fun map_ops ->
  fun f -> 
    f 
      ~find_leaf:map_ops.find_leaf ~find:map_ops.find ~insert:map_ops.insert 
      ~insert_many:map_ops.insert_many ~delete:map_ops.delete

*)
