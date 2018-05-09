(** Combine the isabelle defns with the monad *)

open Tjr_monad
open Isa_btree

module type MONAD_OPS = sig
  type t val monad_ops:t Monad.monad_ops end



module Monad'(S:MONAD_OPS) = struct
  open S

  type ('a,'t) mm = ('a,t) Monad.m  
  (* FIXME this is a bit funny, because the 't is not really playing a role on the isa side *)

  let return x : ('a,t) mm = monad_ops.return x
  let bind (ab: 'a -> ('b,t) mm) (a:('a,t)mm) : ('b,t) mm = monad_ops.bind a ab
  let dummy = ()
  let fmap f a = bind (fun a -> return (f a)) a
end



module Big_step(S:MONAD_OPS) = struct

  module Monad = Monad'(S)
  open Monad
  include Isa_btree.Make(Monad)

  (** The order on keys. B-trees work with ordered keys. *)
  let cmp x : 'k -> 'k -> int = (x#cmp)

  (** Constants. See {!Constants} *)
  let constants x : constants = (x#constants)


  (** Iterate small-step map operations to provide big-step map
      operations. The important functions in this module are [find],
      [insert], [insert_many] and [delete] (together with the leaf stream
      big-step operations). *)


  (* used by store_to_map *)

  (* FIXME separate out wellformedness checking *)

  (* iterate small-step operations till finished ---------------------- *)

  module X = Small_step

  let step ~small_step ~dest fs =
    let rec step fs =
      match dest fs with
      | Some x -> return x
      | None -> small_step fs |> bind (fun fs -> step fs)
    in
    step fs


  (* find leaf -------------------------------------------------------- *)

  let find_leaf' (type k v r t) ~cmp ~constants ~store_ops ~(k:k) ~(r:r) = 
    let small_step = X.find_step ~constants ~cmp ~store_ops in
    let dest = X.dest_f_finished in
    (step ~small_step ~dest) (X.mk_find_state k r)

  let _ = find_leaf'

  (* open Params *)

  let find_leaf ~ps ~store_ops ~k ~r = 
    find_leaf' ~cmp:(cmp ps) ~constants:(constants ps) ~store_ops ~k ~r 
    |> fmap (fun ((r1:'r),(k:'k),(r2:'r),kvs,stk) -> (r2,kvs,stk))

  let _ = find_leaf


  (* find ---------------------------------------- *)

  let find' ~cmp ~constants ~store_ops ~(k:'k) ~(r:'r) = 
    find_leaf' ~cmp ~constants ~store_ops ~k ~r

  (** Find. Take a key, a pointer to the root of a B-tree, and the
      global state. Return the updated state, a reference to the leaf of
      the B-tree that possibly contains the key (or alternatively return
      an error). *)
  let find ~ps ~store_ops ~k ~r =
    find' ~cmp:(cmp ps) ~constants:(constants ps) ~store_ops ~k ~r 
    |> fmap (fun (_,_,r,kvs,_) -> (r,kvs))



  (* insert ---------------------------------------- *)

  let insert' ~cmp ~constants ~store_ops ~k ~v ~r = 
    let small_step = X.insert_step ~constants ~cmp ~store_ops in
    let dest = X.dest_i_finished in
    (step ~small_step ~dest) (X.mk_insert_state k v r)


  (** Insert. Take a key, a value, a reference (to the B-tree root) and
      a state and return an updated state, with a new reference to the
      root of the updated B-tree. *)
  let insert ~ps ~store_ops ~k ~v ~r = 
    insert' ~cmp:(cmp ps) ~constants:(constants ps) ~store_ops ~k ~v ~r  


  (* insert many ---------------------------------------- *)

  let im' ~cmp ~constants ~store_ops ~k ~v ~kvs ~r =
    let small_step = X.im_step ~constants ~cmp ~store_ops in
    let dest = X.dest_im_finished in
    (step ~small_step ~dest) (X.mk_im_state k v kvs r)


  (** Insert many. Take a key, value, and list of further key/values, a
      pointer to the B-tree root and a global state. Return the updated
      state, a new pointer to a B-tree root, and a list of key/values
      which could not be inserted into the same leaf as the first
      key/value. Typically this function is called in a loop till all kvs
      have been inserted. It is assumed faster than multiple inserts,
      although caching may invalidate this assumption. *)
  let insert_many ~ps ~store_ops ~(k:'k) ~(v:'v) ~kvs ~(r:'r) = 
    im' ~cmp:(cmp ps) ~constants:(constants ps) ~store_ops ~k ~v ~kvs ~r 

  let _ = insert_many


  (* delete ---------------------------------------- *)

  let delete' ~cmp ~constants ~store_ops ~(k:'k) ~(r:'r) = 
    let small_step = X.delete_step ~constants ~cmp ~store_ops in
    let dest = X.dest_d_finished in
    (step ~small_step ~dest) (X.mk_delete_state k r)


  (** Delete. Take a key and a reference to a B-tree root, and a global
      state. Return the updated state and a reference to the new root. *)
  let delete ~ps ~store_ops ~(k:'k) ~(r:'r) = 
    delete' ~cmp:(cmp ps) ~constants:(constants ps) ~store_ops ~k ~r 

  let _ = delete


(*

    (* make pre_map_ops ---------------------------------------- *)

    open Store_ops

    (** Construct [pre_map_ops] using functions above. Takes a "parameters" object ps. *)
    let store_ops_to_pre_map_ops ~ps ~(store_ops:('k,'v,'r,'t)store_ops) = 
      dest_store_ops store_ops @@ 
      fun ~store_free ~store_read ~store_alloc ->
      let find_leaf=(fun (k:'k) (r:'r) -> find_leaf ~ps ~store_ops ~k ~r) in
      let find=(fun k r -> find ~ps ~store_ops ~k ~r) in
      let insert=(fun k (v:'v) r -> insert ~ps ~store_ops ~k ~v ~r) in
      let insert_many=(fun k v kvs r -> insert_many ~ps ~store_ops ~k ~v ~kvs ~r) in
      let delete=(fun k r -> delete ~ps ~store_ops ~k ~r) in
      Pre_map_ops.mk_pre_map_ops ~find_leaf ~find ~insert ~insert_many ~delete

    let _ = store_ops_to_pre_map_ops

  end

  (** The main functionality exported by the B-tree code: implement a
      map on top of a store *)
  (* NOTE we phrase it in this form so that we avoid types like
     store_ops that are dependent on the monad *)
  let store_ops_to_pre_map_ops ~ps ~store_free ~store_read ~store_alloc = 
    mk_store_ops ~store_free ~store_read ~store_alloc |> fun store_ops ->
    Big_step.store_ops_to_pre_map_ops ~ps ~store_ops |> fun map_ops ->
    fun f -> 
      f 
        ~find_leaf:map_ops.find_leaf ~find:map_ops.find ~insert:map_ops.insert 
        ~insert_many:map_ops.insert_many ~delete:map_ops.delete
*)
end


let make (type t) ~(monad_ops: t Monad.monad_ops) = 
  let module Monad_ops = 
  struct 
    type nonrec t = t
    let monad_ops = monad_ops
  end
  in
  let module M = Big_step(Monad_ops) in
  fun f -> 
    let open M in
    f ~find_leaf ~find ~insert ~insert_many ~delete

let _ = make
